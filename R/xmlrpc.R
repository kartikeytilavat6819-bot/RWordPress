# A small, self-contained XML-RPC client (http://xmlrpc.com/spec.md).
#
# This replaces the dependency on the XMLRPC package (which was never on CRAN and
# relied on RCurl and XML) with the maintained curl and xml2 packages.  It keeps
# the calling convention of XMLRPC::xml.rpc() so code that passed .opts, .curl or
# .convert through the RWordPress functions' ... continues to work.

xml.rpc =
function(url, method, ..., .args = list(...),
         .opts = list(), .convert = TRUE, .curl = curl::new_handle(),
         useragent = "R-RWordPress")
{
  body = as.character(createBody(method, .args))

  h = .curl
  curl::handle_setopt(h, post = TRUE, postfields = body, followlocation = TRUE,
                      useragent = useragent)
  curl::handle_setheaders(h, "Content-Type" = "text/xml; charset=utf-8")
  if(length(.opts)) {
       # RCurl-style names (ssl.verifypeer) -> curl-style (ssl_verifypeer)
    names(.opts) = gsub(".", "_", names(.opts), fixed = TRUE)
    curl::handle_setopt(h, .list = .opts)
  }

  res = curl::curl_fetch_memory(url, handle = h)
  txt = rawToChar(res$content)
  Encoding(txt) = "UTF-8"

  if(res$status_code %/% 100 != 2)
    stop(sprintf("HTTP error %d from %s calling %s: %s", res$status_code, url, method,
                 substr(trimws(gsub("<[^>]+>", " ", txt)), 1, 200)),
         call. = FALSE)

  if(isTRUE(.convert))
    convertToR(txt)
  else if(is.function(.convert))
    .convert(txt)
  else
    txt
}

createBody =
function(method, args)
{
  doc = xml2::xml_new_root("methodCall")
  xml2::xml_add_child(doc, "methodName", method)
  params = xml2::xml_add_child(doc, "params")
  for(a in args)
    addValue(xml2::xml_add_child(params, "param"), a)
  doc
}


###########
# R -> XML-RPC

addValue =
  # Append a <value> node describing x to parent.
function(parent, x)
{
  v = xml2::xml_add_child(parent, "value")

  if(is.null(x))
    return(addArray(v, list()))

  if(inherits(x, "POSIXlt"))
    x = as.POSIXct(x)

  if(isS4(x) && !is.atomic(x) && !is.list(x)) {
    els = methods::slotNames(x)
    x = structure(lapply(els, function(id) methods::slot(x, id)), names = els)
  }

  if(inherits(x, "AsIs")) {
      # I(x) always becomes an array, even when it has length 1.
    x = unclass(x)
    return(addArray(v, if(is.list(x)) x else as.list(x), asIs = TRUE))
  }

  if(is.list(x)) {
    if(!is.null(names(x)))
      addStruct(v, x)
    else
      addArray(v, x)
    return(invisible(v))
  }

  if(is.raw(x)) {
    xml2::xml_add_child(v, "base64", base64Encode(x))
    return(invisible(v))
  }

  if(is.factor(x))
    x = as.character(x)

  if(length(x) != 1) {
    if(length(names(x)))
      x = unname(x)
    return(addArray(v, x))
  }

  addScalar(v, x)
}

addScalar =
function(v, x)
{
  if(!is.double(x) && is.na(x))
    stop("cannot send NA values via XML-RPC", call. = FALSE)

  if(inherits(x, c("POSIXt", "Date"))) {
    xml2::xml_add_child(v, "dateTime.iso8601", formatISO8601(x))
  } else if(is.logical(x)) {
    xml2::xml_add_child(v, "boolean", if(x) "1" else "0")
  } else if(is.integer(x)) {
    xml2::xml_add_child(v, "int", as.character(x))
  } else if(is.double(x)) {
    if(is.na(x) && !is.nan(x))
      stop("cannot send NA values via XML-RPC", call. = FALSE)
    if(!is.finite(x))
      stop("cannot send non-finite numbers via XML-RPC", call. = FALSE)
    xml2::xml_add_child(v, "double", format(x, scientific = FALSE, digits = 15, trim = TRUE))
  } else if(is.character(x)) {
    xml2::xml_add_child(v, "string", enc2utf8(x))
  } else
    stop("don't know how to convert an object of class ", class(x)[1], " to XML-RPC", call. = FALSE)

  invisible(v)
}

addArray =
function(v, x, asIs = FALSE)
{
  data = xml2::xml_add_child(xml2::xml_add_child(v, "array"), "data")
  if(is.atomic(x) && inherits(x, c("POSIXt", "Date")))
    x = lapply(seq_along(x), function(i) x[i])
  for(el in as.list(x))
    addValue(data, el)
  invisible(v)
}

addStruct =
function(v, x)
{
  s = xml2::xml_add_child(v, "struct")
  for(id in names(x)) {
    m = xml2::xml_add_child(s, "member")
    xml2::xml_add_child(m, "name", id)
    addValue(m, x[[id]])
  }
  invisible(v)
}

formatISO8601 =
function(x)
{
  if(inherits(x, "Date"))
    format(x, "%Y%m%dT00:00:00")
  else
    format(as.POSIXct(x), "%Y%m%dT%H:%M:%S")
}


###########
# XML-RPC -> R

convertToR =
function(node)
{
  if(is.character(node))
    node = parseResponse(node)

  fault = xml2::xml_find_first(node, "/methodResponse/fault/value")
  if(!inherits(fault, "xml_missing")) {
    f = xmlRPCToR(fault)
    e = simpleError(paste("faultCode: ", f$faultCode, " faultString: ", f$faultString))
    e$faultCode = f$faultCode
    e$faultString = f$faultString
    class(e) = c("XMLRPCError", class(e))
    stop(e)
  }

  vals = lapply(xml2::xml_find_all(node, "/methodResponse/params/param/value"), xmlRPCToR)
  if(length(vals) == 1)
    vals[[1]]
  else
    vals
}

parseResponse =
  # Servers with misbehaving plugins sometimes emit a BOM or whitespace before
  # the XML declaration, which is a fatal error for a strict XML parser.
function(txt)
{
  txt = sub("^[\ufeff[:space:]]+", "", txt)
  if(!grepl("^<", txt))
    stop("response is not XML: ", substr(txt, 1, 200), call. = FALSE)
  xml2::read_xml(txt)
}

xmlRPCToR =
function(node)
{
  if(xml2::xml_name(node) == "value") {
    kids = xml2::xml_children(node)
       # A <value> with no type element is a string.
    if(length(kids) == 0)
      return(xml2::xml_text(node))
    node = kids[[1]]
  }

  txt = function() xml2::xml_text(node)

  switch(xml2::xml_name(node),
         array = xmlRPCToR.array(node),
         struct = xmlRPCToR.struct(node),
         i4 =, int = , i8 = {
           val = as.numeric(txt())
           if(abs(val) <= .Machine$integer.max) as.integer(val) else val
         },
         boolean = trimws(txt()) %in% c("1", "true"),
         double = as.numeric(txt()),
         string = txt(),
         dateTime.iso8601 = parseISO8601(txt()),
         base64 = {
           val = base64Decode(txt())
           if(any(val == as.raw(0L))) val else { s = rawToChar(val); Encoding(s) = "UTF-8"; s }
         },
         nil = NULL,
         txt())
}

xmlRPCToR.struct =
function(node)
{
  members = xml2::xml_find_all(node, "./member")
  ans = lapply(members, function(m) xmlRPCToR(xml2::xml_find_first(m, "./value")))
  names(ans) = xml2::xml_text(xml2::xml_find_first(members, "./name"))
  ans
}

xmlRPCToR.array =
  # Arrays of scalars of a common type are simplified to a vector, as XMLRPC did.
function(node)
{
  ans = lapply(xml2::xml_find_all(node, "./data/value"), xmlRPCToR)
  if(length(ans) == 0)
    return(list())

  scalar = all(vapply(ans, function(x) is.atomic(x) && length(x) == 1 && !is.raw(x), logical(1)))
  sameClass = length(unique(lapply(ans, class))) == 1
  if(scalar && sameClass)
    do.call(c, ans)
  else
    ans
}

parseISO8601 =
function(x)
{
  x = trimws(x)
  zonePattern = "(Z|[+-][0-9]{2}:?[0-9]{2})$"
  zone = regmatches(x, regexpr(zonePattern, x))
  x = gsub("-", "", sub(zonePattern, "", x), fixed = TRUE)
  if(!length(zone))
    return(as.POSIXct(strptime(x, "%Y%m%dT%H:%M:%S")))

     # An explicit zone: interpret as UTC and shift by the offset.
  ans = as.POSIXct(strptime(x, "%Y%m%dT%H:%M:%S", tz = "UTC"))
  if(zone != "Z") {
    hm = as.integer(c(substr(zone, 2, 3), substr(zone, nchar(zone) - 1, nchar(zone))))
    ans = ans - (if(substr(zone, 1, 1) == "-") -1 else 1) * (hm[1] * 3600 + hm[2] * 60)
  }
  ans
}

base64Encode =
function(x)
  base64enc::base64encode(x)

base64Decode =
function(x)
  base64enc::base64decode(gsub("[[:space:]]", "", x))
