# A minimal, in-memory imitation of a WordPress XML-RPC endpoint, run in a
# separate R process by setup-fake-server.R.  The request parser here is
# deliberately strict and independent of the package's own decoder: a request
# that is not valid XML-RPC (e.g. a <param> without a <value>) is answered with
# a fault, so the tests catch serialization bugs that lenient servers hide.
#
# Paths:
#   /xmlrpc.php           normal endpoint
#   /bom/xmlrpc.php       response preceded by a BOM and whitespace
#   /disabled/xmlrpc.php  HTTP 405, as when a host disables XML-RPC

USER = "admin"
PASS = "s3cret & <pass>"

state = new.env()
state$posts = list()
state$categories = c("1" = "Uncategorized", "2" = "R", "3" = "Statistics")
state$uploads = list()
state$nextId = 100L

fault = function(code, msg) structure(list(faultCode = code, faultString = msg), class = "fault")

###### decoding (strict)

decodeValue = function(v) {
  if(xml2::xml_name(v) != "value") stop("expected <value>, got <", xml2::xml_name(v), ">")
  kids = xml2::xml_children(v)
  if(length(kids) == 0) return(xml2::xml_text(v))
  if(length(kids) > 1) stop("<value> with more than one child")
  k = kids[[1]]
  txt = xml2::xml_text(k)
  switch(xml2::xml_name(k),
    string = structure(txt, xmlrpc = "string"),
    int = , i4 = { if(!grepl("^[-+]?[0-9]+$", txt)) stop("bad int ", txt); structure(as.integer(txt), xmlrpc = "int") },
    double = { if(!grepl("^[-+]?[0-9]*\\.?[0-9]+$", txt)) stop("bad double ", txt); structure(as.numeric(txt), xmlrpc = "double") },
    boolean = { if(!txt %in% c("0", "1")) stop("bad boolean ", txt); structure(txt == "1", xmlrpc = "boolean") },
    dateTime.iso8601 = { if(!grepl("^[0-9]{8}T[0-9]{2}:[0-9]{2}:[0-9]{2}$", txt)) stop("bad date ", txt); structure(txt, xmlrpc = "dateTime.iso8601") },
    base64 = structure(list(base64enc::base64decode(txt)), xmlrpc = "base64"),
    array = {
      d = xml2::xml_children(k)
      if(length(d) != 1 || xml2::xml_name(d[[1]]) != "data") stop("<array> must contain one <data>")
      structure(lapply(xml2::xml_children(d[[1]]), decodeValue), xmlrpc = "array")
    },
    struct = {
      ms = xml2::xml_children(k)
      ans = lapply(ms, function(m) {
        if(xml2::xml_name(m) != "member") stop("<struct> child must be <member>")
        n = xml2::xml_find_all(m, "./name"); val = xml2::xml_find_all(m, "./value")
        if(length(n) != 1 || length(val) != 1 || length(xml2::xml_children(m)) != 2)
          stop("<member> must have exactly one <name> and one <value>")
        decodeValue(val[[1]])
      })
      names(ans) = xml2::xml_text(xml2::xml_find_first(ms, "./name"))
      structure(ans, xmlrpc = "struct")
    },
    stop("unknown type <", xml2::xml_name(k), ">"))
}

decodeRequest = function(body) {
  doc = xml2::read_xml(body)
  if(xml2::xml_name(doc) != "methodCall") stop("root must be <methodCall>")
  method = xml2::xml_text(xml2::xml_find_first(doc, "/methodCall/methodName"))
  params = xml2::xml_find_all(doc, "/methodCall/params/param")
  args = lapply(params, function(p) {
    kids = xml2::xml_children(p)
    if(length(kids) != 1) stop("<param> must contain exactly one <value>")
    decodeValue(kids[[1]])
  })
  list(method = method, args = args)
}

###### encoding (what WordPress' IXR library produces)

esc = function(x) {
  x = gsub("&", "&amp;", x, fixed = TRUE); x = gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
enc = function(x) {
  t = attr(x, "xmlrpc")
  if(!is.null(t) && t == "dateTime.iso8601") return(sprintf("<value><dateTime.iso8601>%s</dateTime.iso8601></value>", x))
  if(!is.null(t) && t == "base64") return(sprintf("<value><base64>%s</base64></value>", base64enc::base64encode(x[[1]])))
  if(inherits(x, "POSIXct")) return(sprintf("<value><dateTime.iso8601>%s</dateTime.iso8601></value>", format(x, "%Y%m%dT%H:%M:%S", tz = "UTC")))
  if(is.list(x) && (!is.null(names(x)) || identical(t, "struct")))
    return(paste0("<value><struct>", paste0(sprintf("<member><name>%s</name>%s</member>", esc(names(x)),
                  vapply(x, enc, "")), collapse = ""), "</struct></value>"))
  if(is.list(x) || length(x) != 1)
    return(paste0("<value><array><data>", paste0(vapply(as.list(x), enc, ""), collapse = ""), "</data></array></value>"))
  if(is.logical(x)) return(sprintf("<value><boolean>%d</boolean></value>", as.integer(x)))
  if(is.integer(x)) return(sprintf("<value><int>%d</int></value>", x))
  if(is.double(x)) return(sprintf("<value><double>%s</double></value>", format(x, digits = 15)))
  if(identical(t, "untyped")) return(sprintf("<value>%s</value>", esc(x)))
  sprintf("<value><string>%s</string></value>", esc(x))
}
respond = function(x) {
  if(inherits(x, "fault"))
    return(paste0('<?xml version="1.0" encoding="UTF-8"?>\n<methodResponse><fault>', enc(unclass(x)), '</fault></methodResponse>'))
  paste0('<?xml version="1.0" encoding="UTF-8"?>\n<methodResponse><params><param>', enc(x), '</param></params></methodResponse>')
}

###### methods

checkAuth = function(user, pass) identical(as.character(user), USER) && identical(as.character(pass), PASS)

postStruct = function(id) {
  p = state$posts[[id]]
  list(postid = id, title = p$title, description = p$description,
       categories = I(as.list(unname(p$categories))),
       mt_keywords = p$mt_keywords, post_status = p$status, wp_slug = "",
       dateCreated = structure(p$dateCreated, xmlrpc = "dateTime.iso8601"))
}

# Post ids may be sent as int or string, as WordPress accepts either.
applyContent = function(p, s) {
  if(attr(s, "xmlrpc") != "struct") stop("content must be a struct")
  for(f in c("title", "description", "mt_keywords", "post_status"))
    if(!is.null(s[[f]])) p[[sub("post_", "", f)]] = as.character(s[[f]])
  # WordPress silently ignores categories that are not an array.
  if(!is.null(s$categories) && attr(s$categories, "xmlrpc") == "array")
    p$categories = vapply(s$categories, as.character, "")
  if(!is.null(s$dateCreated)) p$dateCreated = as.character(s$dateCreated)
  p
}

handlers = list(
  "mt.supportedMethods" = function() I(names(handlers)),
  "mt.supportedTextFilters" = function() list(),
  "test.echo" = function(...) {
    a = list(...)
    lapply(a, function(x) list(type = attr(x, "xmlrpc"), value = x))
  },
  "test.getUpload" = function(name) structure(list(state$uploads[[name]]$bits), xmlrpc = "base64"),
  "test.untyped" = function() structure("plain text", xmlrpc = "untyped"),
  "test.mixed" = function() list(TRUE, 3L, 2.5, "x", structure("20240102T13:45:10", xmlrpc = "dateTime.iso8601"),
                                 list(), structure(list(), xmlrpc = "struct")),
  "wp.getUsersBlogs" = function(user, pass) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    list(list(isAdmin = TRUE, url = "http://example.test/", blogid = "1", blogName = "Test blog",
              xmlrpc = "http://example.test/xmlrpc.php"))
  },
  "metaWeblog.newPost" = function(blogid, user, pass, content, publish) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(attr(publish, "xmlrpc") != "boolean") stop("publish must be a boolean")
    id = as.character(state$nextId <- state$nextId + 1L)
    p = applyContent(list(title = "", description = "", categories = character(), mt_keywords = "",
                          dateCreated = "20260101T00:00:00"), content)
    p$status = if(publish) "publish" else "draft"
    state$posts[[id]] = p
    id
  },
  "wp.newPage" = function(blogid, user, pass, content, publish) handlers[["metaWeblog.newPost"]](blogid, user, pass, content, publish),
  "metaWeblog.getPost" = function(postid, user, pass) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(is.null(state$posts[[as.character(postid)]])) return(fault(404L, "Invalid post ID."))
    postStruct(postid)
  },
  "metaWeblog.editPost" = function(postid, user, pass, content, publish) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(is.null(state$posts[[as.character(postid)]])) return(fault(404L, "Invalid post ID."))
    state$posts[[as.character(postid)]] = applyContent(state$posts[[as.character(postid)]], content)
    TRUE
  },
  "metaWeblog.deletePost" = function(appkey, postid, user, pass, publish) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(is.null(state$posts[[as.character(postid)]])) return(fault(404L, "Invalid post ID."))
    state$posts[[as.character(postid)]] = NULL
    TRUE
  },
  "wp.deletePage" = function(blogid, user, pass, pageid) handlers[["metaWeblog.deletePost"]]("", pageid, user, pass, TRUE),
  "mt.getRecentPostTitles" = function(blogid, user, pass, num) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    ids = utils::head(rev(names(state$posts)), num)
    lapply(ids, function(id) list(dateCreated = structure(state$posts[[id]]$dateCreated, xmlrpc = "dateTime.iso8601"),
                                  userid = "1", postid = id, title = state$posts[[id]]$title,
                                  post_status = state$posts[[id]]$status))
  },
  "wp.getPages" = function(blogid, user, pass) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    list(list(page_id = 2L, title = "About", description = "about", categories = I(list("Uncategorized")),
              custom_fields = I(list(list(id = "1", key = "k", value = "v"))), wp_page_parent_id = 0L),
         list(page_id = 3L, title = "Contact", description = "contact", categories = I(list()),
              custom_fields = I(list()), wp_page_parent_id = 2L))
  },
  "wp.getPostStatusList" = function(blogid, user, pass) list(draft = "Draft", pending = "Pending Review", private = "Private", publish = "Published"),
  "wp.getPageStatusList" = function(blogid, user, pass) list(draft = "Draft", private = "Private", publish = "Published"),
  "wp.getTags" = function(blogid, user, pass) list(list(tag_id = "7", name = "rstats", count = "3", slug = "rstats")),
  "wp.getOptions" = function(blogid, user, pass) list(blog_title = list(desc = "Site Title", readonly = FALSE, value = "Test blog")),
  "mt.getCategoryList" = function(blogid, user, pass) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    unname(Map(function(id, nm) list(categoryId = id, categoryName = nm), names(state$categories), state$categories))
  },
  "wp.newCategory" = function(blogid, user, pass, cat) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(attr(cat, "xmlrpc") != "struct" || is.null(cat$name)) return(fault(500L, "Invalid category: expected a struct with a name"))
    id = as.character(state$nextId <- state$nextId + 1L)
    state$categories[id] = as.character(cat$name)
    as.integer(id)
  },
  "wp.deleteCategory" = function(blogid, user, pass, catid) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(is.na(state$categories[catid])) return(fault(500L, "Invalid category ID."))
    state$categories = state$categories[names(state$categories) != catid]
    TRUE
  },
  "mt.setPostCategories" = function(postid, user, pass, cats) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(attr(cats, "xmlrpc") != "array") stop("categories must be an array")
    ids = vapply(cats, function(s) { if(attr(s, "xmlrpc") != "struct") stop("each category must be a struct"); as.character(s$categoryId) }, "")
    state$posts[[as.character(postid)]]$categories = unname(state$categories[ids])
    TRUE
  },
  "mt.getPostCategories" = function(postid, user, pass) {
    nms = state$posts[[as.character(postid)]]$categories
    lapply(nms, function(n) list(categoryName = n, categoryId = names(state$categories)[match(n, state$categories)], isPrimary = FALSE))
  },
  "mt.publishPost" = function(postid, user, pass) { state$posts[[as.character(postid)]]$status = "publish"; TRUE },
  "wp.uploadFile" = function(blogid, user, pass, data) {
    if(!checkAuth(user, pass)) return(fault(403L, "Incorrect username or password."))
    if(attr(data$bits, "xmlrpc") != "base64") stop("bits must be base64")
    if(attr(data$overwrite, "xmlrpc") != "boolean") stop("overwrite must be a boolean")
    state$uploads[[as.character(data$name)]] = list(bits = data$bits[[1]], type = as.character(data$type))
    list(id = "55", file = as.character(data$name), url = paste0("http://example.test/wp-content/uploads/", data$name),
         type = as.character(data$type))
  }
)

handle = function(req) {
  if(grepl("^/disabled", req$PATH_INFO))
    return(list(status = 405L, headers = list("Content-Type" = "text/plain"),
                body = "XML-RPC server accepts POST requests only."))

  body = rawToChar(req$rook.input$read())
  out = tryCatch({
    r = decodeRequest(body)
    h = handlers[[r$method]]
    if(is.null(h)) fault(-32601L, paste("server error. requested method", r$method, "does not exist."))
    else do.call(h, r$args)
  }, error = function(e) fault(-32700L, paste("parse error. not well formed:", conditionMessage(e))))

  txt = respond(out)
  if(grepl("^/bom", req$PATH_INFO))
    txt = paste0("﻿\n  ", txt)
  list(status = 200L, headers = list("Content-Type" = "text/xml; charset=UTF-8"), body = charToRaw(enc2utf8(txt)))
}

run = function(port) httpuv::runServer("127.0.0.1", port, list(call = handle))
