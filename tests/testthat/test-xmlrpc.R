body = function(...) as.character(RWordPress:::createBody("m", list(...)))
params = function(...) xml2::xml_find_all(xml2::read_xml(body(...)), "/methodCall/params/param")

test_that("every param holds exactly one <value>, including lists", {
  ps = params(1L, "a", list(), list(a = 1L), list(1, "b"), NULL)
  for(p in ps)
    expect_equal(xml2::xml_name(xml2::xml_children(p)), "value")
})

test_that("scalars use the right XML-RPC types", {
  x = xml2::read_xml(body(1L, 2.5, TRUE, "x & <y>", as.raw(1:3)))
  types = xml2::xml_name(xml2::xml_find_all(x, "//param/value/*"))
  expect_equal(types, c("int", "double", "boolean", "string", "base64"))
  expect_equal(xml2::xml_text(xml2::xml_find_all(x, "//param/value/*")),
               c("1", "2.5", "1", "x & <y>", "AQID"))
  expect_match(body("x & <y>"), "x &amp; &lt;y&gt;", fixed = TRUE)
})

test_that("times keep their minutes (XMLRPC wrote %H:%H)", {
  t = as.POSIXct("2024-01-02 13:45:10")
  expect_match(body(t), "<dateTime.iso8601>20240102T13:45:10</dateTime.iso8601>", fixed = TRUE)
  expect_match(body(as.Date("2024-01-02")), "20240102T00:00:00", fixed = TRUE)
  expect_match(body(as.POSIXlt(t)), "20240102T13:45:10", fixed = TRUE)
})

test_that("doubles are written without exponents", {
  expect_match(body(1e20), "<double>100000000000000000000</double>", fixed = TRUE)
  expect_match(body(1/3), "<double>0.333333333333333</double>", fixed = TRUE)
})

test_that("vectors, I() and NULL become arrays; named lists become structs", {
  x = xml2::read_xml(body(c("a", "b"), I("a"), NULL, list(k = "v"), factor("lvl")))
  vals = xml2::xml_find_all(x, "/methodCall/params/param/value")
  expect_equal(xml2::xml_name(xml2::xml_find_first(vals, "./*")), c("array", "array", "array", "struct", "string"))
  expect_equal(xml2::xml_text(xml2::xml_find_all(vals[[2]], ".//string")), "a")
  expect_equal(xml2::xml_text(xml2::xml_find_first(vals[[5]], "./string")), "lvl")
})

test_that("NA and non-finite numbers are rejected rather than sent as text", {
  expect_error(body(NA), "NA")
  expect_error(body(NA_character_), "NA")
  expect_error(body(Inf), "non-finite")
})

resp = function(v) paste0('<?xml version="1.0"?><methodResponse><params><param>', v, '</param></params></methodResponse>')

test_that("responses are converted to R", {
  conv = RWordPress:::convertToR
  expect_identical(conv(resp("<value><int>5</int></value>")), 5L)
  expect_identical(conv(resp("<value><i4>-5</i4></value>")), -5L)
  expect_identical(conv(resp("<value><i8>9007199254740</i8></value>")), 9007199254740)
  expect_identical(conv(resp("<value><boolean>0</boolean></value>")), FALSE)
  expect_identical(conv(resp("<value>untyped</value>")), "untyped")
  expect_identical(conv(resp("<value><string></string></value>")), "")
  expect_null(conv(resp("<value><nil/></value>")))
  expect_identical(conv(resp("<value><base64>aGVsbG8=</base64></value>")), "hello")
  expect_identical(conv(resp("<value><array><data></data></array></value>")), list())
  expect_identical(conv(resp("<value><struct></struct></value>")), structure(list(), names = character()))
  expect_identical(conv(resp("<value><array><data><value><int>1</int></value><value><int>2</int></value></data></array></value>")), 1:2)
  expect_identical(conv(resp("<value><array><data><value><int>1</int></value><value>a</value></data></array></value>")), list(1L, "a"))
  expect_equal(conv(resp("<value><struct><member><name>a</name><value><double>1.5</double></value></member></struct></value>")), list(a = 1.5))
})

test_that("dates are parsed with and without separators and zones", {
  conv = RWordPress:::convertToR
  local = as.POSIXct("2024-01-02 13:45:10")
  expect_equal(conv(resp("<value><dateTime.iso8601>20240102T13:45:10</dateTime.iso8601></value>")), local)
  expect_equal(conv(resp("<value><dateTime.iso8601>2024-01-02T13:45:10</dateTime.iso8601></value>")), local)
  utc = as.POSIXct("2024-01-02 13:45:10", tz = "UTC")
  expect_equal(as.numeric(conv(resp("<value><dateTime.iso8601>20240102T13:45:10Z</dateTime.iso8601></value>"))), as.numeric(utc))
  expect_equal(as.numeric(conv(resp("<value><dateTime.iso8601>20240102T15:45:10+02:00</dateTime.iso8601></value>"))), as.numeric(utc))
})

test_that("faults become XMLRPCError conditions", {
  txt = '<?xml version="1.0"?><methodResponse><fault><value><struct>
    <member><name>faultCode</name><value><int>403</int></value></member>
    <member><name>faultString</name><value><string>Incorrect username or password.</string></value></member>
    </struct></value></fault></methodResponse>'
  e = tryCatch(RWordPress:::convertToR(txt), error = identity)
  expect_s3_class(e, "XMLRPCError")
  expect_equal(e$faultCode, 403L)
  expect_match(conditionMessage(e), "Incorrect username or password")
})

test_that("a BOM or whitespace before the XML declaration is tolerated", {
  expect_identical(RWordPress:::convertToR(paste0("﻿\n  ", resp("<value><int>1</int></value>"))), 1L)
})
