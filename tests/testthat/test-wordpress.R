# End-to-end tests over HTTP against the fake WordPress server (setup-fake-server.R).

withServer = function() {
  skip_if_no_server()
  withr::local_options(WordpressURL = fakeServer$url, WordpressLogin = fakeServer$login,
                       .local_envir = parent.frame())
}

test_that("values survive a real HTTP round trip unchanged", {
  withServer()
  t = as.POSIXct("2024-01-02 13:45:10")
  out = RWordPress:::xml.rpc(fakeServer$url, "test.echo", 1L, 2.5, TRUE, "x & <y> é",
                             t, as.raw(0:255), c("a", "b"), list(), list(k = list(1L, "v")))
  expect_equal(vapply(out, `[[`, "", "type"),
               c("int", "double", "boolean", "string", "dateTime.iso8601", "base64",
                 "array", "array", "struct"))
  expect_identical(out[[4]]$value, "x & <y> é")
  expect_equal(out[[5]]$value, t)
  expect_identical(out[[7]]$value, c("a", "b"))
  expect_equal(out[[9]]$value, list(k = list(1L, "v")))
  expect_identical(RWordPress:::xml.rpc(fakeServer$url, "test.untyped"), "plain text")
})

test_that("the blog can be queried", {
  withServer()
  b = getUsersBlogs()
  expect_equal(b$blogName, "Test blog")
  expect_true(length(supportedMethods()) > 10)
  expect_equal(categories(), c(Uncategorized = "1", R = "2", Statistics = "3"))
  expect_equal(getPostStatusList()$publish, "Published")
  expect_true("pending" %in% names(getPostStatusList()))   # used to call wp.getPageStatusList
  expect_equal(getTags()[[1]]$name, "rstats")
  expect_equal(getOptions(asDataFrame = FALSE)$blog_title$value, "Test blog")
})

test_that("pages with list-valued fields become a data frame", {
  withServer()
  p = getPages()
  expect_s3_class(p, "data.frame")
  expect_equal(nrow(p), 2)
  expect_equal(p$title, c("About", "Contact"))
  expect_equal(p$wp_page_parent_id, c(0L, 2L))
  expect_type(p$custom_fields, "list")
})

test_that("posts can be created, read, edited, categorized and deleted", {
  withServer()
  id = newPost(list(description = c("<p>line 1</p>", "<p>line 2</p>"), title = "From R",
                    categories = "R", mt_keywords = c("r", "xmlrpc")),
               publish = FALSE)
  expect_s3_class(id, "WordpressPostId")

  p = getPost(id)
  expect_equal(p$title, "From R")
  expect_equal(p$description, "<p>line 1</p>\n<p>line 2</p>")
  expect_equal(p$categories, "R")          # a single category used to be dropped
  expect_equal(p$mt_keywords, "r,xmlrpc")
  expect_equal(p$post_status, "draft")
  expect_s3_class(p$dateCreated, "POSIXct")

  expect_true(editPost(id, "<p>new body</p>"))
  expect_equal(getPost(id)$description, "<p>new body</p>")
  expect_true(appendToPost(id, "<p>more</p>"))
  expect_equal(getPost(id)$description, "<p>new body</p>\n<p>more</p>")

  expect_true(setPostCategories(id, c("statis", "R")))   # partial name match
  expect_setequal(getPost(id)$categories, c("Statistics", "R"))
  expect_equal(getPost(id)$post_status, "publish")
  expect_true(setPostCategories(id, 1, publish = FALSE))
  expect_equal(getPostCategories(id)[[1]]$categoryName, "Uncategorized")

  recent = getPosts(num = 5)
  expect_s3_class(recent, "data.frame")
  expect_true(as.character(id) %in% recent$postid)

  expect_true(deletePost(id))
  expect_error(getPost(id), class = "XMLRPCError")
})

test_that("an empty post list gives an empty data frame", {
  withServer()
  expect_equal(nrow(RWordPress:::makeIntoDataFrame(list())), 0)
})

test_that("pages can be created and deleted", {
  withServer()
  id = newPage(list(title = "Page", description = "body"))
  expect_equal(getPost(id)$title, "Page")
  expect_true(deletePage(id))
})

test_that("categories are created with their full name and deleted by name", {
  withServer()
  cat = newCategory("Reproducible research", description = "made from R")
  expect_s3_class(cat, "WordPressCategoryId")
  expect_equal(names(categories())[categories() == as.character(cat)], "Reproducible research")
  expect_true(deleteCategory("Reproducible research"))
  expect_false("Reproducible research" %in% names(categories()))
  expect_error(deleteCategory("no such category at all"), "don't recognize")
})

test_that("files and in-memory content can be uploaded byte-for-byte", {
  withServer()
  f = tempfile(fileext = ".png")
  bytes = as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0:255))
  writeBin(bytes, f)
  u = uploadFile(f)
  expect_equal(u$type, "image/png")
  expect_equal(u$url, paste0("http://example.test/wp-content/uploads/", basename(f)))
  got = RWordPress:::xml.rpc(fakeServer$url, "test.getUpload", basename(f))
  expect_identical(got, bytes)

  u = uploadFile(I(c("x,y", "1,2")), remoteName = "d.csv")
  expect_equal(u$type, "text/csv")
  expect_identical(RWordPress:::xml.rpc(fakeServer$url, "test.getUpload", "d.csv"), "x,y\n1,2")
  expect_error(uploadFile(I("abc")), "remoteName")
})

test_that("bad credentials give a clear XML-RPC fault", {
  withServer()
  e = tryCatch(getUsersBlogs(login = c(admin = "wrong")), error = identity)
  expect_s3_class(e, "XMLRPCError")
  expect_equal(e$faultCode, 403L)
})

test_that("passwords with XML special characters are escaped", {
  withServer()
  expect_no_error(getUsersBlogs())
})

test_that("a BOM before the response does not break parsing", {
  withServer()
  expect_equal(getUsersBlogs(.server = paste0(fakeServer$base, "/bom/xmlrpc.php"))$blogName, "Test blog")
})

test_that("HTTP errors (e.g. XML-RPC disabled) are reported", {
  withServer()
  expect_error(getUsersBlogs(.server = paste0(fakeServer$base, "/disabled/xmlrpc.php")),
               "HTTP error 405.*accepts POST requests only")
})

test_that("curl options can be passed through ... with RCurl-style names", {
  withServer()
  expect_no_error(getPostStatusList(.opts = list(connecttimeout = 5, ssl.verifypeer = TRUE)))
})
