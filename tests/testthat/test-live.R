# Talks to a real WordPress XML-RPC endpoint; skipped on CRAN and when offline.

test_that("a real WordPress server answers", {
  skip_on_cran()
  skip_if_offline("wordpress.org")
  url = "https://wordpress.org/xmlrpc.php"
  m = tryCatch(supportedMethods(.server = url), error = function(e) skip(conditionMessage(e)))
  expect_true(all(c("wp.getUsersBlogs", "metaWeblog.newPost") %in% m))

  e = tryCatch(getUsersBlogs(login = c(nobody = "wrong"), .server = url), error = identity)
  expect_s3_class(e, "XMLRPCError")
  expect_equal(e$faultCode, 403L)
})
