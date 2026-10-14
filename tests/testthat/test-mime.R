test_that("guessMIMEType works on vectors (failed in R >= 4.2)", {
  expect_equal(guessMIMEType(c("a.png", "b.SVG", "c.jpeg", "d.R", "e", "f.unknownext")),
               c(a.png = "image/png", b.SVG = "image/svg+xml", c.jpeg = "image/jpeg",
                 d.R = "text/plain", e = "application/octet-stream",
                 f.unknownext = "application/octet-stream"))
})

test_that("guessMIMEType works without attaching the package", {
  skip_if_not_installed("callr")
  out = callr::r(function(lib) { .libPaths(lib); RWordPress::guessMIMEType("x.gif") }, args = list(.libPaths()))
  expect_equal(out, c(x.gif = "image/gif"))
})
