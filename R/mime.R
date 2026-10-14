getExtension =
  #
  # getExtension("foo.R")
  # getExtension("foo.tar.gz")
  #
function(name, multiple = FALSE)
{
  ifelse(grepl(".\\.[a-zA-Z0-9]+$", name), gsub(".*\\.([a-zA-Z0-9]+)$", "\\1", name), "")
}


otherMIMETypes = c("r" = "text/plain", # WordPress rejects unknown types such as text/R-code
                   "rmd" = "text/plain",
                   "svg" = "image/svg+xml",
                   "webp" = "image/webp",
                   "avif" = "image/avif",
                   "json" = "application/json",
                   "csv" = "text/csv",
                   "mp4" = "video/mp4",
                   "webm" = "video/webm",
                   "docx" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                   "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                   "pptx" = "application/vnd.openxmlformats-officedocument.presentationml.presentation")

mimeTable = local({
  tbl = NULL
  function() {
    if(is.null(tbl)) {
      env = new.env()
      utils::data("mimeTypeExtensions", package = "RWordPress", envir = env)
      tbl <<- env$mimeTypeExtensions
    }
    tbl
  }
})

guessMIMEType =
  #
  # Determine the MIME type
  #
  # guessMIMEType("foo.txt")
  # guessMIMEType("foo.png")
  # guessMIMEType("foo.jpeg")
  # guessMIMEType("foo.Z")
  # guessMIMEType(c("foo.txt", "foo.png", "foo.jpeg", "foo.Z"))
  #
  # otherMIMETypes overrides/extends the table we constructed, and anything
  # still unknown is sent as application/octet-stream.
  #
function(name)
{
  ext = tolower(getExtension(name))
  ans = unname(otherMIMETypes[ext])
  i = is.na(ans)
  ans[i] = unname(mimeTable()[ext[i]])
  ans[is.na(ans)] = "application/octet-stream"
  structure(ans, names = name)
}
