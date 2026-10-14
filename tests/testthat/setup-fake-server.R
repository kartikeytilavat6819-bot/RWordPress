# Start the fake WordPress server (fake-wordpress.R) in a background R process.

fakeServer = local({
  if(!requireNamespace("callr", quietly = TRUE) || !requireNamespace("httpuv", quietly = TRUE))
    return(NULL)

  port = httpuv::randomPort()
  proc = callr::r_bg(function(file, port) { source(file, local = TRUE); run(port) },
                     args = list(file = normalizePath(test_path("fake-wordpress.R")), port = port))
  withr::defer(proc$kill(), testthat::teardown_env())

  base = sprintf("http://127.0.0.1:%d", port)
  for(i in 1:100) {
    ok = tryCatch({ curl::curl_fetch_memory(base); TRUE }, error = function(e) FALSE)
    if(ok || !proc$is_alive()) break
    Sys.sleep(0.1)
  }
  if(!ok) {
    warning("fake WordPress server did not start: ", paste(proc$read_all_error_lines(), collapse = "\n"))
    return(NULL)
  }
  list(url = paste0(base, "/xmlrpc.php"), base = base,
       login = c(admin = "s3cret & <pass>"))
})

skip_if_no_server = function() {
  if(is.null(fakeServer))
    skip("callr/httpuv not available to run the fake WordPress server")
}
