appendToPost =
function(postid, content, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  current = getPost(postid, login, .server = .server)
  new = paste(current$description, paste(content, collapse = "\n"), sep = "\n")
  editPost(postid, list(title = current$title, description = new), login, TRUE, .server = .server)
}
