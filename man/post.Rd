\name{modifyBlog}
\alias{newCategory}
\alias{publishPost}
\alias{uploadFile}
\alias{newPage}
\alias{newPost}
\alias{deleteCategory}
\alias{deletePage}
\alias{deletePost}
\alias{setPostCategories}
\alias{setPostCategories,ANY,list-method}
\alias{setPostCategories,ANY,character-method}
\alias{setPostCategories,ANY,numeric-method}
\alias{editPost}
\alias{appendToPost}
\alias{guessMIMEType}

\title{Functions for posting, modifying and deleting blog content}
\description{
  These functions are for changing the content of a blog.
  We can create a new page, a new post on a page,
  add a new category, delete a category.
}
\usage{
newCategory(category, description = "???", slug = category, parent = 0L,
             blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")),
              .server = getServerURL())
publishPost(postid, login = getOption("WordpressLogin", stop("need a login and password")),
             ...,  .server = getServerURL())
uploadFile(what, type = guessMIMEType(remoteName), blogid = 0L,
            login = getOption("WordpressLogin", stop("need a login and password")),
             remoteName = basename(what), overwrite = TRUE, ..., .server = getServerURL()  )
newPage(content, publish = TRUE, blogid = 0,
            login = getOption("WordpressLogin", stop("need a login and password")), method = "wp.newPage", .server = getServerURL())
newPost(content, publish = TRUE, blogid = 0,
          login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
deleteCategory(categoryid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL()  )
deletePage(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL()  )
deletePost(postid, login = getOption("WordpressLogin", stop("need a login and password")),
            publish = TRUE, appId = "RWordPress", ..., .server = getServerURL() )
editPost(postid, content, login = getOption("WordpressLogin", stop("need a login and password")),
          publish = TRUE, appId = "RWordpress", ..., .server = getServerURL())
appendToPost(postid, content, login = getOption("WordpressLogin", stop("need a login and password")),
              .server = getServerURL())
setPostCategories(postid, categories,
                  login = getOption("WordpressLogin", stop("need a login and password")),
                  publish = TRUE, ..., .server = getServerURL())
guessMIMEType(name)
}
\arguments{
 \item{category}{the name of the new category to create}
 \item{categoryid}{the identifier for the category of interest, or its name.}
 \item{description}{the description of the new category}
 \item{slug}{the desired name (i.e. URL) of the resource on the server when we
   create it. This can be omitted and the default is typically fine.}
 \item{parent}{the identifier of the parent category, or 0 for a
   top-level category.}
 \item{login}{the login and password given as
 \code{c(login = "password")}}
 \item{postid}{the identifier for the post. See
   \code{\link{getRecentPostTitles}} for the identifiers.}
 \item{...}{additional arguments passed on to the internal XML-RPC
   client. Named arguments \code{.opts} (a list of libcurl options such
   as \code{list(verbose = TRUE)}), \code{.curl} (a handle from
   \code{\link[curl]{new_handle}}) and \code{.convert} are recognized;
   any other arguments are sent as extra XML-RPC parameters.}
 \item{what}{the file to upload. This is typically a file name, but can
   also be an R object given as an object of class \code{AsIs} and that
   is serialized. If it is text, this is the content of the file, not
   the file name. If this is a raw vector (with or without the AsIs
   class), this is also taken as the content. When \code{what} is
   content rather than a file name, \code{remoteName} must be given.
   }
 \item{type}{the MIME type of the file being uploaded.}
 \item{blogid}{the identifier for the blog. Often this can be left as 0
   to identify the default blog identified by the specified login and password.}
 \item{remoteName}{the name to use for the file on the blog server. This
       allows us to use a different name than the one on the local system and
       to specify a name for anonymous object.}
 \item{overwrite}{whether to replace an existing file of the same name
   on the server.}
 \item{content}{either a string giving the body of the post or page, or a
   named list (an XML-RPC struct) with fields such as \code{description}
   (the body), \code{title}, \code{categories}, \code{mt_keywords}
   (tags, comma-separated or a character vector), \code{mt_excerpt},
   \code{post_status} and \code{dateCreated}.
   \code{categories} is always sent as an array, so a single category
   name works.
   For \code{appendToPost}, the text to add to the end of the post.}
 \item{categories}{the categories for the post, given as a character
   vector of category names (matched against \code{\link{getCategories}}),
   a numeric vector of category identifiers, or a list of
   \code{list(categoryId = id)} structs.}
 \item{publish}{a logical value that controls whether the changes are
   immediately published.}
 \item{pageid}{the identifier for the page on the blog}
 \item{appId}{the name of our program making the request}
 \item{method}{the name of the actual XMLRPC method to invoke. This
   allows one function to be parameterized to implement multiple methods
   that are very similar.}
 \item{name}{a character vector of file names.}
  \item{.server}{a string giving the URL of the Wordpress server. This
    can be specified by the caller directly or can be set as a global
    option in R named \code{WordpressURL}, e.g.
       \code{options(WordpressURL = "https://www.mywordpress.org/xmlrpc.php")}.}
}
\value{
  \code{newPost} and \code{newPage} return the identifier of the new post or page.
  \code{uploadFile} returns a list with elements including \code{url},
  \code{file} and \code{type}.
  \code{guessMIMEType} returns a named character vector of MIME types.
  The others return the value from the server, typically \code{TRUE}.
}
\examples{
guessMIMEType(c("plot.png", "code.R", "figure.svg"))

\dontrun{
 options(WordpressLogin = c(myname = "my-application-password"),
         WordpressURL = "https://myblog.example.com/xmlrpc.php")

 id = newPost(list(description = "<p>Posted from R</p>", title = "Hello",
                   categories = "R", mt_keywords = c("r", "xmlrpc")),
              publish = FALSE)
 editPost(id, list(title = "Hello again", description = "<p>Edited</p>"))
 appendToPost(id, "<p>One more paragraph</p>")
 deletePost(id)

 uploadFile("~/bob.png")$url
 uploadFile(I(c("x,y", "1,2")), remoteName = "data.csv")
}
}

\keyword{IO}
\keyword{programming}
