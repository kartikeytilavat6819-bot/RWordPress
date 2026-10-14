# http://codex.wordpress.org/XML-RPC_wp
# http://www.movabletype.org/documentation/developer/api/

ServerURL = "https://omegahat.wordpress.com/xmlrpc.php"
#ServerURL = "https://www.carlboettiger.info/xmlrpc.php"

getServerURL =
 function() {
   getOption("WordpressURL", "https://omegahat.wordpress.com/xmlrpc.php")
 }

categories = getCategoryList = getCategories =
function(login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getCategoryList", 0L, names(login), as.character(login))
  structure(sapply(ans, `[[`, "categoryId"), names = sapply(ans, `[[`, "categoryName"))
}

getPosts = getRecentPostTitles =
function(num = 100, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ...,
         .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getRecentPostTitles", as.integer(blogid),
                     names(login), as.character(login), as.integer(num), ...)
  makeIntoDataFrame(ans)
}

getTags =
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "wp.getTags", as.character(blogid),
                     names(login), as.character(login), ...)
}

getPostCategories =
function(post = 100, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.getPostCategories", as.integer(post), names(login), as.character(login))
}


getPost =
  #XXX match by name by looking up all the posts and matching.
function(postid, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  xml.rpc(.server, "metaWeblog.getPost", as.character(postid), names(login), as.character(login))
}

getPage =
  # ll = getPages()
function(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  #XXX conversion is not right.
   xml.rpc(.server, "wp.getPage", blogid, pageid, names(login), as.character(login), ...)
}


getPages =
  # ll = getPages()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPages", blogid, login, ..., .server = .server)
}

getPageList =
 # ll = getPageList()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPageList", blogid, login, ..., .server = .server)
}

getOptions =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getOptions", blogid, login, ..., .server = .server)
}

getPageStatusList =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPageStatusList", blogid, login, ..., asDataFrame = FALSE, .server = .server)
}

getPostStatusList =
 # ll = getOptions()
function(blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
 generalRequest("wp.getPostStatusList", blogid, login, ..., asDataFrame = FALSE, .server = .server)
}

generalRequest =
function(op, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., asDataFrame = TRUE, .server = getServerURL())
{
  ans = xml.rpc(.server, op, as.integer(blogid),
                     names(login), as.character(login), ...)
  if(asDataFrame) {
     makeIntoDataFrame(ans)
  } else
     ans
}


makeIntoDataFrame =
   # XXX have to make certain that we have the same variables.
function(els)
{
  if(length(els) == 0)
    return(data.frame())
  if(length(names(els)) && all(names(els) != ""))  # a single struct rather than an array of them
    els = list(els)

    # handle scalar variables and non-scalar variables separately.
    # A variable that is missing/empty in some elements is still scalar.
  vars = unique(unlist(lapply(els, names)))
  isScalar = vapply(vars, function(v) all(vapply(els, function(x) length(x[[v]]) <= 1 && is.atomic(x[[v]]), logical(1))),
                    logical(1))
  scalarVars = vars[ isScalar ]

  tmp = lapply(els, function(x) {
                       vals = structure(lapply(scalarVars, function(v) if(length(x[[v]])) x[[v]] else NA),
                                        names = scalarVars)
                       as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
                     })
  ans = do.call("rbind", tmp)
  if(is.null(ans))
    ans = data.frame(row.names = seq_along(els))

      # now go and grab the non-scalar values across the different elements
  if(any(!isScalar)) {
     ids = vars[!isScalar]
     ans[ids] = lapply(ids, function(i) lapply(els, `[[`, i))
  }

  ans
}

newCategory =
  # Problem with this. it only uses the first letter.
  # Also have to get the parent id in, etc.
  #
  # a = newCategory("xmlrpc")
  # cat = newCategory("tmp", "A temporary category that I will then delete")
  # length(categories())
  # deleteCategory(cat)
function(category, description = "???", slug = category, parent = 0L, blogid = 0,
          login = getOption("WordpressLogin", stop("need a login and password")),
           .server = getServerURL())
{
  catg = list(name = as.character(category), slug = as.character(slug),
              parent_id = as.integer(parent), description = as.character(description))
  ans = xml.rpc(.server, "wp.newCategory",
                   as.integer(blogid), names(login), as.character(login), catg)
  structure(ans, names = category, class = "WordPressCategoryId")
}


#
getUsersBlogs =
function(login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = xml.rpc(.server, "wp.getUsersBlogs", names(login), as.character(login))
  ans[[1]]
}


###########
newPage =
  #
  # wp.newPage(list(description = "This is some text that I posted from R", title = "Test from R"))
  #
function(content, publish = TRUE, blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")),
          method = "wp.newPage", .server = getServerURL())
{
  content = postContent(content)

  ans = xml.rpc(.server, method,
                    as.character(blogid), names(login), as.character(login), content, as.logical(publish))
  ans
}


newPost =
  #
  # wp.newPost(list(description = "This is some text that I posted from R", title = "Test from R"), blogId = "1345476")
  #
function(content, publish = TRUE, blogid = 0, login = getOption("WordpressLogin", stop("need a login and password")), .server = getServerURL())
{
  ans = newPage(content, publish, blogid, login, "metaWeblog.newPost", .server = .server)
  structure(ans, class = "WordpressPostId")
}


  # The categories should be an unnamed list with the ids of the category
  #  and we should create an XML-RPC array of these with each element a struct
  #  containing a single element named categoryId and the value  being the category identifier.
  # So if the caller gives us strings that are not numbers, lookup the category ids.
setGeneric("setPostCategories",
             function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")),
                       publish = TRUE, ..., .server = getServerURL())
           standardGeneric("setPostCategories"))

setMethod("setPostCategories", c(categories = "character"),
function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")),
           publish = TRUE, ..., .server = getServerURL())
{
   cats = matchCategory(categories, login, .server = .server)
   values = lapply(unname(cats), function(x) list(categoryId = as.character(x)))
   setPostCategories(postid, values, login, publish, ..., .server = .server)
})

setMethod("setPostCategories", c(categories = "numeric"),
function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")),
           publish = TRUE, ..., .server = getServerURL())
{
   values = lapply(as.character(categories), function(x) list(categoryId = x))
   setPostCategories(postid, values, login, publish, ..., .server = .server)
})

setMethod("setPostCategories", c(categories = "list"),
function(postid, categories, login = getOption("WordpressLogin", stop("need a login and password")), publish = TRUE, ..., .server = getServerURL())
{
  ans = xml.rpc(.server, "mt.setPostCategories", as.character(postid),
                       names(login), as.character(login), categories, ...)
  if(isTRUE(ans) && publish)
     publishPost(postid, login, .server = .server)
  else
     ans
})

publishPost =
 #
function(postid, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "mt.publishPost", as.character(postid),
                     names(login), as.character(login),  ...)
}

if(FALSE) {
 setPostCategories("54", "Web services")
 publishPost("54")
}



##############


uploadFile =
  #
  # uploadFile("RWordpress/R/wp.R")
  #
function(what, type = guessMIMEType(remoteName), blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")),
          remoteName = basename(what), overwrite = TRUE, ..., .server = getServerURL())
{
  if(inherits(what, "AsIs") || is.raw(what)) {
    if(missing(remoteName))
      stop("remoteName must be given when uploading content rather than a file")
    x = unclass(what)
    content = if(is.raw(x)) x
              else if(is.character(x)) charToRaw(enc2utf8(paste(x, collapse = "\n")))
              else serialize(x, NULL)
  } else {
    if(!file.exists(what))
      stop("no such file ", what)
    content = readBinaryFile(what)
  }

  info = list(name = remoteName, type = unname(as.character(type)), bits = content,
              overwrite = as.logical(overwrite))
  xml.rpc(.server, "wp.uploadFile", as.character(blogid),
                     names(login), as.character(login),  info, ...)

}

readBinaryFile =
function (filename)
{
    con = file(filename, "rb")
    on.exit(close(con))
    fs = file.info(filename)$size
    readBin(con, raw(fs), fs)
}


######

deletePost =
function(postid, login = getOption("WordpressLogin", stop("need a login and password")),
          publish = TRUE, appId = "RWordPress", ..., .server = getServerURL())
{
  xml.rpc(.server, "metaWeblog.deletePost", appId, as.character(postid),
                     names(login), as.character(login), as.logical(publish), ...)
}

deleteCategory =
function(categoryid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")),
          ..., .server = getServerURL())
{
  if(!inherits(categoryid, "WordPressCategoryId"))
     categoryid = matchCategory(categoryid, login, .server = .server)

  xml.rpc(.server, "wp.deleteCategory", as.character(blogid),
                     names(login), as.character(login), as.character(categoryid), ...)
}

matchCategory =
function(categories, login, catIds = getCategoryList(login, .server = .server), .server = getServerURL())
{

   i = pmatch(tolower(categories), tolower(names(catIds)), duplicates.ok = TRUE)
   if(any(is.na(i))) {
     miss = which(is.na(i))
     j = lapply(tolower(categories[miss]), function(x) {
                                          tmp = agrep(x, tolower(names(catIds)))
                                          if(length(tmp) > 1)
                                            warning(x, " matches ", paste(names(catIds)[tmp], collapse = ", "))
                                          tmp
                                        })
     if(any(lengths(j) != 1))
          # should create them possibly, but only after looking at agrep to see possible matches.
       stop("don't recognize category ", paste(categories[miss][lengths(j) != 1], collapse = ", "))

     i[miss] = unlist(j)
   }

   catIds[i]

}


deletePage =
  #
  #  pages = getPages()
  #  page = wp.newPage(list(description = "This is some text that I posted from R", title = "Test from R"))
  #  tmp = getPages()
  #  deletePage(getUsersBlogs()[[1]]$blogid, page)
  #  newPages = getPages()
  #  nrow(pages) == nrow(newPages)
  #
function(pageid, blogid = 0L, login = getOption("WordpressLogin", stop("need a login and password")), ..., .server = getServerURL())
{
  xml.rpc(.server, "wp.deletePage", as.character(blogid),
                     names(login), as.character(login),  as.character(pageid), ...)
}


editPost =
function(postid, content, login = getOption("WordpressLogin", stop("need a login and password")), publish = TRUE, appId = "RWordpress", ..., .server = getServerURL())
{
  content = postContent(content)

  xml.rpc(.server, "metaWeblog.editPost", as.character(postid),
                     names(login), as.character(login), content, as.logical(publish), ...)
}

#################

# http://www.sixapart.com/developers/xmlrpc/movable_type_api/

supportedMethods =
function(.server = getServerURL())
   xml.rpc(.server, "mt.supportedMethods")

supportedTextFilters =
function(.server = getServerURL())
   xml.rpc(.server, "mt.supportedTextFilters")



##################


postContent =
  # Normalize the content struct for metaWeblog.newPost/editPost and wp.newPage.
  # WordPress silently ignores 'categories' unless it is an array, so a single
  # category given as a string (e.g. via knitr::knit2wp) was previously dropped.
function(content)
{
  if(!is.list(content))
    content = list(description = content)

  for(id in intersect(c("description", "title", "mt_excerpt", "mt_text_more"), names(content)))
    content[[id]] = paste(as.character(content[[id]]), collapse = "\n")

  if("categories" %in% names(content))
    content$categories = I(as.character(unlist(content$categories)))
  if("mt_keywords" %in% names(content))
    content$mt_keywords = paste(as.character(unlist(content$mt_keywords)), collapse = ",")

  content
}
