# RWordPress

Post to a WordPress blog from R: create and edit posts and pages, upload images, and manage categories.

This is an updated version of Duncan Temple Lang's RWordPress that works with current versions of R (tested on R 4.6). It no longer needs the old `XMLRPC` package, so it installs with no extra steps.

## 1. Install (one time)

```r
install.packages("remotes")
remotes::install_github("kartikeytilavat6819-bot/RWordPress")
```

## 2. Get a password to use from R

Use an **application password**, not your normal WordPress password. You can delete it at any time without affecting your account.

- **WordPress.com:** go to <https://wordpress.com/me/security>, turn on **Two-Step Authentication** if it is off, then create one under **Application Passwords**. You need this if you log in with Google or Apple, because those accounts have no WordPress password.
- **Your own WordPress site:** in the dashboard go to **Users → Profile → Application Passwords**, type a name such as "R" and click **Add New Application Password**.

Copy the password it shows you. It is shown only once.

## 3. Tell R your blog, email and password

Replace the three values with your own. Use your WordPress **username** as the login. Your **email address** usually works too; if it is rejected, try your username.

```r
library(RWordPress)

options(
  WordpressURL   = "https://YOURBLOG.wordpress.com/xmlrpc.php",   # your blog address + /xmlrpc.php
  WordpressLogin = c("you@example.com" = "abcd efgh ijkl mnop")    # c("email or username" = "application password")
)

getUsersBlogs()   # should print your blog's name. If it does, you're connected.
```

**Keep your password out of your scripts (recommended).** Run `usethis::edit_r_environ()` (or open the file `~/.Renviron` in any editor) and add these lines:

```
WP_URL=https://YOURBLOG.wordpress.com/xmlrpc.php
WP_USER=you@example.com
WP_PASS=abcd efgh ijkl mnop
```

Restart R. Then every script can start with:

```r
library(RWordPress)
options(WordpressURL = Sys.getenv("WP_URL"),
        WordpressLogin = setNames(Sys.getenv("WP_PASS"), Sys.getenv("WP_USER")))
```

Never share your `.Renviron` file, and never commit it to GitHub.

## 4. Use it

```r
# Save a post as a private draft (publish = FALSE): nobody else can see it
id <- newPost(list(title = "Hello from R",
                   description = "<p>Written in R.</p>",
                   categories = "R",
                   mt_keywords = c("rstats", "data")),
              publish = FALSE)

getPost(id)                                    # read it back
editPost(id, list(title = "New title", description = "<p>Updated.</p>"), publish = FALSE)
publishPost(id)                                # make it public when you're ready
deletePost(id)                                 # moves it to the trash

uploadFile("plot.png")$url                     # upload an image, get its web address
getPosts(num = 10)                             # your recent posts
getCategories()                                # your categories
```

With `publish = FALSE` a post stays a **draft**. Drafts are not public and do not appear in the WordPress.com Reader or anyone's feed.

### Turn an R Markdown file into a post

```r
knitr::opts_knit$set(upload.fun = function(file) RWordPress::uploadFile(file)$url)
knitr::knit2wp("my-post.Rmd", title = "My analysis", categories = "R", publish = FALSE)
```

## Problems?

| Message | What it means |
|---|---|
| `faultCode: 403 ... Incorrect username or password` | Wrong login, or you need an application password (see step 2). |
| `HTTP error 403` or `HTTP error 405` | Your web host or a security plugin has turned off XML-RPC. Ask your host to allow `xmlrpc.php`. |
| `Could not resolve host` | Check the blog address in `WordpressURL`. |
