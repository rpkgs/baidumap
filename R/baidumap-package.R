#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    v <- packageVersion("baidumap")
    message("baidumap ", v)
    # message(Notification)
  }

  options(baidumap.key = "3sG4CyHRB2hDp6ifhWfQXq9OOySKdKPL")
}

Notification <- paste("Apply an application from here: http://lbsyun.baidu.com/apiconsole/key",
  "Then register you key by running `options(baidumap.key = 'xxx')`",
  sep = "\n"
)

check_mapkey <- function(map_ak) {
  if (map_ak == "" && is.null(getOption("baidumap.key"))) {
    stop(Notification)
  } else {
    map_ak <- ifelse(map_ak == "", getOption("baidumap.key"), map_ak)
  }
  map_ak
}
