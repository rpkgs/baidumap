#' Transform the query character to raw character
#' Take in query and city, return the informations
#' @param a character
#' @return raw character with %. It's used in get_place.
#' @examples
#' \dontrun{
#' url_character("北京")
#' # "%e5%8c%97%e4%ba%ac"
#' }
url_character <- function(x) {
  raw <- as.character(charToRaw(x))
  paste0("%", raw, collapse = "")
}

#' Get place from query
#' Take in query and city, return the informations
#' @param place the place you want to search
#' @param city define the city
#' @return a data frame contains name, longtitude, latitude and address, as well as teleplhone number, if exist.
#' @export get_place
#' @examples
#' \dontrun{
#' ## colleges in beijing
#' bj_college <- get_place("大学", "北京")
#' ## Mcdonald's in shanghai
#' sh_mcdonald <- get_place("麦当劳", "上海")
#' }
get_place <- function(place = NULL, city = "", page_size = 20,
                      pages = Inf, scope = 1, verbose = TRUE, map_ak = "") {
  map_ak %<>% check_mapkey()

  ## character
  place <- url_character(place)
  city <- url_character(city)

  getResult <- function(page = 0) {
    url_head <- "http://api.map.baidu.com/place/v2/search?ak="
    url <- sprintf(
      "%s%s&output=json&query=%s&page_size=%s&&page_num=%s&scope=%s&region=%s",
      url_head, map_ak, place, page_size, page - 1, scope, city
    )

    result <- GET(url) %>% content()
    result %<>% fromJSON2()
    total <- ifelse(is.null(result$total), 0, result$total)
    vars_common <- c("lat", "lng", "name", "address", "province", "city", "area", "detail", "telephone")

    data <- NULL
    if (total > 0) {
      data <- result$results %>%
        cbind(.[, 2], .[, -2]) %>%
        check_colnames(vars_common)
    }
    list(total = total, result = data)
  }

  i <- 1
  result <- getResult(i)

  total_page <- min(ceiling(result$total / 20), pages)
  if (verbose) cat("Get", result$total, "records,", total_page, "page.", "\n")
  if (verbose) cat("    Getting ", i, "th page", "\n")

  res <- list()
  res[[i]] <- result$result
  if (total_page > 1) {
    for (i in 2:total_page) {
      if (verbose) cat("    Getting ", i, "th page", "\n")
      res[[i]] <- getResult(i)$result
    }
  }
  cat("Done!", "\n")
  rbindlist(res)
  # do.call(rbind, res)
}

check_colnames <- function(d, names) {
  for (name in names) {
    if (is.null(d[[name]])) d[[name]] <- NA
  }
  d[, names]
}

fromJSON2 <- function(x) {
  if (!is.list(x)) x <- fromJSON(x)
  if (x$status != 0) {
    message(x$message)
    return(NULL)
  }
  x
}
