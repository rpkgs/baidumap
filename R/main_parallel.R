fix_null <- function(x) {
  I_bad <- which(sapply(x, length) == 0)
  # I_bad = which(is.null(x))
  if (length(I_bad) > 0) x[I_bad] <- NA
  x
}

#' @importFrom parallel makeCluster stopCluster detectCores parLapply parApply
#' @importFrom progress progress_bar
parLapply2 <- function(X, fun, ..., .parallel = FALSE, .rbind = TRUE) {
  if (.parallel) {
    cl <- makeCluster(getOption("cl.cores", detectCores() * 0.8))
    on.exit(stopCluster(cl))

    ans <- parLapply(cl, X, fun, ...)
  } else {
    n <- length(X)
    pb <- progress_bar$new(
      format = "  downloading [:bar] :current/:total eta: :eta",
      total = n, clear = FALSE, width = 80
    )
    fun2 <- function(...) {
      pb$tick()
      fun(...)
    }
    ans <- lapply(X, fun2, ...)
  }
  if (.rbind) ans %<>% do.call(rbind, .)
  ans
}

parApply2 <- function(X, fun, ..., .parallel = FALSE, .rbind = TRUE) {
  if (.parallel) {
    cl <- makeCluster(getOption("cl.cores", detectCores() * 0.8))
    on.exit(stopCluster(cl))

    ans <- parApply(cl, X, MARGIN = 1, FUN = fun, ...)
  } else {
    n <- nrow(X)
    pb <- progress_bar$new(
      format = "  downloading [:bar] :current/:total eta: :eta",
      total = n, clear = FALSE, width = 80
    )
    fun2 <- function(i, ...) {
      pb$tick()
      fun(X[i, ], ...)
    }
    ans <- lapply(1:nrow(X), fun2, ...)
  }
  if (.rbind) ans %<>% do.call(rbind, .)
  ans
}
