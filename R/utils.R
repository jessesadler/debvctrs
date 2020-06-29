## Utilities ##

#' vctrs ifelse function
#'
#' ifelse function using vctrs to replace dplyr::if_else()
#' See https://vctrs.r-lib.org/articles/stability.html#ifelse
#' for code and explanation.
#'
#' @keywords internal

if_else <- function(test, yes, no) {
  vec_assert(test, logical())
  c(yes, no) %<-% vec_cast_common(yes, no)
  c(test, yes, no) %<-% vec_recycle_common(test, yes, no)

  out <- vec_init(yes, vec_size(yes))
  vec_slice(out, test) <- vec_slice(yes, test)
  vec_slice(out, !test) <- vec_slice(no, !test)

  out
}
