

# Getting error in `corrr::correlate()` ('Error: `i` must have one dimension, not 2.') when `diag()` is being used, so doing something similar instead
as_cordf <- function(x, diagonal = NA) {
  if (methods::is(x, 'cor_df')) {
    warning('x is already a correlation data frame.')
    return(x)
  }
  diag(x) <- diagonal
  x <- tibble::as_tibble(x)
  if (ncol(x) != nrow(x)) {
    stop('Input object x is not a square. ', 'The number of columns must be equal to the number of rows.')
  }
  # diag(x) <- diagonal
  x <- corrr::first_col(x, names(x))
  class(x) <- c('cor_df', class(x))
  x
}
library(corrr)
utils::assignInNamespace('as_cordf', as_cordf, ns = 'corrr')
