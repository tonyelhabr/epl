
path_r_profile <- '~/.Rprofile'
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm('path_r_profile')

.library_silently <- function(...) {
  suppressWarnings(suppressPackageStartupMessages(base::library(...)))
}

.library_silently('tidyverse', warn.conflicts = TRUE)
.library_silently('rlang')
.library_silently('teplot')
rm('.library_silently')
filter <- dplyr::filter
select <- dplyr::select

invisible(R.utils::sourceDirectory(file.path('R'), recursive = FALSE))
# config <- config::get()
