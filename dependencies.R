library(pacman)
pacman::p_load(
  tidyr,
  janitor,
  dplyr,
  ggplot2,
  forcats,
  stringr,
  ggtext,
  shades,
  tidytext,
  lubridate,
  forcats,
  xtable
)
data("stop_words")
# pacman::p_install_gh("coolbutuseless/ggpattern")

fct_reorg = function(fac, ...) {
  fct_relevel(fct_recode(fac, ...), names(rlang::dots_list(...)))
}
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}