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
pacman::p_install_gh("coolbutuseless/ggpattern")

fct_reorg <- function(fac, ...) {
  fct_recode(fct_relevel(fac, ...), ...)
}
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}