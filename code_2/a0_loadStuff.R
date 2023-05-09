library(tidyverse)
library(latex2exp)
library(fuzzyjoin)
library(RColorBrewer)
library(purrr)
library(PoissonBinomial) ### Generalized Poisson Binomial distribution
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))
SAVE_PLOT = TRUE #TRUE #FALSE
