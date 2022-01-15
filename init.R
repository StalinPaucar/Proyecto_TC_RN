# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinycssloaders", "modeest", "moments", "openxlsx",
                "tidyverse", "devtools", "tseriesChaos", "Metrics", "DescTools",
                "forecast", "tools", "RSNNS", "quantmod", "ggplot2")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
library(devtools)
devtools :: install_github ("antoniofabio/RTisean", subdir = "RTisean")