# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinycssloaders", "modeest", "moments", "openxlsx",
                "tidyverse", "tseriesChaos", "Metrics", "DescTools",
                "forecast", "tools", "RSNNS", "quantmod", "ggplot2")
# install from local archive
install.packages("localpkgs/RTisean.tar.gz", repos=NULL, type="source")
install.packages("localpkgs/Rnlminb2_2110.79.tar.gz", repos=NULL, type="source")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

