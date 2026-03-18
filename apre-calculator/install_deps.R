# Run this once to install all required packages
pkgs <- c("shiny", "bslib", "ggplot2", "dplyr")
install.packages(pkgs[!pkgs %in% installed.packages()[, "Package"]])
