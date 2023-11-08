# List of required packages
required_packages <- c(
  "tidyverse",
  "flextable",
  "ggplot2",
  "iNEXT",
  "vegan",
  "rredlist",
  "rcites",
  "treemap",
  "shiny",
  "shinydashboard",
  "data.validator",
  "assertr",
  "dplyr",
  "DT",
  "rmarkdown",
  "bslib",
  "kableExtra",
  "shinybusy",
  "markdown",
  "bookdown",
  "SpadeR"
)

# Function to install and load a package if not already installed
install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

# Install and load required packages
for (package in required_packages) {
  install_and_load_package(package)
}
