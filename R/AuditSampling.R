.onLoad <- function(libname, pkgname) {
  #loadDependentLibraries()
  required_packages <- c("tidyverse", "readxl", "ggplot2", "shiny", "shinyjs")

  for (pkg in required_packages) {
    suppressMessages({
      library(pkg, character.only = TRUE)
    })

  }


}
