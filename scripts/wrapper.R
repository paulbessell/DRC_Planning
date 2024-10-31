packages <- c("rmarkdown", "knitr", "sf", "spdep", "tidyverse", "readr", "readxl", "DT", "leaflet", "flextable", "leaflet.extras")
install.packages(setdiff(packages, rownames(installed.packages())))

library(rmarkdown)
library(knitr)

rmarkdown::render("Markdown/ShampaOutput_20240401.Rmd", params = "ask")
system2("open", "Markdown/ShampaOutput_20240401.html")
