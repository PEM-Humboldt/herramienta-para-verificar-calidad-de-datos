if (!require("renv")) {
  install.packages("renv")
}
library("renv")
renv::restore(clean = TRUE, prompt = FALSE)
