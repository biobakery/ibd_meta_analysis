library(tidyverse)
library(knitr)

## Run through all the project specific scripts -------------------------------
# Update some infor in the raw sample2projet file
source("scripts/update_s2p.r")

# Run metadata parsing scripts
scripts = dir("scripts/", pattern = "^project.*")

for (script in scripts) {
  cat(sprintf("Running %s\n", script))
  source(sprintf("scripts/%s", script))  
}

# Join common metadata with sample2projects
source("scripts/merge_annotations.r")

## Re-knit README
# Open README/README.Rmd in RStudio and run through
# knit("README/README.Rmd")
##

