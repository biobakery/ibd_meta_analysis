library(tidyverse)
sample_reads <- read_tsv("raw/Misc/reads.txt", col_names = F) %>% 
  mutate(X2 = gsub("/", "", X2, fixed = T))
sample_stiched_reads <- read_tsv("raw/Misc/stiched_reads.txt", col_names = F)%>% 
  mutate(X2 = gsub("/", "", X2, fixed = T))
sample_all <- read_tsv("raw/Misc/sample2project.txt", col_names = F)
