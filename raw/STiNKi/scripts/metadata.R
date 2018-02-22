library(tidyverse)
study <- "STiNKi"
source("scripts/source.R")
template <- read.csv('scripts/template.csv',
                     stringsAsFactors = F)

dir.create(paste0("processed/", study, "/metadata/"),
           recursive = T,
           showWarnings = F)
meta_raw_merged <- read_tsv(paste0("raw/", study, "/metadata/STiNKi_all.txt"),
                            col_names = F) %>% 
  filter(X6 == "16S",
         X7 == "STiNKi")
meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = "STiNKi",
         PMID = "28191884",
         sample_accession = X1,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", sample_accession),
         body_site = "stool"
  ) %>% select(template$col.name %>% one_of)

for(var in setdiff(template$col.name, colnames(meta_curated))) {
  meta_curated[[var]] <- NA
}
meta_curated <- meta_curated[, template$col.name]
write.table(meta_curated,
            file = paste0("processed/", study, "/metadata/metadata.txt"),
            quote = F,
            sep = "\t",
            row.names = F)
