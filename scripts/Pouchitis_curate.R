setwd("/n/hutlab11_nobackup/users/syma/ibd_meta_analysis/")
source("scripts/source.R")
study <- "Pouchitis"

template <- read.csv('data/template.csv',
                     stringsAsFactors = F)
meta_raw_merged <- read.table(paste0("raw/", study, "/metadata_raw_merged.txt"),
                              header = T,
                              sep = '\t',
                              stringsAsFactors = F,
                              check.names = F)
meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = "PRJNA269954",
         PMID = "25887922",
         sample_accession = sample_accession,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", Patient_ID),
         body_site = body_site %>% 
           recode("Pouch" = "pouch",
                  "PPI" = "PPI"),
         sample_type = "biopsy",
         antibiotics_usage = Antibiotics %>% 
           recode("yes" = "y",
                  "no" = "n"),
         antibiotics_family = NA
  ) %>% select(template$col.name %>% one_of)

for(var in setdiff(template$col.name, colnames(meta_curated))) {
  meta_curated[[var]] <- NA
}
meta_curated <- meta_curated[, template$col.name]
write.table(meta_curated,
            file = paste0("curated/", study, ".txt"),
            quote = F,
            sep = "\t",
            row.names = F)
