setwd("/n/hutlab11_nobackup/users/dshung/ibd_MA/")
source("scripts/source.R")
study <- "PRISM_WGS"

template <- read.csv('data/template.csv',
                     stringsAsFactors = F)
#meta_raw_merged <- read.table(paste0("raw/", study, "/metadata_raw_merged.txt"),
#                              header = T,
#                              sep = '\t',
#                              stringsAsFactors = F,
#                              check.names = F)


meta_raw_merged <- read.table(paste0("/Volumes/HDD_MAC/PhD_4Feb2014/_MICROBIOME/_IBD_MA/Clone_Bitbucket/ibd_meta_analysis/raw/PRISM_WGS/metadata/metadata_raw.txt"),
                              header = T,
                              sep = '\t',
                              stringsAsFactors = F,
                              check.names = F)


meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = Gnum,
         PMID = "224461721",
         sample_accession = ID,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", Patient_ID),
         body_site = Pouch_PPI %>% 
           recode("Pouch" = "pouch",
                  "PPI" = "PPI"),
         age = Age,
         body_site = Location,
         sample_type = 
         disease = Diagnosis,
         gender = Sex,
         calprotectin = Fecal.Calprotectin,
         sample_type = "biopsy",
         antibiotics_usage = antibiotic %>% 
           recode("yes" = "Yes",
                  "no" = "No"),
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

