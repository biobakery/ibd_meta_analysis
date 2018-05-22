rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "Jansson_Lamendella_Crohns"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)


meta_raw <- paste0("raw/", 
                   study, 
                   "/metadata/1629_20180101-113841.txt") %>% 
  read_tsv
meta_raw %>% 
  describe %>% 
  write_csv(paste0("processed/", 
                   study, 
                   "/metadata/1629_20180101-113841_summary.csv"))
meta_curated <- meta_raw %>% 
  mutate(dataset_name = study,
         study_accession = "ERP020401",
         PMID = "28191884",
         subject_accession = host_subject_id,
         sample_accession = sample_accession,
         sampleID = paste(study_accession, 
                          subject_accesion,
                          sample_accession, sep = ":"),
         sample_type = body_site %>% 
           recode("UBERON:feces" = "stool"),
         body_site = body_site %>% 
           recode("UBERON:feces" = "stool"),
         disease = diagnosis_full %>% 
           recode("CD" = "CD",
                  "UC" = "UC",
                  "HC" = "control", 
                  "CC" = "control",
                  "LC" = "control"),
         control = diagnosis_full %>% 
           recode("CD" = "not applicable",
                  "UC" = "not applicable",
                  "HC" = "healthy",
                  "CC" = "collagenous colitis",
                  "LC" = "lymphocytic colitis"),
         IBD_subtype = ibd_subtype %>% 
           recode("CCD" = "cCD", 
                  "ICD_nr" = "iCD",
                  "ICD_r" = "iCD",
                  "UC" = "UC",
                  "LC" = "not applicable",
                  "CC" = "not applicable",
                  "HC" = "not applicable"),
         IBD_subtype_additional = ibd_subtype %>% 
           recode("CCD" = "not applicable", 
                  "ICD_nr" = "iCD non resection",
                  "ICD_r" = "iCD resection",
                  "UC" = "not applicable",
                  "LC" = "not applicable",
                  "CC" = "not applicable",
                  "HC" = "not applicable"),
         L.cat = cd_location %>% 
           recode("Ileal (L1)" = "L1",
                  "Colonic (L2)" = "L2",
                  "Ileocolonic (L3)" = "L3",
                  "Ileal and Upper-GI (L1+L4)" = "L1+L4",
                  "Ileocolonic and Upper-GI (L3+L4)" = "L3+L4",
                  "not applicable" = "not applicable"),
         E.cat = uc_extent %>% 
           recode("Proctitis (E1)" = "E1",
                  "Left sided (E2)" = "E2",
                  "Extensive (E3)" = "E3",
                  "not applicable" = "not applicable"),
         B.cat = cd_behavior %>% 
           recode("Non-stricturing, non-penetrating (B1)" = "B1",
                  "Stricturing (B2)" = "B2",
                  "Penetrating (B3)" = "B3",
                  "not applicable" = "not applicable"),
         perianal = perianal_disease %>% 
           recode("yes" = "y",
                  "no" = "n",
                  "not applicable" = "not applicable"),
         age = NA,
         gender = sex %>% 
           recode("male" = "m",
                  "female" = "f"),
         BMI = bmi %>% 
           recode("missing: not provided" = "NA") %>% 
           as.numeric,
         alcohol = NA,
         smoke = NA,
         country = geo_loc_name,
         calprotectin = calprotectin %>% 
           recode("not applicable" = "-1",
                  "not collected" = "NA") %>% 
           as.numeric,
         antibiotics_usage = NA,
         antibiotics_family = NA,
         time_point = timepoint,
         family = NA,
         DNA_extraction_kit = NA,
         sequencing_platform = NA,
         number_reads = NA,
         number_bases = NA,
         minimum_read_length = NA,
         median_read_length = NA
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
