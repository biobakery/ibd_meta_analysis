rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "Jansson_Lamendella_Crohns"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/1629_20180101-113841.txt") %>% 
  read_tsv # downloaded from Qiita
meta2 <- paste0("raw/", 
                study, 
                "/metadata/ebi_sample_accessions_study_1629.tsv") %>% 
  read_tsv # downloaded from Qiita
meta3 <- paste0("raw/", 
                study, 
                "/metadata/PRJEB18471.txt") %>% 
  read_tsv # downloaded from EBI
meta_raw <- meta1 %>% 
  left_join(meta2) %>% 
  left_join(meta3, by = c("sample_accession" = "secondary_sample_accession"))

meta_curated <- meta_raw %>% 
  mutate(dataset_name = study,
         study_accession = "PRJEB18471",
         PMID = "28191884",
         subject_accession = host_subject_id,
         alternative_subject_accession = NA,
         sample_accession = sample_accession,
         alternative_sample_accession = sample_accession.y,
         batch = NA,
         `16S_sample_accession` = run_accession,
         WGS_sample_accession = NA,
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
                  "LC" = NA_character_,
                  "CC" = NA_character_,
                  "HC" = NA_character_),
         IBD_subtype_additional = ibd_subtype %>% 
           recode("CCD" = "not applicable", 
                  "ICD_nr" = "iCD non resection",
                  "ICD_r" = "iCD resection",
                  "UC" = NA_character_,
                  "LC" = NA_character_,
                  "CC" = NA_character_,
                  "HC" = NA_character_),
         L.cat = cd_location %>% 
           recode("Ileal (L1)" = "L1",
                  "Colonic (L2)" = "L2",
                  "Ileocolonic (L3)" = "L3",
                  "Ileal and Upper-GI (L1+L4)" = "L1+L4",
                  "Ileocolonic and Upper-GI (L3+L4)" = "L3+L4",
                  "not applicable" = NA_character_),
         E.cat = uc_extent %>% 
           recode("Proctitis (E1)" = "E1",
                  "Left sided (E2)" = "E2",
                  "Extensive (E3)" = "E3",
                  "not applicable" = NA_character_),
         B.cat = cd_behavior %>% 
           recode("Non-stricturing, non-penetrating (B1)" = "B1",
                  "Stricturing (B2)" = "B2",
                  "Penetrating (B3)" = "B3",
                  "not applicable" = NA_character_),
         perianal = perianal_disease %>% 
           recode("yes" = "y",
                  "no" = "n",
                  "not applicable" = NA_character_),
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
           recode("not applicable" = "NA",
                  "not collected" = "NA") %>% 
           as.numeric,
         PCDAI = NA,
         antibiotics = NA,
         antibiotics_supp = NA,
         immunosuppressants = NA,
         immunosuppressants_supp = NA,
         steroids = NA,
         steroids_supp = NA,
         mesalamine = NA,
         mesalamine_supp = NA,
         biologics = NA,
         biologics_supp = NA,
         time_point = timepoint,
         family = NA,
         DNA_extraction_kit = NA,
         sequencing_platform = NA,
         number_reads = NA,
         number_bases = NA,
         minimum_read_length = NA,
         median_read_length = NA
) %>% select(template$col.name %>% one_of)

meta_curated <- meta_curated[, template$col.name]
write.table(meta_curated,
            file = paste0("processed/", study, "/metadata/metadata.txt"),
            quote = F,
            sep = "\t",
            row.names = F)
