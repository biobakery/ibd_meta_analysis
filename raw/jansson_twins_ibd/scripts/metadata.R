rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "jansson_twins_ibd"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta_raw <- paste0("raw/", 
                study, 
                "/metadata/1070_20170412-111709.txt") %>% 
  read_tsv # downloaded from Qiita
meta_curated <- meta_raw %>% 
  mutate(dataset_name = study,
         study_accession = study,
         PMID = "28191884",
         subject_accession = host_subject_id,
         alternative_subject_accession = NA,
         sample_accession = sample_name,
         alternative_sample_accession = anonymized_name,
         batch = NA,
         `16S_sample_accession` = sample_name %>% 
           gsub(".", "_", ., fixed = TRUE),
         WGS_sample_accession = NA,
         sample_type = body_site %>% 
           recode("UBERON:feces" = "stool",
                  "UBERON:colonic mucosa" = "biopsy",
                  "UBERON:ileal mucosa" = "biopsy"),
         body_site = body_site %>% 
           recode("UBERON:feces" = "stool",
                  "UBERON:colonic mucosa" = "colon",
                  "UBERON:ileal mucosa" = "ileum"),
         disease = disease_stat2 %>% 
           recode("colonic Crohn's disease" = "CD",
                  "Ileal and colonic Crohn's disease" = "CD",
                  "ileal Crohn's disease" = "CD",
                  "ulcerative colitis-LS" = "UC",
                  "Healthy" = "control"),
         control = NA,
         IBD_subtype = disease_stat2 %>% 
           recode("colonic Crohn's disease" = "cCD",
                  "Ileal and colonic Crohn's disease" = "cCD",
                  "ileal Crohn's disease" = "iCD",
                  "ulcerative colitis-LS" = "UC",
                  "Healthy" = NA_character_),
         IBD_subtype_additional = NA,
         L.cat = NA,
         E.cat = NA,
         B.cat = NA,
         perianal = NA,
         age = age,
         gender = sex %>% 
           recode("male" = "m",
                  "female" = "f"),
         BMI = NA,
         alcohol = NA,
         smoke = NA,
         country = geo_loc_name,
         calprotectin = NA,
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
         time_point = NA,
         family = pair_number,
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
