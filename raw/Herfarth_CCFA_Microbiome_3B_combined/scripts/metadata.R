rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "Herfarth_CCFA_Microbiome_3B_combined"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/2538_20180418-110548.txt") %>% 
  read_tsv # downloaded from Qiita
meta2 <- paste0("raw/", 
                study, 
                "/metadata/ebi_sample_accessions_study_2538.tsv") %>% 
  read_tsv # downloaded from Qiita
meta3 <- paste0("raw/", 
                study, 
                "/metadata/PRJEB23009.txt") %>% 
  read_tsv # downloaded from EBI
meta_raw <- meta1 %>% 
  left_join(meta2) %>% 
  left_join(meta3, by = c("sample_accession" = "secondary_sample_accession"))
meta_raw <- meta_raw %>% 
  filter(sample_accession != "None",
         sample_type != "control blank")
meta_curated <- meta_raw %>% 
  mutate(dataset_name = "Herfarth_CCFA_Microbiome_3B_combined",
         study_accession = "PRJEB23009",
         PMID = "29055911",
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
         disease = ibd %>% 
           recode("Crohns" = "CD",
                  "Control" = "control"),
         control = ibd %>% 
           recode("CD" = "not applicable",
                  "UC" = "not applicable",
                  "Control" = "healthy"),
         IBD_subtype = NA,
         IBD_subtype_additional = NA,
         L.cat = NA,
         E.cat = NA,
         B.cat = NA,
         perianal = NA,
         age = age %>% as.numeric,
         age_at_diagnosis = NA,
         gender = sex %>% 
           recode("male" = "m",
                  "female" = "f",
                  "Missing: Not provided" = NA_character_),
         BMI = body_mass_index %>% 
           as.numeric,
         alcohol = alcohol_frequency %>% 
           recode("Regularly" = "y",
                  "Occasionally" = "y",
                  "Rarely" = "n",
                  "Never" = "n",
                  "Missing: Not provided" = NA_character_),
         smoke = smoker_history %>% 
           recode("never" = "never",
                  "former, 26 years" = "former",
                  "former, 22 years" = "former",
                  "y" = "current",
                  "former, 10 years" = "former",
                  "Never" = "never",
                  "Missing: Not provided" = NA_character_),
         country = NA,
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
         time_point = collection_date,
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
