rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "BIDMC-FMT"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  filter(Project == study, Technology == "16S") %>% 
  select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, "/metadata/",
                study, "_common.txt") %>% 
  read_tsv
meta3 <- paste0("raw/", study, "/metadata/",
                study, "_special.txt") %>% 
  read_tsv
meta_raw <- meta1 %>% 
  left_join(meta2) %>% 
  left_join(meta3)

meta_curated <- meta_raw %>% 
  mutate(
    dataset_name = "BIDMC-FMT",
    study_accession = "BIDMC-FMT",
    PMID = NA,
    subject_accession = DonorID,
    alternative_subject_accession = NA,
    sample_accession = GID,
    alternative_sample_accession = NA,
    batch = SequencingRun,
    `16S_sample_accession` = GID,
    WGS_sample_accession = NA,
    sample_type = Location %>% 
      recode(Stool = "stool"),
    body_site = Location %>% 
      recode(Stool = "stool"),
    disease = Diagnosis %>% 
      recode(CD = "CD"),
    control = NA,
    IBD_subtype = NA,
    IBD_subtype_additional = NA,
    L.cat = DiseaseLocation,
    E.cat = NA,
    B.cat = NA,
    perianal = NA,
    age = Age,
    gender = Gender %>% 
      recode(Male = "m",
             Female = "f"),
    BMI = NA,
    alcohol = NA,
    smoke = NA,
    country = NA,
    calprotectin = NA,
    antibiotics = Antibiotics %>% 
      recode(Yes = "y",
             No = "n"),
    antibiotics_supp = NA,
    immunosuppressants = Immunosuppressants %>% 
      recode(Yes = "y",
             No = "n"),
    immunosuppressants_supp = NA,
    steroids = Steroids %>% 
      recode(Yes = "y",
             No = "n"),
    steroids_supp = NA,
    time_point = NA,
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
