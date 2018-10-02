rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "MucosalIBD"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  filter(Project == study, Technology == "16S") %>% 
  select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, "/metadata/",
                study, ".csv") %>% 
  read_csv %>% 
  mutate(
    subject_accession = `Attributes/Attribute/13/__text`,
    sample_accession = `Ids/Id/0/__text`,
    body_site = `Attributes/Attribute/7/__text`,
    age = `Attributes/Attribute/9/__text`,
    disease = `Attributes/Attribute/10/__text`,
    gender = `Attributes/Attribute/11/__text`
  ) %>% 
  select(subject_accession,
         sample_accession,
         body_site,
         age,
         disease,
         gender)
meta_raw <- meta1 %>% 
  left_join(meta2,
            by = c("DonorID" = "subject_accession"))

meta_curated <- meta_raw %>% 
  mutate(
    dataset_name = "MucosalIBD",
    study_accession = "PRJNA317429",
    PMID = "27699268",
    subject_accession = DonorID,
    alternative_subject_accession = NA,
    sample_accession = sample_accession,
    alternative_sample_accession = NA,
    batch = SequencingRun,
    `16S_sample_accession` = GID,
    WGS_sample_accession = NA,
    sample_type = body_site %>% 
      recode(`epithelium of ileum` = "biopsy"),
    body_site = body_site %>% 
      recode(`epithelium of ileum` = "ileum"),
    disease = disease %>% 
      recode(`Crohn's disease` = "CD",
             `Not IBD` = "control"),
    control = NA,
    IBD_subtype = NA,
    IBD_subtype_additional = NA,
    L.cat = NA,
    E.cat = NA,
    B.cat = NA,
    perianal = NA,
    age = age,
    age_at_diagnosis = NA,
    gender = gender %>% 
      recode(male = "m",
             female = "f"),
    BMI = NA,
    alcohol = NA,
    smoke = NA,
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
