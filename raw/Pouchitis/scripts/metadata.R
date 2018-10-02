rm(list = ls())
library(tidyverse)
source("scripts/misc/helpers.R")
study <- "Pouchitis"
template <- read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  filter(Project == study, Technology == "16S") %>% 
  select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, 
                "/metadata/Sample List Broad_pheno7_6_2012.xlsx") %>% 
  readxl::read_excel()
meta3 <- paste0("raw/", study, 
                "/metadata/MSH.txt") %>% 
  read_delim(" ")
meta_raw <- meta1 %>% 
  left_join(meta2, by = c("OriginalID" = "Sample ID")) %>% 
  left_join(meta3, by = c("OriginalID" = "DirkSampleID"))

meta_curated <- meta_raw %>% 
  mutate(
    dataset_name = "Pouchitis",
    study_accession = "Pouchitis",
    PMID = NA,
    subject_accession = DonorID,
    alternative_subject_accession = NA,
    sample_accession = GID,
    alternative_sample_accession = OriginalID,
    batch = SequencingRun,
    `16S_sample_accession` = GID,
    WGS_sample_accession = NA,
    sample_type = `Sample Location` %>% 
      recode(Pouch = "biopsy",
             PPI = "biopsy",
             Sigmoid = "biopsy",
             TI = "biopsy"),
    body_site = `Sample Location` %>% 
      recode(Pouch = "pouch",
             PPI = "PPI",
             Sigmoid = "sigmoid",
             TI = "TI"),
    disease = Diagnosis.y %>% 
      recode(CD = "CD",
             UC = "UC",
             IC = "IC",
             FAP = "FAP",
             HC = "control"),
    control = NA,
    IBD_subtype = diseasesubtype %>% 
      recode(iCD = "iCD",
             cCD = "cCD",
             CD = "CD",
             UC = "UC",
             IC = "IC",
             control = NA_character_),
    IBD_subtype_additional = NA,
    L.cat = disease_extent %>% 
      recode(L1 = "L1",
             L2 = "L2",
             L3 = "L3",
             .default = NA_character_),
    E.cat = disease_extent %>% 
      recode(E1 = "E1",
             E2 = "E2",
             E3 = "E3",
             .default = NA_character_),
    B.cat = B.cat %>% 
      recode(B1 = "B1",
             B2 = "B2",
             B3 = "B3",
             unknown = NA_character_),
    perianal = perinanal %>% 
      recode(yes = "y",
             no = "n",
             unknown = NA_character_),
    age = Age,
    age_at_diagnosis = Ageof_Diag,
    gender = Gender.y %>% 
      recode(Male = "m",
             Female = "f"),
    BMI = NA,
    alcohol = NA,
    smoke = smoking %>% 
      recode(Current = "current",
             Former = "former",
             Never = "never",
             unknown = NA_character_),
    country = NA,
    calprotectin = NA,
    PCDAI = NA,
    antibiotics = antibiotics %>% 
      recode(yes = "y",
             no = "n",
             unknown = NA_character_),
    antibiotics_supp = NA,
    immunosuppressants = immunosup %>% 
      recode(no = "n",
             unknown = NA_character_),
    immunosuppressants_supp = NA,
    steroids = steroids %>% 
      recode(yes = "y",
             no = "n",
             unknown = NA_character_),
    steroids_supp = NA,
    mesalamine = mesalamine %>% 
      recode(yes = "y",
             no = "n",
             unknown = NA_character_),
    mesalamine_supp = NA,
    biologics = biologics %>% 
      recode(yes = "y",
             no = "n",
             unknown = NA_character_),
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
