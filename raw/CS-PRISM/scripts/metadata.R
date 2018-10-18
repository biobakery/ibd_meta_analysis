rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "CS-PRISM"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- readr::read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  dplyr::filter(Project == study, Technology == "16S") %>% 
  dplyr::select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, "/metadata/",
                study, "_common.txt") %>% 
  readr::read_tsv()
meta3 <- paste0("raw/", study, "/metadata/",
                study, "_special.txt") %>% 
  readr::read_tsv()
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2) %>% 
  dplyr::left_join(meta3)

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "CS-PRISM",
    study_accession = "CS-PRISM",
    PMID = "23013615",
    subject_accession = DonorID %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = GID %>% as.character(),
    alternative_sample_accession = NA_character_,
    batch = SequencingRun %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_type = Location %>% 
      dplyr::recode("Stool" = "stool"),
    body_site = Location %>% 
      dplyr::recode("Stool" = "stool"),
    disease = Diagnosis %>% 
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "Control" = "control"),
    control = Diagnosis %>% 
      dplyr::recode("CD" = NA_character_,
                    "UC" = NA_character_,
                    "Control" = "HC"),
    IBD_subtype = NA_character_,
    IBD_subtype_additional = NA_character_,
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_c = NA_character_,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis_c = NA_character_,
    gender = Gender %>% 
      dplyr::recode(Male = "m",
                    Female = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    site = "MGH",
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    antibiotics = Antibiotics %>% 
      dplyr::recode(Yes = "y",
                    No = "n"),
    antibiotics_supp = NA_character_,
    immunosuppressants = Immunosuppressants %>% 
      dplyr::recode(Yes = "y",
                    No = "n"),
    immunosuppressants_supp = NA_character_,
    steroids = Steroids %>% 
      dplyr::recode(Yes = "y",
                    No = "n"),
    steroids_supp = NA_character_,
    mesalamine = NA_character_,
    mesalamine_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = NA_character_,
    time_point_supp = NA_character_,
    family = NA_character_,
    family_supp = NA_character_,
    extraction_kit_16S = NA_character_,
    sequencing_platform_16S = NA_character_,
    number_reads_16S = NA_integer_,
    number_bases_16S = NA_integer_,
    minimum_read_length_16S = NA_integer_,
    median_read_length_16S = NA_integer_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}