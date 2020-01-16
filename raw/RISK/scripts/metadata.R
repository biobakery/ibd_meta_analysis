rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "RISK"
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccccc")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- readr::read_tsv("data/metadata_raivo/sample2project_common.txt",
                         col_types = 
                           readr::cols_only(GID = readr::col_character(),
                                            Project = readr::col_character(),
                                            DonorID = readr::col_character(),
                                            OriginalID = readr::col_character(),
                                            SequencingRun = readr::col_character(),
                                            Technology = readr::col_character())) %>% 
  dplyr::filter(Project == study, Technology == "16S") %>% 
  dplyr::select(-Technology)

meta2 <- paste0("raw/", study, "/metadata/",
                study, "_common.txt") %>% 
  readr::read_tsv(col_types = "ccccccd")

meta3 <- paste0("raw/", study, "/metadata/",
                study, "_special.txt") %>% 
  readr::read_tsv(col_types = "ccccdcc")

meta4 <- paste0("raw/", study, "/metadata/RISK_metadata.tsv") %>% 
  readr::read_tsv(col_names = FALSE,
                  col_types = readr::cols(.default = readr::col_character())) %>% 
  t() %>% 
  set_colnames(.[1, ]) %>% 
  {tibble::as_tibble(.[-1, ])} %>% 
  dplyr::rename(Diagnosis_followup = Diagnosis)

meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("OriginalID", "DonorID", "Project")) %>% 
  dplyr::left_join(meta3, by = c("OriginalID", "DonorID", "Project")) %>% 
  dplyr::left_join(meta4, by = c("GID" = "ID"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "RISK",
    PMID = "24629344",
    subject_accession = DonorID %>% as.character(),
    sample_accession = GID %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = NA_character_,
    study_accession_db = NA_character_,
    subject_accession_db = NA_character_,
    sample_accession_db = NA_character_,
    batch = SequencingRun %>% as.character(),
    sample_type = Location %>% 
      dplyr::recode("Stool" = "stool",
                    "Terminal Ileum" = "biopsy",
                    "Rectum" = "biopsy"),
    body_site = Location %>% 
      dplyr::recode("Stool" = NA_character_,
                    "Terminal Ileum" = "ileum",
                    "Rectum" = "rectum"),
    body_site_additional = Location %>% 
      dplyr::recode("Stool" = NA_character_,
                    "Terminal Ileum" = "terminal ileum",
                    "Rectum" = "rectum"),
    disease = Diagnosis %>% 
      dplyr::recode("CD" = "CD",
                    "Control" = "control"),
    control = Diagnosis %>% 
      dplyr::recode("CD" = NA_character_,
                    "Control" = "nonIBD"),
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = dplyr::case_when(
      disease == "CD" & STRCTR_STATUS == "B2" ~ "B2",
      disease == "CD" & INTPEN_STATUS == "B3" ~ "B3",
      disease == "CD" ~ "B1",
      TRUE ~ NA_character_),
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = NA_character_,
    race = Race %>% 
      dplyr::recode("caucasian" = "white",
                    "african" = "african_american",
                    "other" = NA_character_,
                    .missing = NA_character_),
    gender = Gender %>% 
      dplyr::recode("Male" = "m",
                    "Female" = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    calprotectin = NA_real_,
    PCDAI = PCDAI %>% as.numeric(),
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = Antibiotics %>% 
      dplyr::recode("Yes" = "y",
             "No" = "n"),
    antibiotics_supp = NA_character_,
    immunosuppressants = NA_character_,
    immunosuppressants_supp = NA_character_,
    steroids = NA_character_,
    steroids_supp = NA_character_,
    mesalamine_5ASA = NA_character_,
    mesalamine_5ASA_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = 1,
    time_point_supp = NA_character_,
    family = NA_character_,
    family_supp = NA_character_,
    method_MBX = NA_character_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())
# Can do this because new-onset cohort
meta_curated <- meta_curated %>% 
  dplyr::group_by(subject_accession) %>% 
  dplyr::mutate(age_at_diagnosis = ifelse(any(!is.na(age)),
                                          min(age, na.rm = TRUE),
                                          NA),
                age_at_diagnosis.cat =   
                  dplyr::case_when(age_at_diagnosis <= 16 ~ "A1",
                                   age_at_diagnosis <= 40 ~ "A2",
                                   age_at_diagnosis > 40 ~ "A3",
                                   is.na(age_at_diagnosis) ~ NA_character_)) %>% 
  dplyr::ungroup()

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
