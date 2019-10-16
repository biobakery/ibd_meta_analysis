rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "PROTECT"
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccc")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta_raw <- paste0("raw/", 
                   study, 
                   "/metadata/SupplementTable6.xlsx") %>% 
  readxl::read_xlsx(sheet = "SupplementTable6", skip = 2)
meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = study %>% as.character(),
    PMID = "30308161",
    subject_accession = SubjectID %>% as.character(),
    sample_accession = GID %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = NA_character_,
    study_accession_db = NA_character_,
    subject_accession_db = NA_character_,
    sample_accession_db = NA_character_,
    batch = NA_character_,
    sample_type = sampleType %>% 
      dplyr::recode("biopsy" = "biopsy",
                    "stool" = "stool",
                    .missing = NA_character_),
    body_site = sampleType %>% 
      dplyr::recode("biopsy" = "rectum",
                    "stool" = NA_character_,
                    .missing = NA_character_),
    body_site_additional = sampleType %>% 
      dplyr::recode("biopsy" = "rectum",
                    "stool" = NA_character_,
                    .missing = NA_character_),
    disease = "UC",
    control = NA_character_,
    L.cat = NA_character_,
    E.cat = MONTREAL_ORD %>% 
      dplyr::recode("Extensive or Pancolitis or Unasessable" = "E3",
                    "Left-sided colitis" = "E2",
                    "Proctosigmoiditis" = "E1",
                    .missing = NA_character_),
    B.cat = NA_character_,
    perianal = NA_character_,
    age = age %>% as.numeric,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat =   NA_character_,
    race = race %>% 
      dplyr::recode("White" = "white",
                    "Black" = "african_american",
                    "Asian" = "asian_pacific_islander",
                    "Native American" = "native_american",
                    "More than one" = "more_than_one",
                    "Unknown" = NA_character_,
                    .missing = NA_character_),
    gender = gender %>% 
      dplyr::recode("M" = "m",
                    "F" = "f",
                    .missing = NA_character_),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    calprotectin = CALPROTECTIN_WKall %>% 
      dplyr::recode("NA" = NA_character_) %>% 
      as.numeric(),
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = dplyr::case_when(
      antibiotics == "No" ~ "n",
      !is.na(antibiotics_time_to) & antibiotics_time_to <= 27  ~ "y",
      TRUE ~ "n"),
    antibiotics_supp = antibiotics_type %>% as.character(),
    immunosuppressants = NA_character_,
    immunosuppressants_supp = NA_character_,
    steroids = dplyr::case_when(
      collectionWeek == 0 ~ "n",
      collectionWeek == 4 & INITIAL_TRT_C4 %in% c("CS-Oral", "CS-IV") ~ "y",
      TRUE ~ NA_character_),
    steroids_supp = ifelse(collectionWeek == 4 & INITIAL_TRT_C4 %in% c("CS-Oral", "CS-IV"),
                           INITIAL_TRT_C4,
                           NA_character_),
    mesalamine_5ASA = dplyr::case_when(
      collectionWeek == 0 ~ "n",
      collectionWeek == 4 & INITIAL_TRT_C4 %in% "5ASA" ~ "y",
      TRUE ~ NA_character_),
    mesalamine_5ASA_supp = ifelse(collectionWeek == 4 & INITIAL_TRT_C4 %in% "5ASA",
                                  INITIAL_TRT_C4,
                                  NA_character_),
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = NA_integer_,
    time_point_supp = collectionWeek %>% as.character(),
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

# format time point into 1, 2, 3, ...
meta_curated <- meta_curated %>% 
  dplyr::mutate(time_point = as.numeric(time_point_supp)) %>% 
  dplyr::arrange(time_point) %>% 
  dplyr::group_by(subject_accession) %>% 
  dplyr::mutate(time_point = create_timepoint(time_point)) %>% 
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
