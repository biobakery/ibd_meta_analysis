rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "HMP2"
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccccc")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/samples_16s.csv") %>% 
  readr::read_csv(col_types = "c")
meta2 <- paste0("raw/", 
                study, 
                "/metadata/hmp2_metadata_2018-08-20.csv") %>% 
  readr::read_csv(col_types = 
                    readr::cols_only(
                      Project = readr::col_character(),
                      `External ID` = readr::col_character(),
                      `Participant ID` = readr::col_character(),
                      biopsy_location = readr::col_character(),
                      diagnosis = readr::col_character(),
                      baseline_montreal_location = readr::col_character(),
                      `Extent (E)` = readr::col_character(),
                      `Behavior (B)` = readr::col_character(),
                      consent_age = readr::col_double(),
                      `Age at diagnosis` = readr::col_double(),
                      race = readr::col_character(),
                      sex = readr::col_character(),
                      BMI = readr::col_double(),
                      `Alcohol (beer, brandy, spirits, hard liquor, wine, aperitif, etc.)` = readr::col_character(),
                      `smoking status` = readr::col_character(),
                      site_name = readr::col_character(),
                      fecalcal = readr::col_double(),
                      Antibiotics = readr::col_character(),
                      `Immunosuppressants (e.g. oral corticosteroids)` = readr::col_character(),
                      week_num = readr::col_character()
                    ))
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("Samples.16s" = "External ID")) 

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "HMP2",
    PMID = "31142855",
    subject_accession = `Participant ID` %>% as.character(),
    sample_accession = Samples.16s %>% as.character(),
    sample_accession_16S = Samples.16s %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = "ibdmdb",
    study_accession_db = "HMP2",
    subject_accession_db = NA_character_,
    sample_accession_db = Samples.16s %>% as.character(),
    batch = NA_character_,
    sample_type = biopsy_location %>% 
      dplyr::recode("Rectum" = "biopsy",
                    "Ileum" = "biopsy",
                    "Cecum" = "biopsy",
                    "Descending (left-sided) colon" = "biopsy",
                    "Sigmoid Colon" = "biopsy",
                    "Transverse colon" = "biopsy",
                    "Ascending (right-sided) colon" = "biopsy",
                    .missing = NA_character_),
    body_site = biopsy_location %>% 
      dplyr::recode("Rectum" = "rectum",
                    "Ileum" = "ileum",
                    "Cecum" = "colon",
                    "Descending (left-sided) colon" = "colon",
                    "Sigmoid Colon" = "colon",
                    "Transverse colon" = "colon",
                    "Ascending (right-sided) colon" = "colon",
                    .missing = NA_character_),
    body_site_additional = biopsy_location,
    disease = diagnosis %>% 
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "nonIBD" = "control"),
    control = diagnosis %>% 
      dplyr::recode("CD" = NA_character_,
                    "UC" = NA_character_,
                    "nonIBD" = "nonIBD"),
    L.cat = baseline_montreal_location %>% 
      dplyr::recode("L1" = "L1",
                    "L1+L4" = "L1+L4",
                    "L2" = "L2+L4",
                    "L3" = "L3",
                    "L3+L4" = "L3+L4",
                    .missing = NA_character_),
    E.cat = `Extent (E)` %>% 
      dplyr::recode(.missing = NA_character_),
    B.cat = `Behavior (B)` %>% 
      dplyr::recode(.missing = NA_character_),
    perianal = NA_character_,
    age = consent_age %>% as.numeric(),
    age_at_diagnosis = `Age at diagnosis` %>% as.numeric(),
    age_at_diagnosis.cat =   
      dplyr::case_when(age_at_diagnosis <= 16 ~ "A1",
                       age_at_diagnosis <= 40 ~ "A2",
                       age_at_diagnosis > 40 ~ "A3",
                       is.na(age_at_diagnosis) ~ NA_character_),
    race = race %>% 
      dplyr::recode("White" = "white",
                    "Black or African American" = "african_american",
                    "American Indian or Alaska Native" = "native_american",
                    "More than one race" = "more_than_one",
                    "Other" = NA_character_,
                    .missing = NA_character_),
    gender = sex %>% 
      dplyr::recode("Male" = "m",
                    "Female" = "f",
                    .missing = NA_character_),
    BMI = BMI %>% as.numeric(),
    alcohol = `Alcohol (beer, brandy, spirits, hard liquor, wine, aperitif, etc.)` %>% 
      dplyr::recode(.missing = NA_character_),
    smoke = `smoking status` %>% 
      dplyr::recode(.missing = NA_character_),
    calprotectin = fecalcal %>% as.numeric,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = Antibiotics %>% 
      dplyr::recode("No" = "n",
                    .missing = NA_character_),
    antibiotics_supp = NA_character_,
    immunosuppressants = `Immunosuppressants (e.g. oral corticosteroids)` %>% 
      dplyr::recode("No" = "n",
                    .missing = NA_character_),
    immunosuppressants_supp = NA_character_,
    steroids = NA_character_,
    steroids_supp = NA_character_,
    mesalamine_5ASA = NA_character_,
    mesalamine_5ASA_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = NA_real_,
    time_point_supp = week_num %>% as.character,
    family = NA_character_,
    family_supp = NA_character_,
    method_MBX = NA_character_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

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
