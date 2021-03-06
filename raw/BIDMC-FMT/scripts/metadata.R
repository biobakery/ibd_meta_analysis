rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "BIDMC-FMT"
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
  readr::read_tsv(col_types = "ccccccc")
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("Project", "DonorID", "OriginalID")) %>% 
  dplyr::left_join(meta3, by = c("Project", "DonorID", "OriginalID"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "BIDMC-FMT",
    PMID = "27542133",
    study_accession = NA_character_,
    PMID = NA_character_,
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
                    .missing = NA_character_),
    body_site = Location %>% 
      dplyr::recode("Stool" = NA_character_,
                    .missing = NA_character_),
    body_site_additional = Location %>% 
      dplyr::recode("Stool" = NA_character_,
                    .missing = NA_character_),
    disease = Diagnosis %>% 
      dplyr::recode("CD" = "CD",
                    .missing = NA_character_),
    control = NA_character_,
    L.cat = DiseaseLocation %>% 
      dplyr::recode("L2" = "L2",
                    "L3" = "L3",
                    .missing = NA_character_),
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = NA_character_,
    race = NA_character_,
    gender = Gender %>% 
      dplyr::recode(Male = "m",
                    Female = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = NA_character_,
    # Set to NA unless confirmed what it means
    antibiotics_supp = NA_character_,
    immunosuppressants = NA_character_,
    # Set to NA unless confirmed what it means
    immunosuppressants_supp = NA_character_,
    # Set to NA unless confirmed what it means
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

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
