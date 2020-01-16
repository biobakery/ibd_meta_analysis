rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "MucosalIBD"
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
                study, ".csv") %>% 
  readr::read_csv(col_types = 
                    readr::cols(
                      .default = readr::col_character(),
                      `Ids/Id/0/_is_primary` = readr::col_double(),
                      `Description/Organism/_taxonomy_id` = readr::col_double(),
                      `Attributes/Attribute/2/__text` = readr::col_date(format = ""),
                      `Attributes/Attribute/5/__text` = readr::col_double(),
                      `Attributes/Attribute/9/__text` = readr::col_double(),
                      `Links/Link/__text` = readr::col_double(),
                      `Status/_when` = readr::col_datetime(format = ""),
                      `_publication_date` = readr::col_datetime(format = ""),
                      `_last_update` = readr::col_datetime(format = ""),
                      `_submission_date` = readr::col_datetime(format = ""),
                      `_id` = readr::col_double()
                    )) %>% 
  dplyr::mutate(
    subject_accession = `Attributes/Attribute/13/__text`,
    sample_accession = `Ids/Id/0/__text`,
    body_site = `Attributes/Attribute/7/__text`,
    age = `Attributes/Attribute/9/__text`,
    disease = `Attributes/Attribute/10/__text`,
    gender = `Attributes/Attribute/11/__text`
  ) %>% 
  dplyr::select(subject_accession,
                sample_accession,
                body_site,
                age,
                disease,
                gender)
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2,
                   by = c("DonorID" = "subject_accession"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "MucosalIBD",
    PMID = "27699268",
    subject_accession = DonorID %>% as.character(),
    sample_accession = OriginalID %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = "SRA",
    study_accession_db = "PRJNA317429",
    subject_accession_db = NA_character_,
    sample_accession_db = sample_accession %>% as.character(),
    batch = SequencingRun %>% as.character(),
    sample_type = body_site %>% 
      dplyr::recode("epithelium of ileum" = "biopsy"),
    body_site = body_site %>% 
      dplyr::recode("epithelium of ileum" = "ileum"),
    body_site_additional = body_site %>% 
      dplyr::recode("epithelium of ileum" = "Terminal ileum"),
    disease = disease %>% 
      dplyr::recode("Crohn's disease" = "CD",
                    "Not IBD" = "control",
                    .missing = NA_character_),
    control = disease %>% 
      dplyr::recode("CD" = NA_character_,
                    "control" = "nonIBD",
                    .missing = NA_character_),
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = age %>% as.numeric(),
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = NA_character_,
    race = NA_character_,
    gender = gender %>% 
      dplyr::recode(male = "m",
                    female = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    site = NA_character_,
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = NA_character_,
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

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
