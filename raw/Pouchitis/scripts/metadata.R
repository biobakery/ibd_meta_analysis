rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "Pouchitis"
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
meta2 <- paste0("raw/", study, 
                "/metadata/Sample List Broad_pheno7_6_2012.xlsx") %>% 
  readxl::read_excel()
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("OriginalID" = "Sample ID"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "Pouchitis",
    PMID = "25887922",
    subject_accession = paste0(Family_ID, ":", Member_ID) %>% as.character(),
    sample_accession = OriginalID %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = NA_character_,
    study_accession_db = NA_character_,
    subject_accession_db = NA_character_,
    sample_accession_db = NA_character_,
    batch = SequencingRun %>% as.character(),
    sample_type = `Sample Location` %>% 
      dplyr::recode("Pouch" = "biopsy",
                    "PPI" = "biopsy",
                    "Sigmoid" = "biopsy",
                    "TI" = "biopsy"),
    body_site = `Sample Location` %>% 
      dplyr::recode("Pouch" = "ileum",
                    "PPI" = "ileum",
                    "Sigmoid" = "colon",
                    "TI" = "ileum"),
    body_site_additional = `Sample Location`,
    disease = Diagnosis %>% # These mappings are generated based on Dirk's metadata sheet
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "IC" = "IC",
                    "MC" = "IC",
                    "unconfirmed IBD" = NA_character_,
                    "FAP" = "FAP",
                    "FAP/cdiff" = "FAP",
                    "HC" = "control",
                    "Unaffected" = "control",
                    .missing = NA_character_),
    control = disease %>% 
      dplyr::recode("CD" = NA_character_,
                    "UC" = NA_character_,
                    "IC" = NA_character_,
                    "FAP" = NA_character_,
                    "control" = "HC",
                    .missing = NA_character_),
    L.cat = `Montreal Disease location` %>% 
      dplyr::recode("L1" = "L1",
                    "L2" = "L2",
                    "L3" = "L3",
                    .default = NA_character_,
                    .missing = NA_character_),
    E.cat = `Montreal Disease location` %>% 
      dplyr::recode("E1" = "E1",
                    "E2" = "E2",
                    "E3" = "E3",
                    .default = NA_character_,
                    .missing = NA_character_),
    B.cat = Behaviour %>% 
      dplyr::recode("1.0" = "B1",
                    "2.0" = "B2",
                    "3.0" = "B3",
                    .default = NA_character_,
                    .missing = NA_character_) %>% 
      ifelse(disease == "CD", ., NA_character_),
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_at_diagnosis = Ageof_Diag %>% 
      dplyr::recode("UK" = NA_character_) %>% 
      as.numeric(),
    age_at_diagnosis.cat =   
      dplyr::case_when(age_at_diagnosis <= 16 ~ "A1",
                       age_at_diagnosis <= 40 ~ "A2",
                       age_at_diagnosis > 40 ~ "A3",
                       is.na(age_at_diagnosis) ~ NA_character_),
    race = NA_character_,
    gender = Gender %>% 
      dplyr::recode("Male" = "m",
                    "Female" = "f"),
    BMI = BMI %>% 
      dplyr::recode("UK" = NA_character_) %>% 
      as.numeric(),
    alcohol = NA_character_,
    smoke = Smoking %>% 
      dplyr::recode("Current Smoker" = "current",
                    "Ex-Smoker" = "former",
                    "Never Smoked" = "never",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    site = `Collection Centre` %>% as.character(),
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = Antibiotics %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    antibiotics_supp = NA_character_,
    immunosuppressants = Immunosuppression %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    immunosuppressants_supp = NA_character_,
    steroids = Steroids %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    steroids_supp = NA_character_,
    mesalamine_5ASA = Mesalamine %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    mesalamine_5ASA_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = 1,
    time_point_supp = NA_character_,
    family = Family_ID,
    family_supp = "Family ID",
    method_MBX = NA_character_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

meta_curated <- meta_curated[, template$col.name]
test <- check_subject(meta_curated, "age_at_diagnosis")
meta_curated %>% 
  dplyr::filter(subject_accession %in% test$subject_accession[test$n_cat > 1]) %>% 
  nrow()
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
