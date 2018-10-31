rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "Herfarth_CCFA_Microbiome_3B_combined"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/2538_20180418-110548.txt") %>% 
  readr::read_tsv() # downloaded from Qiita
meta1 %>% 
  dplyr::filter(host_scientific_name == "Homo sapiens")
meta2 <- paste0("raw/", 
                study, 
                "/metadata/ebi_sample_accessions_study_2538.tsv") %>% 
  readr::read_tsv() # downloaded from Qiita
meta3 <- paste0("raw/", 
                study, 
                "/metadata/PRJEB23009.txt") %>% 
  readr::read_tsv() # downloaded from EBI
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2) %>% 
  dplyr::left_join(meta3, by = c("sample_accession" = "secondary_sample_accession"))
meta_raw <- meta_raw %>% 
  dplyr::filter(sample_accession != "None",
                sample_type != "control blank")
meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "Herfarth_CCFA_Microbiome_3B_combined",
    study_accession = "PRJEB23009",
    PMID = "29055911",
    subject_accession = host_subject_id %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = sample_accession %>% as.character(),
    alternative_sample_accession = sample_accession.y %>% as.character(),
    batch = NA_character_,
    sample_accession_16S = run_accession %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_type = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool"),
    body_site = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool"),
    body_site_additional = NA_character_,
    disease = ibd %>% 
      dplyr::recode("Crohns" = "CD",
                    "Control" = "control"),
    control = ibd %>% 
      dplyr::recode("Crohns" = NA_character_,
                    "Control" = "nonIBD"),
    IBD_subtype = NA_character_,
    IBD_subtype_additional = NA_character_,
    L.cat = disease_location %>% 
      stringr::str_remove_all("^A\\d") %>% 
      stringr::str_remove_all("B\\d$") %>% 
      stringr::str_replace_all("^l", "L") %>% 
      dplyr::recode("Missing: Not provided" = NA_character_),
    E.cat = NA_character_,
    B.cat = disease_location %>% 
      stringr::str_remove_all("^A\\d") %>% 
      stringr::str_remove_all("^[Ll]\\d") %>% 
      dplyr::recode("Missing: Not provided" = NA_character_),
    perianal = NA_character_,
    age = age %>% as.numeric(),
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = disease_location %>% 
      stringr::str_remove_all("B\\d$") %>% 
      stringr::str_remove_all("[Ll]\\d$") %>% 
      dplyr::recode("Missing: Not provided" = NA_character_),
    race = race %>% 
      dplyr::recode("Missing: Not provided" = NA_character_,
                    "Caucasian" = "white"),
    gender = sex %>% 
      dplyr::recode("male" = "m",
                    "female" = "f",
                    "Missing: Not provided" = NA_character_),
    BMI = body_mass_index %>% 
      as.numeric(),
    alcohol = alcohol_frequency %>% 
      dplyr::recode("Regularly" = "y",
                    "Occasionally" = "y",
                    "Rarely" = "n",
                    "Never" = "n",
                    "Missing: Not provided" = NA_character_),
    smoke = smoker_history %>% 
      dplyr::recode("never" = "never",
                    "former, 26 years" = "former",
                    "former, 22 years" = "former",
                    "y" = "current",
                    "former, 10 years" = "former",
                    "Never" = "never",
                    "Missing: Not provided" = NA_character_),
    site = "UNC",
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    antibiotics = NA_character_,
    antibiotics_supp = NA_character_,
    immunosuppressants = NA_character_,
      # immunosuppressants %>% 
      # dplyr::recode("1" = "y",
      #        "0" = "n",
      #        "Missing: Not provided" = NA_character_),
    immunosuppressants_supp = NA_character_,
    steroids = NA_character_,
      # steroids %>% 
      # dplyr::recode("0" = "n",
      #        "Missing: Not provided" = NA_character_),
    steroids_supp = NA_character_,
    mesalamine_5ASA = NA_character_,
      # asa_5_asa %>%
      # dplyr::recode("1" = "y",
      #        "0" = "n",
      #        "Missing: Not provided" = NA_character_),
    mesalamine_5ASA_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = collection_date %>% 
      dplyr::recode(
        "Missing: Not provided" = NA_character_),
    time_point_supp = "Collection date",
    family = NA_character_,
    family_supp = NA_character_,
    extraction_kit_16S = NA_character_,
    sequencing_platform_16S = NA_character_,
    number_reads_16S = NA_integer_,
    number_bases_16S = NA_integer_,
    minimum_read_length_16S = NA_integer_,
    median_read_length_16S = NA_integer_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of()) %>% 
  dplyr::filter(sample_accession != "ERS1973740") # This one seems to be mislabelled

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
