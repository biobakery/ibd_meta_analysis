rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "Herfarth_CCFA_Microbiome_3B_combined"
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccccc")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/2538_20180418-110548.txt") %>% 
  readr::read_tsv(col_types =
                    readr::cols(
                      .default = readr::col_character(),
                      dna_extracted = readr::col_logical(),
                      elevation = readr::col_double(),
                      host_taxid = readr::col_double(),
                      latitude = readr::col_double(),
                      longitude = readr::col_double(),
                      physical_specimen_remaining = readr::col_logical(),
                      public = readr::col_logical(),
                      qiita_study_id = readr::col_double(),
                      taxon_id = readr::col_double()
                    )) # downloaded from Qiita
# meta1 %>% 
#   dplyr::filter(host_scientific_name == "Homo sapiens")
meta2 <- paste0("raw/", 
                study, 
                "/metadata/ebi_sample_accessions_study_2538.tsv") %>% 
  readr::read_tsv(col_types = "cc") # downloaded from Qiita
meta3 <- paste0("raw/", 
                study, 
                "/metadata/PRJEB23009.txt") %>% 
  readr::read_tsv(col_types =
                    readr::cols(
                      study_accession = readr::col_character(),
                      sample_accession = readr::col_character(),
                      secondary_sample_accession = readr::col_character(),
                      experiment_accession = readr::col_character(),
                      run_accession = readr::col_character(),
                      tax_id = readr::col_double(),
                      scientific_name = readr::col_character(),
                      instrument_model = readr::col_character(),
                      library_layout = readr::col_character(),
                      fastq_ftp = readr::col_character(),
                      fastq_galaxy = readr::col_character(),
                      submitted_ftp = readr::col_character(),
                      submitted_galaxy = readr::col_character(),
                      sra_ftp = readr::col_character(),
                      sra_galaxy = readr::col_character(),
                      cram_index_ftp = readr::col_logical(),
                      cram_index_galaxy = readr::col_logical()
                    )) # downloaded from EBI
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = "sample_name") %>% 
  dplyr::left_join(meta3, by = c("sample_accession" = "secondary_sample_accession"))
meta_raw <- meta_raw %>% 
  dplyr::filter(sample_accession != "None",
                sample_type != "control blank")
meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "Herfarth_CCFA_Microbiome_3B_combined",
    PMID = "29055911",
    subject_accession = host_subject_id %>% as.character(),
    sample_accession = sample_accession %>% as.character(),
    sample_accession_16S = run_accession %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_accession_MBX = NA_character_,
    database = "ENA",
    study_accession_db = "PRJEB23009",
    subject_accession_db = NA_character_,
    sample_accession_db = sample_accession.y %>% as.character(),
    batch = NA_character_,
    sample_type = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool"),
    body_site = body_site %>% 
      dplyr::recode("UBERON:feces" = NA_character_),
    body_site_additional = body_site %>% 
      dplyr::recode("UBERON:feces" = NA_character_),
    disease = ibd %>% 
      dplyr::recode("Crohns" = "CD",
                    "Control" = "control"),
    control = ibd %>% 
      dplyr::recode("Crohns" = NA_character_,
                    "Control" = "nonIBD"),
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
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
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
    time_point = NA_real_,
    time_point_supp = collection_date %>% 
      dplyr::recode(
        "Missing: Not provided" = NA_character_),
    family = NA_character_,
    family_supp = NA_character_,
    method_MBX = NA_character_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of()) %>% 
  dplyr::filter(sample_accession != "ERS1973740") # This one seems to be mislabelled

# format time point into 1, 2, 3, ...
meta_curated <- meta_curated %>% 
  dplyr::mutate(time_point = as.Date(time_point_supp)) %>% 
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
