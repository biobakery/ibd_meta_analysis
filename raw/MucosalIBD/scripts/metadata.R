rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "MucosalIBD"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- readr::read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  dplyr::filter(Project == study, Technology == "16S") %>% 
  dplyr::select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, "/metadata/",
                study, ".csv") %>% 
  readr::read_csv() %>% 
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
    study_accession = "PRJNA317429",
    PMID = "27699268",
    subject_accession = DonorID %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = OriginalID %>% as.character(),
    alternative_sample_accession = sample_accession %>% as.character(),
    batch = SequencingRun %>% as.character(),
    `16S_sample_accession` = GID %>% as.character(),
    WGS_sample_accession = NA_character_,
    sample_type = body_site %>% 
      dplyr::recode("epithelium of ileum" = "biopsy"),
    body_site = body_site %>% 
      dplyr::recode("epithelium of ileum" = "ileum"),
    disease = disease %>% 
      dplyr::recode("Crohn's disease" = "CD",
             "Not IBD" = "control"),
    control = disease %>% 
      dplyr::recode("Crohn's disease" = NA_character_,
                    "Not IBD" = "nonIBD"),
    IBD_subtype = NA_character_,
    IBD_subtype_additional = NA_character_,
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = age %>% as.numeric(),
    age_at_diagnosis = NA_real_,
    gender = gender %>% 
      dplyr::recode(male = "m",
             female = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    site = NA_character_,
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    antibiotics = NA_character_,
    antibiotics_supp = NA_character_,
    immunosuppressants = NA_character_,
    immunosuppressants_supp = NA_character_,
    steroids = NA_character_,
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
