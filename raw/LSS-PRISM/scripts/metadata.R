rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "LSS-PRISM"
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccc")
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
meta2 <- readxl::read_xlsx("raw/CS-PRISM/metadata/consolidatedWT_IBD_metadata.xlsx",
                           sheet = "Data") %>% 
  dplyr::mutate(Included = Included == 35)
meta3 <- readxl::read_xlsx("raw/CS-PRISM/metadata/consolidatedWT_IBD_metadata.xlsx",
                           sheet = "metadata")
meta3_long1 <- meta3 %>% 
  tidyr::gather(key = `sample collection ID`,
                value = `sample ID`,
                `flora1 (NI)`,
                `flora2 (NI)`,
                `flora3 (NI)`,
                `flora5 (Inf)`,
                `flora6 (Inf)`,
                `flora7 (Inf)`,
                `flora9 (NI)`,
                `flora10 (NI)`) %>% 
  dplyr::filter(!is.na(`sample ID`)) # First, match based on sampe ID if possible. LSS has biopsy samples
meta3_long2 <- meta3 %>% 
  dplyr::filter(is.na(`flora1 (NI)`),
                is.na(`flora2 (NI)`),
                is.na(`flora3 (NI)`),
                is.na(`flora5 (Inf)`),
                is.na(`flora6 (Inf)`),
                is.na(`flora7 (Inf)`),
                is.na(`flora9 (NI)`),
                is.na(`flora10 (NI)`)) %>% 
  tidyr::gather(key = `# of 16S sample`,
                value = `16S sample ID`,
                `16S G`, `16S G2`) %>% 
  dplyr::filter(!is.na(`16S sample ID`)) # Then, match based on GID if sample ID is not available
# Fortunately these samples are all stool samples

meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("GID" = "16S G#")) %>% 
  dplyr::mutate(SubjectID2 = DonorID %>% 
                  stringr::str_replace("^P", "")) 
# Most of the matchings between 16S sample IDs are correct
unmatched <- meta_raw %>% 
  dplyr::filter(
    `Collaborator Sample ID...4` %>% 
      is_in(meta3_long1$`sample ID`) %>% 
      not,
    GID %>% 
      is_in(meta3_long2$`16S sample ID`) %>% 
      not,
    Included)
# View(unmatched)
nrow(unmatched)

meta_raw_sampleID <- meta_raw %>% 
  dplyr::inner_join(meta3_long1, by = c("Collaborator Sample ID...4" = "sample ID")) %>% 
  dplyr::select(-`16S G`, - `16S G2`)
meta_raw_GID <- meta_raw %>% 
  dplyr::filter(!(GID %in% meta_raw_sampleID$GID)) %>% 
  dplyr::inner_join(meta3_long2, by = c("GID" = "16S sample ID")) %>% 
  dplyr::select(-`flora1 (NI)`,
                -`flora2 (NI)`,
                -`flora3 (NI)`,
                -`flora5 (Inf)`,
                -`flora6 (Inf)`,
                -`flora7 (Inf)`,
                -`flora9 (NI)`,
                -`flora10 (NI)`,
                -`# of 16S sample`) %>% 
  dplyr::mutate(`sample collection ID` = "stool")
# Sanity check
nrow(meta_raw_sampleID) + nrow(meta_raw_GID) + nrow(unmatched) == sum(meta_raw$Included)
all(colnames(meta_raw_sampleID) == colnames(meta_raw_GID))

meta_raw <- rbind(meta_raw_sampleID, meta_raw_GID)

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "LSS-PRISM",
    PMID = "29183332",
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
    sample_type = `sample collection ID` %>% 
      dplyr::recode("stool" = "stool",
                    .missing = NA_character_),
    body_site_additional = `sample collection ID` %>% 
      dplyr::recode("stool" = NA_character_,
                    .missing = NA_character_),
    body_site = `sample collection ID` %>% dplyr::recode(
      "stool" = NA_character_,
      .missing = NA_character_),
    disease = Diagnosis %>% 
      dplyr::recode("Crohn's Disease" = "CD",
                    "Ulcerative colitis" = "UC",
                    .missing = NA_character_),
    control = Diagnosis %>% 
      dplyr::recode(.default = NA_character_,
                    .missing = NA_character_),
    L.cat = `Location (L) prior to first surgery` %>% 
      dplyr::recode("L3 (Ileocolon)" = "L3",
                    "L2 (Colon)" = "L2", 
                    "L1 (TI)" = "L1",
                    "L3 + L4 (Ileocolon and Upper GI)" = "L3+L4",
                    .missing = NA_character_),
    E.cat = `Extent (E)` %>% 
      dplyr::recode("E3 (Extensive UC / Pancolitis)" = "E3",
                    "E1 (Ulcerative proctitis)" = "E1",
                    .missing = NA_character_),
    B.cat = `Behavior (B)` %>% 
      dplyr::recode("B1 (Inflammatory disease)" = "B1",
                    "B1p (Inflammatory, perianal disease)" = "B1",
                    "B3p (Penetrating and perianal disease)" = "B3",
                    .missing = NA_character_),
    perianal = `Behavior (B)` %>% 
      dplyr::recode("B1 (Inflammatory disease)" = "n",
                    "B1p (Inflammatory, perianal disease)" = "y",
                    "B3p (Penetrating and perianal disease)" = "y",
                    .missing = NA_character_),
    age = ifelse(`Age at sample collection (year)` < 999,
                 `Age at sample collection (year)`,
                 NA_real_),
    age_at_diagnosis = ifelse(`Age at Diagnosis` < 999,
                              `Age at Diagnosis`,
                              NA_real_),
    age_at_diagnosis.cat =   
      dplyr::case_when(age_at_diagnosis <= 16 ~ "A1",
                       age_at_diagnosis <= 40 ~ "A2",
                       age_at_diagnosis > 40 ~ "A3",
                       is.na(age_at_diagnosis) ~ NA_character_),
    race = Race %>% 
      dplyr::recode("White" = "white",
                    .missing = NA_character_),
    gender = Sex %>% 
      dplyr::recode(Male = "m",
                    Female = "f",
                    .missing = NA_character_),
    BMI = ifelse(Weight != 999,
                 Weight,
                 NA) /
      ifelse(Height != 999,
             (Height/100)^2,
             NA),
    alcohol = NA_character_,
    smoke = `smoking status` %>% 
      dplyr::recode("Never smoked" = "never",
                    "Current smoker" = "current",
                    "Former smoker" = "former",
                    .missing = NA_character_),
    calprotectin = `Fecal Calprotectin Results` %>% 
      stringr::str_replace_all(stringr::fixed("ug/g"), "") %>% 
      as.numeric,
    PCDAI = NA_real_,
    HBI = NA_real_,
    SCCAI = NA_real_,
    antibiotics = dplyr::case_when(
      `Flagyl (Metronidazole)` %in% "Currently taking" |
        `Cipro (Ciprofloxacin)` %in% "Currently taking" |
        `Xifaxin (rifaxamin)` %in% "Currently taking" |
        Levaquin %in% "Currently taking" ~ "y",
      `Flagyl (Metronidazole)` == "Not currently taking" &
        `Cipro (Ciprofloxacin)` == "Not currently taking" &
        `Xifaxin (rifaxamin)` == "Not currently taking" &
        Levaquin == "Not currently taking" ~ "n",
      TRUE ~ NA_character_
    ) ,
    antibiotics_supp = 
      paste0(ifelse(`Flagyl (Metronidazole)` %in% "Currently taking",
                    "Flagyl (Metronidazole),",
                    ""),
             ifelse(`Cipro (Ciprofloxacin)` %in% "Currently taking",
                    "Cipro (Ciprofloxacin),",
                    ""),
             ifelse(`Xifaxin (rifaxamin)` %in% "Currently taking",
                    "Xifaxin (rifaxamin),",
                    ""),
             ifelse(Levaquin %in% "Currently taking",
                    "Levaquin,",
                    "")) %>% 
      stringr::str_replace_all(",$", ""),
    immunosuppressants = dplyr::case_when(
      `Azathioprine (Imuran, Azasan)` %in% "Currently taking" |
        `Methotrexate` %in% "Currently taking" |
        `Mercaptopurine (Purinethol, 6MP)` %in% "Currently taking" ~ "y",
      `Azathioprine (Imuran, Azasan)` == "Not currently taking" &
        `Methotrexate` == "Not currently taking" &
        `Mercaptopurine (Purinethol, 6MP)` == "Not currently taking" ~ "n",
      TRUE ~ NA_character_
    ),
    immunosuppressants_supp = 
      paste0(ifelse(`Azathioprine (Imuran, Azasan)` %in% "Currently taking",
                    "Azathioprine (Imuran, Azasan),",
                    ""),
             ifelse(`Methotrexate` %in% "Currently taking",
                    "Methotrexate,",
                    ""),
             ifelse(`Mercaptopurine (Purinethol, 6MP)` %in% "Currently taking",
                    "Mercaptopurine (Purinethol, 6MP),",
                    "")) %>% 
      stringr::str_replace_all(",$", ""),
    steroids = dplyr::case_when(
      `Prednisone` %in% "Currently taking" |
        `Entocort (Budesonide)` %in% "Currently taking" |
        `Uceris (Budesonide ER)` %in% "Currently taking" |
        `SoluMedrol (Medrol)` %in% "Currently taking" |
        `IV steroids` %in% "Currently taking" |
        `Cortenemas, Cortifoam, Proctofoam` %in% "Currently taking" ~ "y",
      `Prednisone` == "Not currently taking" |
        `Entocort (Budesonide)` == "Not currently taking" |
        `Uceris (Budesonide ER)` == "Not currently taking" |
        `SoluMedrol (Medrol)` == "Not currently taking" |
        `IV steroids` == "Not currently taking" |
        `Cortenemas, Cortifoam, Proctofoam` == "Not currently taking" ~ "n",
      TRUE ~ NA_character_
    ),
    steroids_supp = 
      paste0(ifelse(`Prednisone` %in% "Currently taking",
                    "Prednisone,",
                    ""),
             ifelse(`Entocort (Budesonide)` %in% "Currently taking",
                    "Entocort (Budesonide),",
                    ""),
             ifelse(`Uceris (Budesonide ER)` %in% "Currently taking",
                    "Uceris (Budesonide ER),",
                    ""),
             ifelse(`SoluMedrol (Medrol)` %in% "Currently taking",
                    "SoluMedrol (Medrol),",
                    ""),
             ifelse(`IV steroids` %in% "Currently taking",
                    "IV steroids,",
                    ""),
             ifelse(`Cortenemas, Cortifoam, Proctofoam` %in% "Currently taking",
                    "Cortenemas, Cortifoam, Proctofoam,",
                    "")) %>% 
      stringr::str_replace_all(",$", ""),
    mesalamine_5ASA = dplyr::case_when(
      `Delzicol (oral mesalamine)` %in% "Currently taking" |
        `Asacol (mesalamine)` %in% "Currently taking" |
        `Pentasa (mesalamine)` %in% "Currently taking" |
        `Lialda (mesalamine)` %in% "Currently taking" |
        `Apriso (mesalamine)` %in% "Currently taking" |
        `Colazal (balasalizide)` %in% "Currently taking" |
        `Sulfazalazine (Azulfidine)` %in% "Currently taking" |
        `Dipentum (olsalazine)` %in% "Currently taking" |
        `Rowasa enemas (mesalamine enemas)` %in% "Currently taking" |
        `Canasa suppositories (mesalamine suppositories)` %in% "Currently taking" ~ "y",
      `Delzicol (oral mesalamine)` == "Not currently taking" |
        `Asacol (mesalamine)` == "Not currently taking" |
        `Pentasa (mesalamine)` == "Not currently taking" |
        `Lialda (mesalamine)` == "Not currently taking" |
        `Apriso (mesalamine)` == "Not currently taking" |
        `Colazal (balasalizide)` == "Not currently taking" |
        `Sulfazalazine (Azulfidine)` == "Not currently taking" |
        `Dipentum (olsalazine)` == "Not currently taking" |
        `Rowasa enemas (mesalamine enemas)` == "Not currently taking" |
        `Canasa suppositories (mesalamine suppositories)` == "Not currently taking" ~ "n",
      TRUE ~ NA_character_
    ),
    mesalamine_5ASA_supp = 
      paste0(ifelse(`Delzicol (oral mesalamine)` %in% "Currently taking",
                    "Delzicol (oral mesalamine),",
                    ""),
             ifelse(`Asacol (mesalamine)` %in% "Currently taking",
                    "Asacol (mesalamine),",
                    ""),
             ifelse(`Pentasa (mesalamine)` %in% "Currently taking",
                    "Pentasa (mesalamine),",
                    ""),
             ifelse(`Lialda (mesalamine)` %in% "Currently taking",
                    "Lialda (mesalamine),",
                    ""),
             ifelse(`Apriso (mesalamine)` %in% "Currently taking",
                    "Apriso (mesalamine),",
                    ""),
             ifelse(`Colazal (balasalizide)` %in% "Currently taking",
                    "Colazal (balasalizide),",
                    ""),
             ifelse(`Sulfazalazine (Azulfidine)` %in% "Currently taking",
                    "Sulfazalazine (Azulfidine),",
                    ""),
             ifelse(`Dipentum (olsalazine)` %in% "Currently taking",
                    "Dipentum (olsalazine),",
                    ""),
             ifelse(`Rowasa enemas (mesalamine enemas)` %in% "Currently taking",
                    "Rowasa enemas (mesalamine enemas),",
                    ""),
             ifelse(`Canasa suppositories (mesalamine suppositories)` %in% "Currently taking",
                    "Canasa suppositories (mesalamine suppositories),",
                    "")) %>% 
      stringr::str_replace_all(",$", ""),
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = NA_real_,
    time_point_supp = `LSS Month`,
    family = NA_character_,
    family_supp = NA_character_,
    method_MBX = NA_character_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())
meta_curated <- meta_curated[, template$col.name]

# format time point into 1, 2, 3, ...
meta_curated <- meta_curated %>% 
  dplyr::mutate(time_point = as.numeric(time_point_supp)) %>% 
  dplyr::arrange(time_point) %>% 
  dplyr::group_by(subject_accession) %>% 
  dplyr::mutate(time_point = create_timepoint(time_point)) %>% 
  dplyr::ungroup()

if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
