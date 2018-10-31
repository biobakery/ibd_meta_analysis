# Check that data frame is consistent with template
check.template <- function(metadata, template) {
  template <- readr::read_csv("data/template.csv", col_types = readr::cols())
  # Check that column names agree
  cond <- colnames(metadata) == template$col.name
  if(!all(cond)) 
    stop("The following columns and template don't match!\n",
         paste(template$col.name[!cond], collapse = ", "))
  # Check that column availability agrees
  cond <- metadata %>% 
    dplyr::select(dplyr::one_of(template$col.name[template$requiredness == "required"])) %>% 
    sapply(function(x) all(!is.na(x)))
  if(!all(cond))
    stop("The following columns that requires full availability don't meet requirements!\n",
         paste(names(cond)[!cond], collaspse = ", "))
  # Check that column types agree
  cond <- sapply(meta_curated, class) == template$var.class
  if(!all(cond))
    stop("The following column types and template don't match!\n",
         paste(names(cond)[!cond], collaspse = ", "))
  # Check that uniqueness agrees
  cond <- metadata %>% 
    dplyr::select(dplyr::one_of(template$col.name[template$uniqueness == "unique"])) %>% 
    sapply(function(x) !anyDuplicated(x[!is.na(x)])) 
  if(!all(cond))
    stop("The following columns that require uniqueness don't meet requirement!\n",
         paste(names(cond)[!cond], collaspse = ", "))
  # Check that (for categorical variables) variable content agrees
  cond <- sapply(template$col.name[template$allowedvalues != "*"], 
                 function(variable) {
                   values.allowed <- template$allowedvalues[template$col.name == variable] %>% 
                     strsplit("|", fixed = TRUE) %>% 
                     magrittr::extract2(1)
                   values <- metadata %>% magrittr::extract2(variable)
                   return(all(values[!is.na(values)] %in% values.allowed))
                 })
  if(!all(cond))
    stop("The following columns that requires specific values don't meet requirement!\n",
         paste(names(cond)[!cond], collapse = ", "))
  # Check that subject-specific variables are consistent
  cond <- sapply(setdiff(template$col.name[template$`subject specific?` == "y"], 
                         "subject_accession"), 
                 function(variable) {
                   metadata %>% 
                     dplyr::group_by(subject_accession) %>% 
                     dplyr::summarise(n_cat = dplyr::n_distinct(!!rlang::sym(variable),
                                                                na.rm = TRUE)) %>% 
                     dplyr::mutate(is_unique = n_cat <= 1) %>% 
                     extract2("is_unique") %>% 
                     all()
                 })
  if(!all(cond))
    stop("The following columns should be subject-specific!\n",
         paste(names(cond)[!cond], collapse = ", "))
  cond <- sapply(setdiff(template$col.name[template$`subject specific?` == "y"], 
                         "subject_accession"), 
                 function(variable) {
                   metadata %>% 
                     check_subject(variable) %>% 
                     dplyr::mutate(is_unique = n_cat == 1) %>% 
                     extract2("is_unique") %>% 
                     all()
                 })
  if(!all(cond))
    stop("The following subject-specific columns have uneven missingness pattern!\n",
         paste(names(cond)[!cond], collapse = ", "))
  
  # Check that IBD and control subtypes are concordant with disease
  cond <- metadata %>% 
    dplyr::transmute(
      CD = check_disease(metadata, "CD"),
      UC = check_disease(metadata, "UC"),
      control = check_disease(metadata, "control")
    ) %>% 
    apply(2, all)
  if(!all(cond))
    stop("The following disease categories have inconsistent subtype labelling!\n",
         paste(names(cond)[!cond], collapse = ", "))
  return(TRUE)
}

check_subject <- function(metadata, variable) {
  metadata %>% 
    dplyr::group_by(subject_accession) %>% 
    dplyr::summarise(n_cat = dplyr::n_distinct(!!rlang::sym(variable),
                                               na.rm = FALSE))
}

check_disease <- function(metadata, category) {
  if(category == "CD")
    metadata$disease == "CD" | (is.na(metadata$L.cat) & is.na(metadata$B.cat))
  else if(category == "UC")
    metadata$disease == "UC" | is.na(metadata$E.cat)
  else if(category == "control")
    metadata$disease == "control" | is.na(metadata$control)
  else stop("Category must be one of CD, UC, or control!")
}

# Helper functions for viewing spreadsheets
describe_var <- function(x) {
  class_x <- class(x)
  if(class_x == "logical") {
    summary_x <- c("TRUE_perc",
                   mean(x %in% TRUE) %>% 
                     round(digits = 4),
                   "na perc",
                   mean(is.na(x)) %>% 
                     round(digits = 4)) %>% 
      paste(collapse = " ")
  } 
  else if (class_x == "numeric") {
    summary_x <- c("mean",
                   mean(x, na.rm = TRUE) %>% 
                     round(digits = 4),
                   "max",
                   max(x, na.rm = TRUE) %>% 
                     round(digits = 4),
                   "min",
                   min(x, na.rm = TRUE) %>% 
                     round(digits = 4),
                   "na_perc",
                   mean(is.na(x)) %>% 
                     round(digits = 4)) %>% 
      paste(collapse = " ")
  }
  else if (class_x == "integer") {
    summary_x <- c("n_unique",
                   length(unique(x) %>% setdiff(NA)),
                   "na_perc",
                   mean(is.na(x)) %>% 
                     round(digits = 4),
                   "max",
                   max(x, na.rm = TRUE),
                   "min",
                   min(x, na.rm = TRUE)) %>% 
      paste(collapse = " ")
  }
  else if (class_x == "character") {
    summary_x = unique(x) %>% 
      setdiff(NA) %>% {
        c("n_unique",
          length(.),
          "na_perc", 
          mean(is.na(x)) %>% 
            round(digits = 4),
          "unique_values",
          .[1:min(length(.), 5)]) 
      } %>% 
      paste(collapse = " ")
  }
  else summary_x <- "others"
  return(c("class" = class_x, 
           "summary" = summary_x))
}

# Helper functions for viewing spreadsheets
describe <- function(df) {
  df %>% 
    purrr::map_df(describe_var) %>%
    dplyr::mutate(info = c("class", "summary")) %>% 
    tidyr::gather(variable_name, value, -info) %>% 
    tidyr::spread(info, value)
}

# transpose_df <- function(df) {
#   t_df <- data.table::transpose(df)
#   colnames(t_df) <- rownames(df)
#   rownames(t_df) <- colnames(df)
#   t_df <- t_df %>%
#     tibble::rownames_to_column(df = .) %>%
#     tibble::as_data_frame(x = .)
#   return(t_df)
# }