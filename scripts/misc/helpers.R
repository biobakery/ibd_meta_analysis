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

describe <- function(df) {
  df %>% 
    map_df(describe_var) %>%
    mutate(info = c("class", "summary")) %>% 
    gather(variable_name, value, -info) %>% 
    spread(info, value)
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