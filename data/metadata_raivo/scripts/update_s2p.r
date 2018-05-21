library(tidyverse)

# Read relevant datasets ------------------------------------------------------
sample2project = read_tsv("sample2project.txt", col_names = c("GID", "StoolID", "OriginalID", "DonorID", "SequencingRun", "Technology", "Project"))

mm = read_delim("~/Raivo/Projects/IBD_metaanalysis/metadata/raw/complete_mapping_1070_Aug28.csv", "\t", escape_double = FALSE, col_names = FALSE) %>% 
  select(OriginalID = X2, X1)

sample2project = sample2project %>% 
  left_join(mm) %>% 
  mutate(DonorID = coalesce(X1, DonorID)) %>% 
  select(GID, StoolID, OriginalID, DonorID, SequencingRun, Technology, Project)

write_delim(sample2project, "sample2project_updated.txt", delim = "\t", col_names = F)




