library(tidyverse)

# Read data -------------------------------------------------------------------
sample2project = read_tsv("sample2project_updated.txt", col_names = c("GID", "StoolID", "OriginalID", "DonorID", "SequencingRun", "Technology", "Project"))

files = dir("processed/", pattern = "(common|special).rds")
files = data_frame(
  File = files
)

files = files %>% 
  extract(File, c("Project", "Type"), "(.*)_(common|special).rds", remove = F) %>% 
  mutate(Table = map(File, ~ readRDS(sprintf("processed/%s", .x))))

merged = pmap(filter(files, Type == "common"), function(Project, Table, ...){
  p = Project
  res = sample2project %>% 
    filter(Project == p) %>% 
    left_join(Table)
})

sample2project_common = sample2project %>% 
  left_join(bind_rows(merged))

write_tsv(sample2project_common, "sample2project_common.txt")
saveRDS(sample2project_common, "sample2project_common.rds")




