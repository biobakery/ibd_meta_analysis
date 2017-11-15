library(SRAdb)
library(magrittr)

meta_raw <- read.table('raw/Jansson_Lamendella_Crohns/metadata_raw.txt',
                       header = T,
                       sep = '\t',
                       stringsAsFactors = F,
                       check.names = F)
meta_raw$sample_alias <- paste0('qiita_sid_1629:', meta_raw$sample_name)

sra_con <- dbConnect(SQLite(), 'data/SRAmetadb.sqlite')
sample_accesion <- sraConvert('ERP020401', sra_con = sra_con)$sample %>% unique
sample_table <- dbGetQuery(sra_con, paste0("select * from sample where sample_accession in (",
                                           sample_accesion %>% 
                                             paste0("'", ., "'") %>% 
                                             paste(collapse = ', '),
                                           ")"))

sample_common <- intersect(meta_raw$sample_alias, sample_table$sample_alias)
meta_raw <- meta_raw %>% subset(sample_alias %in% sample_common)
sample_table <- sample_table %>% subset(sample_alias %in% sample_common)


dbGetQuery(sra_con, paste0("select * from sample where sample_alias='PRJNA269954.N00379'"))
meta_raw$Classification[meta_raw$sample_alias %in% setdiff(meta_raw$sample_alias, sample_table$sample_alias)]
