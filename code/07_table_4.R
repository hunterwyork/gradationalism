library(kableExtra)
library(data.table)
library(dplyr)


all_out_wide <- readRDS( "../ref/table_4_data.rds")


knitr::kable(all_out_wide,"latex",
             row.names = F, escape = F,
             align = c("l", rep("c", 4)),
             col.names = c("", "Father-Son", "Father-Daughter", "Mother-Son", "Mother-Daughter"), 
             booktabs = T,
             linesep = "") %>%
  add_header_above(c(" " =1, "Intergenerational Elasticity Among" = 4)) %>% 
  save_kable(file =  paste0("../outputs/",model_version, "/table_4.tex"))


