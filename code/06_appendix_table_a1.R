library(data.table)
library(kableExtra)

table2 <- fread("../ref/appendix_table_a1.csv")
table2[, Datasource :=  sub('^(\\w?)', '\\U\\1', Datasource, perl=T)]
table2[,names(table2)[2:5] := lapply(.SD, formatC, digits = 0, big.mark = ","), .SDcols = names(table2)[2:5]]

table2 %>%  # mutate_all(linebreak, double_escape = F, linebreaker = "\\n") %>% 
  kable("latex", row.names = F, escape = F,align = c("l", rep("r", 6)),
        booktabs = T,
        linesep = "",
        caption = "Table 1: Total Parent-Child Pairings, Occupational Categories, and Data Coverage by Datasource") %>%  kable_styling()%>%
  column_spec(2:7, width = "2.7cm") %>% 
  row_spec(12,bold=T,hline_after = F)
