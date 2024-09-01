library(Rfast)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(haven)
library(data.table)
library(umap)
library(zoo)
pc_n <- 15
n <- 10000
theme_set(theme_bw(base_size = 8, base_family = "serif")) 
# load the data



setwd("/Users/hyork/Dropbox (Princeton)/projects/occupation/code")

# load occ_soc xwalk
occ_xwalk <- data.table(read_excel("../ref/nem-occcode-cps-crosswalk.xlsx"))
names(occ_xwalk) <- occ_xwalk[4,] %>% unlist()
occ_xwalk <- occ_xwalk[-(1:4),]
occ_xwalk[, OCCSOC := gsub("-", "", `Hybrid SOC Code`)]
# now create skills dataset

skills_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Skills.xlsx") %>% data.table()
skills_2018[, year := 2018]
setnames(skills_2018, names(skills_2018), gsub(" ", ".", names(skills_2018), fixed = T))
skills_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
skills1 <- rbindlist(list( skills_2018), fill = T)
skills1 <- skills1[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
skills1[, Element.Name := paste0("skl_", Element.Name)]
# add abilities
abilities_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Abilities.xlsx") %>% data.table()
abilities_2018[, year := 2018]
setnames(abilities_2018, names(abilities_2018), gsub(" ", ".", names(abilities_2018), fixed = T))
abilities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
abilities <- rbindlist(list( abilities_2018), fill = T)
abilities <- abilities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
abilities[, Element.Name := paste0("abl_", Element.Name)]
# add knowledge
knowledge_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Knowledge.xlsx") %>% data.table()
knowledge_2018[, year := 2018]
setnames(knowledge_2018, names(knowledge_2018), gsub(" ", ".", names(knowledge_2018), fixed = T))
knowledge_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
knowledge <- rbindlist(list(knowledge_2018), fill = T)
knowledge <- knowledge[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
knowledge[, Element.Name := paste0("knl_", Element.Name)]
# add work activities
workactivities_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Work Activities.xlsx") %>% data.table()
workactivities_2018[, year := 2018]
setnames(workactivities_2018, names(workactivities_2018), gsub(" ", ".", names(workactivities_2018), fixed = T))
workactivities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workactivities <- rbindlist(list( workactivities_2018), fill = T)
workactivities <- workactivities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workactivities[, Element.Name := paste0("act_", Element.Name)]
# add work styles

workstyles_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Work Styles.xlsx") %>% data.table()

workstyles_2018[, year := 2018]
setnames(workstyles_2018, names(workstyles_2018), gsub(" ", ".", names(workstyles_2018), fixed = T))
workstyles_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workstyles <- rbindlist(list(workstyles_2018), fill = T)
workstyles <- workstyles[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workstyles[, Element.Name := paste0("sty_", Element.Name)]
# add work context

context_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Work Context.xlsx") %>% data.table()
context_2018[, year := 2018]
setnames(context_2018, names(context_2018), gsub(" ", ".", names(context_2018), fixed = T))
context_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
context <- rbindlist(list( context_2018), fill = T)
context <- context[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
context <- context[Scale.ID %in% c("CX", "CT")]
context[, Element.Name := paste0("ctx_", Element.Name)]
# add work values
values_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Work Values.xlsx") %>% data.table()
values_2018[, year := 2018]
setnames(values_2018, names(values_2018), gsub(" ", ".", names(values_2018), fixed = T))
values_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
values <- rbindlist(list(values_2018), fill = T)
values <- values[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
values <- values[Scale.ID %in% c("EX")]
values[, Element.Name := paste0("vlu_", Element.Name)]
# add education training, etc

education_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Education, Training, and Experience.xlsx") %>% data.table()
ed_map <- read_excel("../ref/db_22_2_excel 2018.2/Education, Training, and Experience Categories.xlsx") %>% data.table()

education_2018[, year := 2018]
setnames(education_2018, names(education_2018), gsub(" ", ".", names(education_2018), fixed = T))
education_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
education <- rbindlist(list( education_2018), fill = T)

###### formerly weighted mean. now mode
get_mode <- function(x, w){x[w == max(w)]}
education[, cum_percent := rev(cumsum(rev(Data.Value))), by = .(`O*NET-SOC.Code`, Element.Name, Scale.ID, year)]
education <- education[,.(Data.Value = max(Category[cum_percent >80])), by = .(O.NET.SOC.Code, Element.Name, Scale.ID, year)]
education <- education[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
education[is.infinite(Data.Value), data_value := 1]
education[, Element.Name := paste0("edu_", Element.Name)]
# # add tasks
# task_2009 <- read.delim('../ref/db_14_0 2009.7/Task Ratings.txt') %>% data.table()
# task_2013 <-  read.delim('../ref/db_18_0_2013.7/Task Ratings.txt') %>% data.table()
# task_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Task Ratings.xlsx") %>% data.table()
# task_2009[, year := 2009]
# task_2013[, year := 2013]
# task_2018[, year := 2018]
# setnames(task_2018, names(task_2018), gsub(" ", ".", names(task_2018), fixed = T))
# task_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
# task <- rbindlist(list(task_2009, task_2013, task_2018), fill = T)
# task <- task[, .(O.NET.SOC.Code, Task, Scale.ID, Data.Value, Standard.Error, year)]
# task <- task[Scale.ID %in% c("IM")]
# task[, Element.Name := paste0("tsk_", Task)]
# task <- task[!is.na(Task)]
#
skills <- rbindlist(list(skills1, knowledge, abilities, workstyles, 
                         workactivities,context, values, education), fill = T)
#skills <- rbindlist(list(skills, knowledge, abilities, workstyles, workactivities), fill = T)
# standardize
skills[Scale.ID == "LV", Data.Value := Data.Value/7]
skills[Scale.ID == "EX", Data.Value := (Data.Value-1)/6]
skills[Scale.ID == "IM", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CX", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CT", Data.Value := (Data.Value-1)/2]
skills[Scale.ID == "RW", Data.Value := (Data.Value-1)/9]
skills[Scale.ID == "PT", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "OJ", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "RL", Data.Value := (Data.Value-1)/12]
skills[Element.Name %like% "sty_", Scale.ID := "LV"]
skills[Element.Name %like% "ctx", Scale.ID := "LV"]
skills[Element.Name %like% "vlu", Scale.ID := "LV"]
skills[Element.Name %like% "edu_", Scale.ID := "LV"]
#
skills <- skills[Scale.ID == "LV"]
#saveRDS(skills, "../ref/skills_unedited.rds")
#





# reformat onet codes to merge
# skills[,Element.Name2 := paste0(substr(Element.Name, 1, 15), "_", str_sub(Element.Name, -15, -1))]
# skills_xwalk <- skills[,.(Element.Name2, Element.Name)] %>% unique()
# fwrite(skills_xwalk, "../ref/skills_xwalk.csv")
#skills[,Element.Name := paste0(substr(Element.Name, 1, 15), "_", str_sub(Element.Name, -15, -1))]
skills[, OCCSOC := gsub("-", "", substr(O.NET.SOC.Code,1,7))]
skills[, Standard.Error := as.numeric(Standard.Error)]
# skills <- skills[,.(Data.Value = mean(Data.Value),
#                     Standard.Error = mean((Standard.Error))), by = .(Element.Name,Scale.ID, OCCSOC)]
# add income
# chack to see which occ codes are missing
occ_xwalk[!OCCSOC %in% unique(skills$OCCSOC), unique(OCCSOC)]
# 
skills[, Element.Name := paste0(Element.Name, ".", Scale.ID)]
#skills[,Data.Value := percent_rank(Data.Value), by = .(Element.Name, Scale.ID)]
# skills <- skills[, .(Data.Value  = mean(Data.Value, na.rm = T), 
#            Standard.Error = mean(Standard.Error, na.rm = T)), # throw away SE anyway
#            by = .(Element.Name, Element.Name2, OCCSOC)]

skills <- skills[, .(Data.Value  = mean(Data.Value, na.rm = T), 
                     Standard.Error = mean(Standard.Error, na.rm = T)), # throw away SE anyway
                 by = .(Element.Name, OCCSOC, O.NET.SOC.Code)]
skills <- skills[!OCCSOC %in% skills[is.na(Data.Value)]$OCCSOC]


skills <- skills[!Element.Name %like% "edu_Appre|edu_Job"]


skills_wide <- dcast(skills, OCCSOC + O.NET.SOC.Code   ~Element.Name, value.var = "Data.Value")
skills_wide <- skills_wide[!OCCSOC %in% skills_wide[!complete.cases(skills_wide)]$OCCSOC]

skills_wide <- skills_wide[OCCSOC != 152091]

skills <- copy(skills_wide)



occ50_xwalk <- readxl::read_xlsx("../ref/Census_integrated_occ_crosswalks.xlsx") %>% data.table()
occ50_xwalk[,`Occupation category description`:=na.locf(`Occupation category description`)]
occ50_xwalk[,OCC1950:=na.locf(OCC1950)]
occ50_xwalk <- occ50_xwalk[,.(`OCC1950`, `ACS 2003-`, `Occupation category description`, `2000`)]
setnames(occ50_xwalk, c("OCC1950", "CPS Code", "OCC1950_name", "OCC2000"))
occ50_xwalk <- occ50_xwalk[!is.na(`CPS Code`)]

occ50_xwalk <- occ50_xwalk[OCC1950 %in% 0:990]
occ50_xwalk <- occ50_xwalk[!is.na(`CPS Code`)]
occ50_xwalk[, `CPS Code` := as.numeric(`CPS Code`)]

#

occ_2012_soc <- readxl::read_excel("../ref/2018-occupation-code-list-and-crosswalk.xlsx", sheet = 2) %>% data.table()
occ_2012_soc <- occ_2012_soc[-(1:13)]
occ_2012_soc[, '...1' := NULL]
setnames(occ_2012_soc, c("occ_title", "occ_2018", "SOC"))

occ_2012_soc[, occ_2018 := as.numeric(occ_2018)]



# 2010 to 2018 crosswalk

occ_2010_2018 <- readxl::read_excel("../ref/2018-occupation-code-list-and-crosswalk.xlsx", sheet = 4) %>% data.table()
occ_2010_2018 <- occ_2010_2018[-(1:3)]
occ_2010_2018 <- occ_2010_2018[,1:3]
setnames(occ_2010_2018, c("SOC", "occ_2018", "occ_title"))

occ_2010_2018[, occ_2018 := as.numeric(occ_2018)]
occ_2010_2018 <- occ_2010_2018[!occ_2018 %in% occ_2012_soc$occ_2018]

occ_2012_soc <- rbind(occ_2012_soc, occ_2010_2018)

# fix some stragglers before merging
occ50_xwalk[`CPS Code` == 130, `CPS Code` := 135]
occ50_xwalk[`CPS Code` == 200, `CPS Code` := 205]
occ50_xwalk[`CPS Code` == 2140, `CPS Code` := 2145]
occ50_xwalk[`CPS Code` == 2150, `CPS Code` := 2160]
occ50_xwalk[`CPS Code` == 3130, `CPS Code` := 3255]
occ50_xwalk[`CPS Code` == 3950, `CPS Code` := 3955]
occ50_xwalk[`CPS Code` == 4550, `CPS Code` := 9050]
occ50_xwalk[`CPS Code` == 6000, `CPS Code` := 6005]
occ50_xwalk[`CPS Code` == 6350, `CPS Code` := 6355]
occ50_xwalk[`CPS Code` == 8230, `CPS Code` := 8256]
occ50_xwalk[`CPS Code` == 8240, `CPS Code` := 8255]
occ50_xwalk[`CPS Code` == 6510, `CPS Code` := 6515]


occ_2012_soc_merge <- merge(occ_2012_soc[!is.na(occ_2018)], occ50_xwalk[!is.na(`CPS Code`)], by.x = "occ_2018", by.y = "CPS Code", all= T)
occ_2012_soc_merge[!is.na(OCC2000) & is.na(SOC)]


occ_2012_soc_merge[, SOC_num := gsub("-", "", SOC)]

occ_2012_soc_merge <- occ_2012_soc_merge[!is.na(OCC2000)]
occ_2012_soc_merge <- occ_2012_soc_merge[SOC != "none"]

bad_codes <-occ_2012_soc_merge[! SOC_num %in% unique(skills$SOC), SOC]

skills[, SOC := str_sub(O.NET.SOC.Code,1,7)]

skills <- skills[!SOC %like% "^55"]
bad_codes <- bad_codes[!bad_codes %like% "^55"]

factor_names <- names(skills)[names(skills)%like% ".LV$"]
vars <- names(skills)[names(skills)%like% ".LV$"]
# vars <- paste0("`", vars, "`")

vars_no_ed <- vars[!vars %like% "^edu_Required"]
vars_ed <- vars[vars %like% "^edu_Required"]

# do first not ed
the_restr <- function(c.code){
  print(c.code)
  if(nrow(skills[SOC %like% paste0("^", substr(c.code,1,6))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,6)), 
                  lapply(.SD, mean, na.rm = T), 
                  .SDcols = vars_no_ed]
    out[, SOC := c.code]
    out[, agg_level := 5]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,5))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,5)), 
                  lapply(.SD, mean, na.rm = T), 
                  .SDcols = vars_no_ed]
    out[, SOC := c.code]
    out[, agg_level := 4]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,4))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,4)), 
                  lapply(.SD, mean, na.rm = T), 
                  .SDcols = vars_no_ed]
    out[, SOC := c.code]
    out[, agg_level := 3]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,2))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,2)), 
                  lapply(.SD, mean, na.rm = T), 
                  .SDcols = vars_no_ed]
    out[, SOC := c.code]
    out[, agg_level := 2]
  }
  return(out)
}


new_codes <- lapply(bad_codes, the_restr) %>% rbindlist()


the_restr2 <- function(c.code){
  if(nrow(skills[SOC %like% paste0("^", substr(c.code,1,6))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,6)), 
                  lapply(.SD, median, na.rm = T), 
                  .SDcols = vars_ed]
    out[, SOC := c.code]
    out[, agg_level := 5]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,5))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,5)), 
                  lapply(.SD, median, na.rm = T), 
                  .SDcols = vars_ed]
    out[, SOC := c.code]
    out[, agg_level := 4]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,4))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,4)), 
                  lapply(.SD, median, na.rm = T), 
                  .SDcols = vars_ed]
    out[, SOC := c.code]
    out[, agg_level := 3]
  }else if (nrow(skills[SOC %like% paste0("^", substr(c.code,1,2))]) > 0){
    out <- skills[SOC %like% paste0("^", substr(c.code,1,2)), 
                  lapply(.SD, median, na.rm = T), 
                  .SDcols = vars_ed]
    out[, SOC := c.code]
    out[, agg_level := 2]
  }
  return(out)
}


new_codes_ed <- lapply(bad_codes, the_restr2) %>% rbindlist()

new_codes <- merge(unique(new_codes), unique(new_codes_ed), by = c("SOC", "agg_level"))

skills_all <- rbind(skills, new_codes, fill =T )

skills_all[, SOC_num := gsub("-", "", SOC)]


# some duplicates
skills_all_coll_no_ed <- skills_all[, lapply(.SD, mean, na.rm = T), .SDcols = vars_no_ed, by = SOC_num]
skills_all_coll_ed <- skills_all[, lapply(.SD, median, na.rm = T), .SDcols = vars_ed, by = SOC_num]

skills_all_coll <- merge(skills_all_coll_ed, skills_all_coll_no_ed, by = c( "SOC_num"))

#skills_occ_census
skills_all_coll_census <- merge(skills_all_coll, occ_2012_soc_merge, "SOC_num")

# save crosswalk
soc_occ2000 <- skills_all_coll_census[,.(SOC = SOC[1]), by = .(OCC2000)]

skills_final_no_ed <- skills_all_coll_census[,lapply(.SD, mean, na.rm = T), .SDcols = vars_no_ed, by = .(OCC2000, occ_title, OCC1950, OCC1950_name)]
skills_final_ed <- skills_all_coll_census[,lapply(.SD, median, na.rm = T), .SDcols = vars_ed, by = .(OCC2000, occ_title, OCC1950, OCC1950_name)]

skills_final <- merge(skills_final_ed, skills_final_no_ed, by= c("OCC2000", "occ_title", "OCC1950", "OCC1950_name"))

# subset to only occs in the data

skills_arch <- copy(skills_final)
skills <- copy(skills_final)



#
# out <- psych::fa.parallel(x= cor(skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV"]]), fm="minres", fa="fa", n.obs = nrow(skills))
# 
# data.table(component = 1:length(out$fa.values), fa_values = out$fa.values, fa_sim_data = out$fa.sim) %>% 
#   .[1:16] %>% 
#   melt(id.vars = "component") %>% 
#   .[variable == "fa_sim_data", variable := "Simulated"] %>% 
#   .[variable == "fa_values", variable := "Observed"] %>% 
#   ggplot() +
#   geom_point(aes(x = component, y = value, shape = variable, color = variable))  + 
#   geom_line(aes(x = component, y = value, color = variable, linetype = variable))  + 
#   scale_linetype_manual(values = 2:4) + 
#   scale_color_viridis_d(begin = .4, end = .8) +
#   scale_y_continuous(trans = "log10", limits = c(0.1, 100)) + 
#   scale_x_continuous(breaks = seq(0,20, 1))  +
#   labs(x = "Factor Analysis Component", y = "Eigenvalue", 
#        title = "Comparison of Factor Analysis Observed Eigenvalues and\nThose of A Randomly Generated Dataset of the Same Size", color = "", shape = "", linetype = "") + 
#   theme(legend.position = "bottom", 
#         legend.text = element_text(size = 8)) -> gg
# 





###
# # label factors
fact_anal_orig <- psych::fa(skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV"]], nfactors = 10, max.iter = 100000, scores = "regression", rotate = "varimax")

# append to data
fact_scores <- fact_anal_orig$scores %>% data.table()
setnames(fact_scores, names(fact_scores), gsub("MR", "fact_", names(fact_scores)))
skills <- cbind(skills, fact_scores)


######################################
#collapse to microocc
########################################
###
cheng <- data.table(readstata13::read.dta13("../ref/occ1950_mc_xwalk_70.dta"))
cheng[, occ1950 := gsub("[^A-Za-z0-9]", "", tolower(occ1950))]
# occ50 recode
occ50_recode <- fread("../ref/occ1950_recode.csv")
occ50_recode <- occ50_recode[,1:2]
names(occ50_recode) <- unlist(occ50_recode[1, ])
occ50_recode <- occ50_recode[-1,]
occ50_recode <- occ50_recode[!is.na(as.numeric(occ1950_num))]
occ50_recode[, occ1950 := gsub("[^A-Za-z0-9]", "", tolower(occ1950))]
# merge
cheng <- merge(cheng, occ50_recode[,.(occ1950, occ1950_num)], all.x = T)
#fix the stragglers
fix <- cheng[is.na(occ1950_num)]
candidates <- occ50_recode[!occ1950 %in% cheng$occ1950]
for(c.fix in 1:nrow(fix)){
  goal <- substr(fix[c.fix, occ1950],1,7)
  new <- candidates[candidates$occ1950 %like% goal, occ1950_num]
  if(length(new) == 1){fix[c.fix, new_occ1950_num := new]}
}
fix[is.na(new_occ1950_num), new_occ1950_num := c(43, 34, 44, 603, 16, 46, 605,94, 19,48,69,84,26,23,27,29)]
cheng <- merge(cheng, fix[,.(occ1950, new_occ1950_num)], by = "occ1950", all.x = T)
cheng[is.na(occ1950_num), occ1950_num := new_occ1950_num]
cheng[, occ1950_num := as.numeric(occ1950_num)]
cheng[, new_occ1950_num := NULL]
setnames(cheng, "occ1950_num", "OCC1950")
cheng[, occ1950 := NULL]
factorr <- function(x){ifelse(is.character(x), return(factor(x)), return(x))}
cheng[,names(cheng) := lapply(.SD, factorr), .SDcols = names(cheng)]
##

cheng[, OCC1950 := as.character(OCC1950)]
skills <- merge(skills, cheng, by = "OCC1950", all.x = T)

########################################
factors <-fact_anal_orig$scores %>% data.table()
facys <- data.frame(matrix(as.numeric(fact_anal_orig$loadings), attributes(fact_anal_orig$loadings)$dim, dimnames=attributes(fact_anal_orig$loadings)$dimnames))

# make a little figure showing top 5 traits per factor
get_vars <- function(df, x, n){
  df <- data.table(df, keep.rownames = T)
  y <- df[, get(x)] %>% abs() %>% order(decreasing = T)
  signs <-  df[, get(x)][y] %>% sign()
  signs <- ifelse(signs == -1, "---", "")
  out <- df[, rn][y][1:n]
  out <- paste0(signs[1:n], out, signs[1:n])
  out <- gsub( ".LV", "", out)
  return(out)
}

table3 <- lapply(names(facys)[!names(facys) %like% "rn"], get_vars, df = facys, n = 10)
table3 <- lapply(table3, function(x){paste0(x, collapse = "; ")})




factors_names <- c(paste0("fact_", 1:10,"_",
                          c("Problem Solving", 
                            "Manual Tasks/Hazardous Work",
                            "Concern/Caringness/Self Control",
                            "Spatial Orientation/Driving",
                            "Exactitude/Repition",
                            "Management/Human Resources",
                            "Innovation/Creativity",
                            "Engineering",
                            "Coordination/Cooperation",
                            "Construction")))

factors_names <- paste0(factors_names, "_fac")
setnames(skills, names(skills)[names(skills) %like% "fact_"], factors_names)


# make that table now
maxes <- lapply(factors_names, function(x){skills[which.max(get(x)), occ_title]})
mins <- lapply(factors_names, function(x){skills[which.min(get(x)), occ_title]})

table_out3 <- data.table(Factor = factors_names, 
                         `Top 5 Components` = table3, 
                         `Maximum Occupation` = maxes, 
                         `Minimum Occupation` = mins)

#saveRDS(table_out3, "../occs_exemps.rds")

#stargazer::stargazer(table_out3, summary = F, out = "../table3.html")


# merge occupation data from census
setwd("/Users/hyork/Dropbox (Princeton)/projects/psid")

SEI <- fread("sup/usa_00039.csv")
SEI <- SEI[,.(OCC, HWSEI)] %>% unique()
skills[, OCC2000 := as.integer(OCC2000)]
SEI[, OCC := as.integer(OCC)]

skills <- merge(skills, SEI, all.x = T, by.x = "OCC2000",by.y = "OCC")
soc_occ2000[, OCC2000 := as.integer(OCC2000)]
skills <- merge(skills, soc_occ2000)


saveRDS(skills[,.SD, .SDcols = names(skills)[names(skills) %like% "fact|OCC|occ|SEI|SOC"]],
        "sup/skills_xwalked.rds")


saveRDS(skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV$|OCC2000"]],
        "sup/skills_xwalked_no_factor.rds")


skills[,.SD, .SDcols = names(skills)[names(skills) %like% "OCC|occ|SOC"]] %>% .[!duplicated(OCC2000)] %>% 
  saveRDS(.,
          "sup/occ_hierarchy.rds")

