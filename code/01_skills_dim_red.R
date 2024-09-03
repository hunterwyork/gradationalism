library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(haven)
library(data.table)
library(ggpubr)
library(readr)
library(psych)
library(readstata13)

theme_set(theme_bw(base_size = 8, base_family = "serif")) 


# load mobility data, but just to know which occupations are present in the dataset to be analyzed
gen <- read_dta( "../inputs/pooled_data(1).dta") %>% data.table()
moms <- gen[!is.na(mmicroocc)]
gen[, parent_sex := 1]
moms[, parent_sex := 2]
moms[, focc1950 := mocc1950]
moms[, fmicroocc := mmicroocc]
moms[, focc_rank := mocc_rank]
#moms[, fa_edu := mo_edu]
moms[, fbirthyear := mbirthyear]
moms[, fcohort := mcohort]
gen <- rbind(gen, moms)


temp <- names(gen)

# subset
#gen <- gen[birthyear > 1950]


# load occ_soc xwalk
occ_xwalk <- data.table(read_excel("../ref/nem-occcode-cps-crosswalk.xlsx"))
names(occ_xwalk) <- occ_xwalk[4,] %>% unlist()
occ_xwalk <- occ_xwalk[-(1:4),]
occ_xwalk[, OCCSOC := gsub("-", "", `Hybrid SOC Code`)]



######################################################################################
################ now create skills dataset ####################################
######################################################################################
# load each file separately, clean, append, and standardize

skills_2018 <- read_excel("../ref/db_27_0_excel/Skills.xlsx") %>% data.table()
skills_2018[, year := 2018]
setnames(skills_2018, names(skills_2018), gsub(" ", ".", names(skills_2018), fixed = T))
skills_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
skills1 <- rbindlist(list( skills_2018), fill = T)
skills1 <- skills1[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
skills1[, Element.Name := paste0("skl_", Element.Name)]


# add abilities
abilities_2018 <- read_excel("../ref/db_27_0_excel/Abilities.xlsx") %>% data.table()
abilities_2018[, year := 2018]
setnames(abilities_2018, names(abilities_2018), gsub(" ", ".", names(abilities_2018), fixed = T))
abilities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
abilities <- rbindlist(list( abilities_2018), fill = T)
abilities <- abilities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
abilities[, Element.Name := paste0("abl_", Element.Name)]


# add knowledge
knowledge_2018 <- read_excel("../ref/db_27_0_excel/Knowledge.xlsx") %>% data.table()
knowledge_2018[, year := 2018]
setnames(knowledge_2018, names(knowledge_2018), gsub(" ", ".", names(knowledge_2018), fixed = T))
knowledge_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
knowledge <- rbindlist(list(knowledge_2018), fill = T)
knowledge <- knowledge[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
knowledge[, Element.Name := paste0("knl_", Element.Name)]


# add work activities
workactivities_2018 <- read_excel("../ref/db_27_0_excel/Work Activities.xlsx") %>% data.table()
workactivities_2018[, year := 2018]
setnames(workactivities_2018, names(workactivities_2018), gsub(" ", ".", names(workactivities_2018), fixed = T))
workactivities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workactivities <- rbindlist(list( workactivities_2018), fill = T)
workactivities <- workactivities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workactivities[, Element.Name := paste0("act_", Element.Name)]
# add work styles

workstyles_2018 <- read_excel("../ref/db_27_0_excel/Work Styles.xlsx") %>% data.table()

workstyles_2018[, year := 2018]
setnames(workstyles_2018, names(workstyles_2018), gsub(" ", ".", names(workstyles_2018), fixed = T))
workstyles_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workstyles <- rbindlist(list(workstyles_2018), fill = T)
workstyles <- workstyles[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workstyles[, Element.Name := paste0("sty_", Element.Name)]
# add work context

context_2018 <- read_excel("../ref/db_27_0_excel/Work Context.xlsx") %>% data.table()
context_2018[, year := 2018]
setnames(context_2018, names(context_2018), gsub(" ", ".", names(context_2018), fixed = T))
context_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
context <- rbindlist(list( context_2018), fill = T)
context <- context[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
context <- context[Scale.ID %in% c("CX", "CT")]
context[, Element.Name := paste0("ctx_", Element.Name)]
# add work values
values_2018 <- read_excel("../ref/db_27_0_excel/Work Values.xlsx") %>% data.table()
values_2018[, year := 2018]
setnames(values_2018, names(values_2018), gsub(" ", ".", names(values_2018), fixed = T))
values_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
values <- rbindlist(list(values_2018), fill = T)
values <- values[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
values <- values[Scale.ID %in% c("EX")]
values[, Element.Name := paste0("vlu_", Element.Name)]
# add education training, etc

education_2018 <- read_excel("../ref/db_27_0_excel/Education, Training, and Experience.xlsx") %>% data.table()

education_2018[, year := 2018]
setnames(education_2018, names(education_2018), gsub(" ", ".", names(education_2018), fixed = T))
education_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
education <- rbindlist(list( education_2018), fill = T)
education <- education[,.(Data.Value = stats::weighted.mean(Category, Data.Value)), by = .(O.NET.SOC.Code, Element.Name, Scale.ID, year)]
education <- education[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
education[, Element.Name := paste0("edu_", Element.Name)]


# collapse to one object called skills
skills <- rbindlist(list(skills1, knowledge, abilities, workstyles, 
                         workactivities,context, values, education), fill = T)


# standardize so that each scale is between 0 and 1
skills[Scale.ID == "LV", Data.Value := Data.Value/7]
skills[Scale.ID == "EX", Data.Value := (Data.Value-1)/6]
skills[Scale.ID == "IM", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CX", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CT", Data.Value := (Data.Value-1)/2]
skills[Scale.ID == "RW", Data.Value := (Data.Value-1)/9]
skills[Scale.ID == "PT", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "OJ", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "RL", Data.Value := (Data.Value-1)/11]

# keep different scale id for four domains, which are missing level
skills[Element.Name %like% "sty_", Scale.ID := "LV"]
skills[Element.Name %like% "ctx", Scale.ID := "LV"]
skills[Element.Name %like% "vlu", Scale.ID := "LV"]
skills[Element.Name %like% "edu_", Scale.ID := "LV"]
#
skills <- skills[Scale.ID == "LV"]
skills[, OCCSOC := gsub("-", "", substr(O.NET.SOC.Code,1,7))]
skills[, Standard.Error := as.numeric(Standard.Error)]
skills[, Element.Name := paste0(Element.Name, ".", Scale.ID)]

saveRDS(skills, "../ref/skills_unedited.rds")


# collapse to a smaller dataset
skills <- skills[, .(Data.Value  = mean(Data.Value, na.rm = T), 
                     Standard.Error = mean(Standard.Error, na.rm = T)), # throw away SE anyway
                 by = .(Element.Name, OCCSOC)]

# exclude two education/credential characteristics with missing values
skills <- skills[!Element.Name %like% "Job-Related|Apprent"]
skills <- skills[!OCCSOC %in% skills[is.na(Data.Value)]$OCCSOC]

# cast wide
skills_wide <- dcast(skills, OCCSOC   ~Element.Name, value.var = "Data.Value")

# for fishers and hunters, edu variables are missing, interpelate with zero
skills_wide[OCCSOC == 453031, `edu_On-Site or In-Plant Training.LV` := 0]
skills_wide[OCCSOC == 453031, `edu_On-the-Job Training.LV` := 0]
skills_wide[OCCSOC == 453031, `edu_Related Work Experience.LV` := 0]
skills_wide[OCCSOC == 453031, `edu_Required Level of Education.LV` := 0]

skills_wide <- skills_wide[!OCCSOC %in% skills_wide[!complete.cases(skills_wide)]$OCCSOC]

skills <- copy(skills_wide)

skills <- merge(skills, occ_xwalk[,.(OCCSOC, `CPS Code`, `CPS Occupational Title`)], all.x = T)


######################################################################################
################ Crosswalk, which is a daunting task #################################
######################################################################################

### source handwritten code to do the crosswalk
source("99_crosswalk_code.R")

# reduce to observations with more than 10 cases
gen[,.N, by = occ1950] %>% 
  merge(., gen[,.N, by = focc1950], by.x = "occ1950", by.y = "focc1950") %>%
  .[, N := N.x + N.y] %>%
  .[, occ1950 := as.character(occ1950)] %>% 
  merge(., skills[,.(OCC1950, OCC1950_name)], by.x = "occ1950", by.y = "OCC1950") %>% .[order(N)] %>%
  .[N.x < 10|N.y < 10, occ1950] %>% unique() -> excl

skills <- skills[!OCC1950 %in% excl]


skills[,.(OCC1950, OCC1950_name, OCCSOC, `CPS Occupational Title`)] %>% unique() %>% .[!is.na(OCC1950)] -> xwalk_occs_soc


# save crosswalk for later use
fwrite(xwalk_occs_soc, "../ref/xwalks_occ_soc.csv")




vars <- names(skills)[names(skills) %like% ".LV|fact_"]
skills[, OCC1950_name := unique(OCC1950_name)[1], by = OCC1950]
skills <- skills[,lapply(.SD, mean), .SDcols = vars, by = .(OCC1950, OCC1950_name)]

# subset to only occs in the data. This drops one NA row.
skills <- skills[OCC1950 %in% gen$occ1950 | OCC1950 %in% gen$focc1950]

skills_arch <- copy(skills)

######################################################################################
################ Conduct Factor Analysis ####################################
######################################################################################

# run parallel analysis to choose number of factors

out <- psych::fa.parallel(x= cor(skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV"]]), fm="minres", fa="fa",
                          n.obs = nrow(skills),sim = F, n.iter = 1 )

# extract values for parallel analysis

data.table(component = 1:length(out$fa.values), fa_values = out$fa.values, fa_sim_data = out$fa.sim) %>% 
  .[1:16] %>% 
  melt(id.vars = "component") %>% 
  .[variable == "fa_sim_data", variable := "Simulated"] %>% 
  .[variable == "fa_values", variable := "Observed"] %>% 
  ggplot() +
  geom_point(aes(x = component, y = value, shape = variable, color = variable))  + 
  geom_line(aes(x = component, y = value, color = variable, linetype = variable))  + 
  scale_linetype_manual(values = 2:4) + 
  scale_color_viridis_d(begin = .4, end = .8) +
  scale_y_continuous(trans = "log10", limits = c(0.1, 100)) + 
  scale_x_continuous(breaks = seq(0,20, 1))  +
  labs(x = "Factor Analysis Component", y = "Eigenvalue", 
       # title = "Comparison of Factor Analysis Observed Eigenvalues and\nThose of A Randomly Generated Dataset of the Same Size", 
       color = "", shape = "", linetype = "") + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8)) -> gg

dir.create("../outputs")

pdf("../outputs/appendix_figure_a2.pdf", width = 4.3, height = 3.3)
print(gg)
dev.off()

tiff("../outputs/appendix_figure_a2.tiff", width = 4.3, height = 3.3, res = 800, units = 'in')
print(gg)
dev.off()





###
# # Rerun FA using N factors (N = 10)
fact_anal_orig <- psych::fa(skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV"]], nfactors = 10, max.iter = 100000,
                            scores = "regression", rotate = "varimax")

# append scores to data

# on some machines this leads to different outcomes. I haven't figured out why, but it is reproducible on my machine.
fact_scores <- fact_anal_orig$scores %>% data.table()
setnames(fact_scores, names(fact_scores), gsub("MR", "fact_", names(fact_scores)))

skills <- cbind(skills, fact_scores)


# add in status and prestige - THese are gotten from an IPUMS extract

SEI <- fread("../ref/usa_00022.csv")
SEI <- SEI[,.(OCC1950, PRESGL, SEI)] %>% unique()
setnames(SEI, c("PRESGL"), c( "prestige"))
SEI[, OCC1950 := as.character(OCC1950)]
skills[, OCC1950 := as.character(OCC1950)]
skills <- merge(skills, SEI, all.x = T, by = "OCC1950")

# do QR decomposition to orthogonalize SEI
skills_subset <- skills[, .SD, .SDcols = names(skills)[names(skills) %like% "fact_|SEI"]]
skills_subset$SEI <- scale(skills_subset$SEI)
qr.Q(qr(skills_subset)) %>% data.table()-> skills_subset_norm

skills$SEI_norm <- skills_subset_norm$V11


######
extract_cum_variance <- function (x, digits = 3L, cutoff = 0.1, sort = FALSE, ...) 
{
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1) 
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  return(varex)
}


loadings <- extract_cum_variance(fact_anal_orig$loadings)

# load back in characteristics' original names
alt_names <- fread("../ref/skills_xwalk.csv")
setnames(skills, paste0(alt_names$Element.Name2[paste0(alt_names$Element.Name2, ".LV") %in% names(skills)], ".LV"), 
         paste0(alt_names$Element.Name[paste0(alt_names$Element.Name2, ".LV") %in% names(skills)], ".LV"), skip_absent = T)
cps_codes <- skills$`CPS Code`

#######################################################################################
################ Append data on Macro/Meso/Micro Occs #################################
######################################################################################
mes_mac_mic <- data.table(readstata13::read.dta13("../ref/occ1950_mc_xwalk_70.dta"))
mes_mac_mic[, occ1950 := gsub("[^A-Za-z0-9]", "", tolower(occ1950))]


# occ50 recode
occ50_recode <- fread("../ref/occ1950_recode.csv")
occ50_recode <- occ50_recode[,1:2]
names(occ50_recode) <- unlist(occ50_recode[1, ])
occ50_recode <- occ50_recode[-1,]
occ50_recode <- occ50_recode[!is.na(as.numeric(occ1950_num))]
occ50_recode[, occ1950 := gsub("[^A-Za-z0-9]", "", tolower(occ1950))]
# merge
mes_mac_mic <- merge(mes_mac_mic, occ50_recode[,.(occ1950, occ1950_num)], all.x = T)


#fix the stragglers
fix <- mes_mac_mic[is.na(occ1950_num)]
candidates <- occ50_recode[!occ1950 %in% mes_mac_mic$occ1950]

# there are some orthographic differences
# just do a string match to find the nearest match and then clean up mismatches
for(c.fix in 1:nrow(fix)){
  goal <- substr(fix[c.fix, occ1950],1,7)
  new <- candidates[candidates$occ1950 %like% goal, occ1950_num]
  if(length(new) == 1){fix[c.fix, new_occ1950_num := new]}
}
fix[is.na(new_occ1950_num), new_occ1950_num := c(43, 34, 44, 603, 16, 46, 605,94, 19,48,69,84,26,23,27,29)]
mes_mac_mic <- merge(mes_mac_mic, fix[,.(occ1950, new_occ1950_num)], by = "occ1950", all.x = T)
mes_mac_mic[is.na(occ1950_num), occ1950_num := new_occ1950_num]
mes_mac_mic[, occ1950_num := as.numeric(occ1950_num)]
mes_mac_mic[, new_occ1950_num := NULL]
setnames(mes_mac_mic, "occ1950_num", "OCC1950")
mes_mac_mic[, occ1950 := NULL]
factorr <- function(x){ifelse(is.character(x), return(factor(x)), return(x))}
mes_mac_mic[,names(mes_mac_mic) := lapply(.SD, factorr), .SDcols = names(mes_mac_mic)]
##

mes_mac_mic[, OCC1950 := as.character(OCC1950)]
skills <- skills[!is.na(OCC1950)]
skills <- merge(skills, mes_mac_mic, by = "OCC1950", all.x = T)


######################################################################################
################ Create table showing exemplary occupations ##########################
################ and defining characteristics for each factor ########################
######################################################################################

factors <-fact_anal_orig$scores %>% data.table()
facys <- data.frame(matrix(as.numeric(fact_anal_orig$loadings), attributes(fact_anal_orig$loadings)$dim, dimnames=attributes(fact_anal_orig$loadings)$dimnames))

# make a little figure showing top N traits per factor
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

# return top five traits per factor
table3 <- lapply(names(facys)[!names(facys) %like% "rn"], get_vars, df = facys, n = 5)
table3 <- lapply(table3, function(x){paste0(x, collapse = "; ")})


# give them all names!

factors_names <- c(paste0("fact_", 1:10,"_",
                          c("Problem Solving", 
                            "Precision/Operation/Quality Control",
                            "Concern/Caringness/Self Control",
                            "Attention to Detail/Repetition",
                            "Finance/Management",
                            "Spatial Orientation/Transportation",
                            "Innovation/Originality/Persistence",
                            "Construction/Installation",
                            "Judgment/Interpretation",
                            "Responsibility/Leadership")))

factors_names <- paste0(factors_names, "_fac")
setnames(skills, names(skills)[names(skills) %like% "fact_"], factors_names)


# make that table now an dsave for later
maxes <- lapply(factors_names, function(x){skills[(order(get(x), decreasing = T)), paste0(OCC1950_name[1], collapse = "; ")]})
mins <- lapply(factors_names, function(x){skills[(order(get(x))), paste0(OCC1950_name[1], collapse = "; ")]})

table_out3 <- data.table(Factor = factors_names, 
                         `Top 5 Components` = table3, 
                         `Maximum Occupation` = maxes, 
                         `Minimum Occupation` = mins)

saveRDS(table_out3, "../occs_exemps.rds")

stargazer::stargazer(table_out3, summary = F, out = "../table3.html")




skills <-skills[,lapply(.SD, mean), by = .(OCC1950, OCC1950_name, microocc, mesoocc, macroocc), .SDcols = names(skills)[names(skills) %like% ".LV|fact|PC|SEI|prestige|SEI"]]


# standardize SEI and PRESTIGE to also be 0-1

skills[, prestige := prestige/100]
skills[, SEI := SEI/100]

#
skills_attrs <- skills[,.SD, .SDcols = names(skills)[names(skills) %like% ".LV|fact|PC|median|percent_bl|percent_f|SEI|prestige|percent_se"]]

######################################################################################
################ Make figure for cumulative variance explained #######################
######################################################################################


data.table(loadings, keep.rownames = T) %>% 
  setnames(., c("Indicator", paste0(1:10, ": ", tstrsplit(factors_names, "_", keep = 3)[[1]]))) %>% 
  melt(id.vars = "Indicator") %>% 
  .[Indicator == "Cumulative Var", Indicator := "Cumulative Variance"] %>% 
  .[Indicator == "Cumulative Variance"] %>% 
  ggplot() +
  geom_point(aes(x = variable, y = value), shape = 1)  + 
  geom_line(aes(x = variable, y = value, group = Indicator), linetype = 3)  + 
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1), labels = scales::percent) + 
  labs(x = "Factor Analysis Component", y = "Cumulative Variance Explained", 
       # title = "Cumulative Variance Explained by Each Successive Latent Factor",
       color = "", shape = "", linetype = "") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8))  -> gg

#gg
pdf("../outputs/appendix_figure_a1.pdf", width = 4.3, height = 3.3)
print(gg)
dev.off()

tiff("../outputs/appendix_figure_a1.tiff", width = 4.3, height = 3.3, res = 800, units = 'in')
print(gg)
dev.off()





######################################################################################
################ Make figure for cumulative variance explained #######################
######################################################################################

# create function to ensure factor variables are  between 0-1 in graph
standardizer <- function(x){
  (x - min(x))/(max(x) - min(x))
}


skills_temp <- copy(skills[,.SD, .SDcols = names(skills)[names(skills) %like% "^fact_|SEI" & !names(skills) %like% "norm$"]] )

# make some line break changes for graph purposes

names(skills_temp) <- gsub("fact_", "", names(skills_temp))
names(skills_temp) <- gsub("_fac", "", names(skills_temp))
names(skills_temp) <- gsub("_", ": ", names(skills_temp))
names(skills_temp) <- gsub("_", ": ", names(skills_temp))
names(skills_temp) <- gsub("ory/Inn", "ory\nInn", names(skills_temp))
names(skills_temp) <- gsub("ail/Tim", "ail\nTim", names(skills_temp))
names(skills_temp) <- gsub("ity/Pat", "ity\nPat", names(skills_temp))
names(skills_temp) <- gsub("ers/Str", "ers\nStr", names(skills_temp))
names(skills_temp) <- gsub("ing/Mon", "ing\nMon", names(skills_temp))

gg <- skills_temp %>% 
  melt(., id.vars = "SEI") %>% 
  .[variable != "SEI: norm"] %>% 
  #  .[, value := scale(value), by= .(variable)] %>% 
  .[, value := standardizer(value), by= .(variable)] %>% 
  ggplot() +
  geom_hline(yintercept = .5, linetype = 2, color = "blue") + 
  geom_point(aes(x = SEI, y = value), shape = 1, size = .9, alpha = .7) + 
  geom_smooth(aes(x = SEI, y = value), method = "lm", se = T, color = "red",fill = "red", alpha = .25, linetype = "dotted")+
  facet_wrap(~variable) + 
  ggtitle("Scatters of Factor Variables and SEI by Occupation") +
  theme_bw(base_size = 10, base_family = "serif")+
  theme(panel.spacing = unit(1, "lines")) + 
  labs(title = "", x = "SEI", y = "Scaled Value")+ 
  stat_cor(aes(x = SEI, y = value, label = after_stat(r.label)), color = "red", geom = "label",label.x.npc = .73, label.y.npc = .06, #label.x = .55, label.y = .06, 
           size = 2.5)

pdf("../outputs/appendix_figure_a3.pdf", height= 6, width = 10)
print(gg)
dev.off()

tiff("../outputs/appendix_figure_a3.tiff", width = 10, height = 6, res = 800, units = 'in')
print(gg)
dev.off()


# save output
saveRDS(skills, "../ref/skills_occ1950_new.rds")


