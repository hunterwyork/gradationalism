library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(haven)
library(data.table)
library(gnm)
library(ggthemes)
library(kableExtra)
library(ggh4x)
library(lmtest)

# ggplot settings
theme_set(theme_bw(base_size = 8, base_family = "serif")) 

# set an arbitrary model name
model_version <- "20240820_submission_final"

# load data and structure so that there is an independent line for every parent child observation
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

# load data on occupational characteristics, crosswalked to 1950 occ codes

skills <- readRDS("../ref/skills_occ1950_new.rds")
temp <- names(skills)[!names(skills) %like% "OCC195|micro|meso|macro|percent"]

# create function to standardize between 0 and 1
standardizer <- function(x){
  (x - min(x))/(max(x) - min(x))
}

skills[, (temp) :=  lapply(.SD, standardizer), .SDcols =temp]

#####################################

# collapse to count data by parent/child occ cells
counts <- gen[,.N, by = .(focc1950,occ1950, sex, parent_sex)]
counts[, focc1950:= as.numeric(focc1950)]
counts[,occ1950:= as.numeric(occ1950)]
counts[, sex:= as.numeric(sex)]
counts[, parent_sex:= as.numeric(parent_sex)]


# ensure mobility  table is fully square by adding 0s for missing cells
new_counts <- expand.grid(occ1950 = unique(counts$occ1950),
                          focc1950 = unique(counts$focc1950),
                          sex = 1:2,
                          # fa_edu_recode = unique(counts$fa_edu_recode),
                          #edu_recode = unique(counts$edu_recode),
                          parent_sex = 1:2) %>% 
  data.table()
counts <- merge(counts, new_counts, by = names(new_counts), all = T)
counts[is.na(N), N := 0]

# don't include military codes
counts <- counts[!occ1950 %in% c(999,997,995,595) & occ1950 < 979]
counts <- counts[!focc1950 %in% c(999,997,995,595) & focc1950 < 979]


# save variable of occ chars
vars <- names(skills)[(!names(skills) %like% "OCC|occ")]

skills[, OCC1950 := as.numeric(OCC1950)]

# merge mobility table with child characteristics
counts <- merge(counts, skills[,.SD, .SDcols = c("OCC1950", vars, 
                                                 "microocc", "mesoocc", "macroocc")], by.x = "occ1950", by.y = "OCC1950")

# merge again, this time with parent characteristics
# NB I often refer to parents as fathers, due to comon nomenclature for mobility tables
father_skills <- copy(skills)
setnames(father_skills, names(father_skills), paste0(names(father_skills), "_f"))
counts <- merge(counts, father_skills[,.SD, .SDcols = c("OCC1950_f", paste0(vars, "_f"), 
                                                        "microocc_f", "mesoocc_f", "macroocc_f")], by.x = "focc1950", by.y = "OCC1950_f")

########### merge on gen
gen <- merge(gen, skills[,.SD, .SDcols = c("OCC1950", vars, "microocc", "mesoocc", "macroocc")], by.x = "occ1950", by.y = "OCC1950")
father_skills <- copy(skills)
setnames(father_skills, names(father_skills), paste0(names(father_skills), "_f"))
gen <- merge(gen, father_skills[,.SD, .SDcols = c("OCC1950_f", paste0(vars, "_f"), 
                                                  "microocc_f", "mesoocc_f", "macroocc_f")], by.x = "focc1950", by.y = "OCC1950_f")


# create table of input data
gen[, datasource_char := haven::as_factor(gen$datasource)]
table2 <- gen[,.("Father-Son Pairs" = length(parent_sex[parent_sex == 1 & sex == 1]),
                 "Mother-Daughter Pairs" = length(parent_sex[parent_sex == 2 & sex == 2]),
                 "Father-Daughter Pairs" = length(parent_sex[parent_sex == 1 & sex == 2]),
                 "Mother-Son Pairs" = length(parent_sex[parent_sex == 2 & sex == 1]),
                 "Unique Occupations" = length(unique(occ1950)),
                 "Birth Cohort Span" = paste0(min(birthyear), "-", max(birthyear))), by = datasource_char]
table2 <- table2[order(datasource_char, decreasing = F)]
table2 <- table2 %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum)))
table2[, datasource_char := as.character(datasource_char)]
setnames(table2, "datasource_char", "Datasource")
table2[is.na(table2)] <- ""
table2[nrow(table2), Datasource := "Totals"]
table2[Datasource == "Totals", `Unique Occupations` := length(unique(gen$occ1950))]
table2[Datasource == "Totals", `Birth Cohort Span` := paste0(min(gen$birthyear), "-", max(gen$birthyear))]
fwrite(table2, "../ref/appendix_table_a1.csv")

######################################################
##### Regress
######################################################
# create dummy indicators for preserved micro/meso/macro/occ_1950 class across generations (diagonals)
# micro_c_neg is reverse indicator
counts[,micro_c :=  ifelse(microocc == microocc_f, 1, 0)]
counts[,micro_c_neg :=  ifelse(microocc == microocc_f, 0,1)]
counts[,macro_c :=  ifelse(macroocc == macroocc_f, 1, 0)]
counts[,meso_c :=  ifelse(mesoocc == mesoocc_f, 1, 0)]
counts[,occ1950_c :=  ifelse(occ1950 == focc1950, 1, 0)]

# save list of variable names
vars_list <- list( c(vars[vars %like% "^PC"]),
                   c(vars[vars %like% paste0(paste0(paste0("^fact_", 1:10, "_"),
                                                    collapse = "|")) & ! vars %like% "rank$"]))
vars_list[[2]] <- c( vars_list[[2]])

c.vars <- vars_list[[2]]
saveRDS(c.vars,"../ref/c.vars.RDS")


# create function to determine pseudo_IGE of all variables for each parent child gender combo
iger <- function(c.var, c.sex, c.parent_sex){
  print(c.var)
  # lm(as.formula(paste0("`", c.var, "` ~  ","`", c.var, "_f` + fact_20_SEI_f" )), gen[parent_sex == c.parent_sex & sex == c.sex]) %>%
  #   summary() %>% coefficients() %>% data.table(keep.rownames = T)-> out
  lm(as.formula(paste0("`", c.var, "` ~  ","`", c.var, "_f`" )), gen[parent_sex == c.parent_sex & sex == c.sex]) %>%
    summary() %>% coefficients() %>% data.table(keep.rownames = T)-> out
  
  out[, var := c.var]
  out[, sex := c.sex]
  out[, parent_sex := c.parent_sex]
  out <- out[c(1,2),]
  return(out)
}

#add a new name for SEI just to keep consistant
gen[, fact_20_SEI := SEI]
gen[, fact_20_SEI_f := SEI_f]

mapply_grid <- expand.grid(var = c(c(c.vars, "fact_20_SEI")), 
                           c.sex = 1:2, 
                           c.parent_sex = 1:2) %>% data.table()

mapply_out <- mapply(iger, c.var = mapply_grid$var, c.sex = mapply_grid$c.sex, c.parent_sex = mapply_grid$c.parent_sex, SIMPLIFY = F)

all_out <- rbindlist(mapply_out) 

all_out <- all_out[rn != "(Intercept)"]
all_out[, Variable := gsub("`", "", rn)]
all_out[, Variable := gsub("_fac_f", "", Variable)]
all_out[, Variable := gsub("_fac_rank_f", "", Variable)]
all_out[, Variable := gsub("_f$", "", Variable)]

all_out[, Variable := gsub("fact_", "", Variable)]
all_out[, Variable := gsub("_rank", "", Variable)]
all_out[, Variable := gsub("_", ": ", Variable)]
all_out[, Variable := gsub("20: ", "", Variable)]
all_out[, Method := ifelse(rn %like% "rank", "Rank/Rank", "IGE")]

all_out[, `Parent/Child` := paste0(ifelse(parent_sex == 1, "Father", "Mother"), "/", ifelse(sex == 1, "Son", "Daughter"))]
all_out[, `Parent/Child` := factor(`Parent/Child`, levels = unique(as.character(`Parent/Child`))[c(1,2,3,4)])]
all_out[,Variable := factor(Variable, levels = c("SEI", unique(all_out$Variable)[order((as.numeric(substr(tstrsplit(unique(all_out$Variable), " ", keep = 2, type.convert = T)[[1]], 1,2))))][c(1:10)]))]

ggplot(all_out) + 
  geom_bar(aes(x = Variable, y = Estimate, fill = `Parent/Child`),width = .5, position = position_dodge(width = .5), 
           stat = "identity") + 
  facet_grid(~Method) + 
  coord_flip() + 
  ggthemes::scale_fill_colorblind() + 
  theme_bw(base_family =  "serif") + 
  theme(strip.background =element_rect(fill="white"))  +
  guides(fill = guide_legend(reverse = TRUE))


all_out[, upper := Estimate + 1.96*`Std. Error`]
all_out[, lower := Estimate - 1.96*`Std. Error`]
all_out[, con_ing := paste0("[", formatC(round(lower,3),format = "f",  digits = 3, flag = ""), ", ",
                            formatC(round(upper,3),format = "f",  digits = 3, flag = ""), "]")]
all_out[, estim := formatC(round(Estimate,3),format = "f",  digits = 3, flag = "")]

all_out <- all_out[Method == "IGE"]
all_out[, final := paste0(estim, "\\newline", con_ing)]
all_out <- all_out[order(Variable)]
all_out_wide <- dcast(all_out, Variable ~ `Parent/Child`, value.var = "final")

dir.create(paste0("../outputs/", model_version))

knitr::kable(all_out_wide,"latex",
             row.names = F, escape = F,
             align = c("l", rep("c", 4)),
             col.names = c("", "Father-Son", "Father-Daughter", "Mother-Son", "Mother-Daughter"), 
             booktabs = T,
             linesep = "") %>%
  add_header_above(c(" " =1, "Intergenerational Elasticity Among" = 4)) %>% 
  save_kable(file =  paste0("../outputs/",model_version, "/table_4.tex"))


# save counts as intermediate file for table 5 later
saveRDS(counts,   paste0("../ref/counts", model_version, ".Data"))





###
# loop over parent child gender combos and save models for each
###
for(c.sex in 1:2){
  for(c.parent_sex in 1:2){
    print(c.sex)
    print(c.parent_sex)
    
    
    ######################################################################################
    ################ Model with Mic/Mac/Meso Immobility ##################################
    ######################################################################################
    base_model_mic_mac_mes <- glm(data = counts[sex == c.sex & parent_sex == c.parent_sex],
                                  N ~ as.factor(occ1950) + as.factor(focc1950) +
                                    micro_c:as.factor(microocc) +
                                    macro_c:as.factor(macroocc) +
                                    meso_c:as.factor(mesoocc), family = "poisson", model = F)
    base_model_mic_mac_mes$data <- NULL
    
    
    ######################################################################################
    ################ Model with Mic/Mac/Meso Immobility and Status Imm/Mob ###############
    ######################################################################################
    base_model_mic_mac_mes_status <- glm(data = counts[sex == c.sex & parent_sex == c.parent_sex],
                                         N ~ as.factor(occ1950) + as.factor(focc1950) +
                                           micro_c:as.factor(microocc) +
                                           macro_c:as.factor(macroocc) +
                                           meso_c:as.factor(mesoocc) + 
                                           micro_c_neg:SEI:SEI_f + 
                                           micro_c:SEI:SEI_f, family = "poisson", model = F)
    base_model_mic_mac_mes_status$data <- NULL
    
    ######################################################################################
    ################ Model with Mes/Mac Immobility #######################################
    ######################################################################################
    
    base_model_mac_mes <- glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                              N ~ as.factor(occ1950) + as.factor(focc1950) +
                                macro_c:as.factor(macroocc) +
                                meso_c:as.factor(mesoocc), family = "poisson", model = F)
    base_model_mac_mes$data <- NULL
    
    ######################################################################################
    ################ Model with Mac/Meso Immobility and Status Imm/Mob ###################
    ######################################################################################
    
    base_model_mac_mes_status <- glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                                     N ~ as.factor(occ1950) + as.factor(focc1950) +
                                       macro_c:as.factor(macroocc) +
                                       meso_c:as.factor(mesoocc)+ 
                                       micro_c_neg:SEI:SEI_f + 
                                       micro_c:SEI:SEI_f, family = "poisson", model = F)
    base_model_mac_mes_status$data <- NULL
    
    ######################################################################################
    ################ Status and Scaling Imm/Mobility ####################################
    ######################################################################################
    
    base_model_vars_10 <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                               as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950)  + 
                                    micro_c_neg:SEI:SEI_f + 
                                    micro_c:SEI:SEI_f +",
                                                 paste0("micro_c:`",paste0(c.vars[1:10], "`:`",
                                                                           paste0(c.vars[1:10], "_f`")),
                                                        collapse = " + ") ,
                                                 " + ",
                                                 paste0("micro_c_neg:`",paste0(c.vars[1:10], "`:`",
                                                                               paste0(c.vars[1:10], "_f`")),
                                                        collapse = " + "))), family = "poisson", model = F)
    base_model_vars_10$data <- NULL
    
    
    ######################################################################################
    ################ Only Scaling Immobility/Mobility ####################################
    ######################################################################################
    
    base_model_vars_10_no_status <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                                         as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950)  +",
                                                           paste0("micro_c:`",paste0(c.vars[1:10], "`:`",
                                                                                     paste0(c.vars[1:10], "_f`")),
                                                                  collapse = " + ") ,
                                                           " + ",
                                                           paste0("micro_c_neg:`",paste0(c.vars[1:10], "`:`",
                                                                                         paste0(c.vars[1:10], "_f`")),
                                                                  collapse = " + "))), family = "poisson", model = F)
    base_model_vars_10_no_status$data <- NULL
    
    
    
    ######################################################################################
    ################ Model with only Status Imm/Mobility #################################
    ######################################################################################
    
    base_model_status <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                              N ~ as.factor(occ1950) + as.factor(focc1950) +
                                micro_c_neg:SEI:SEI_f + 
                                micro_c:SEI:SEI_f, family = "poisson", model = F)
    base_model_status$data <- NULL
    
    ######################################################################################
    ################ Model with Row Column Marginals Only ################################
    ######################################################################################
    
    base_model <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                       N ~ as.factor(occ1950) + as.factor(focc1950), family = "poisson", model = F)
    
    base_model$data <- NULL
    
    ######################################################################################
    ################ Model with Mic/Mes/Mac Immobility, Status, and Scaling Immobility/Moblity
    ######################################################################################
    
    base_model_mic_mac_mes_vars_10 <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                                           as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950) +
                                                micro_c:as.factor(microocc) +               
                                    macro_c:as.factor(macroocc) +
                                    meso_c:as.factor(mesoocc) + 
                                    micro_c_neg:SEI:SEI_f + 
                                    micro_c:SEI:SEI_f+",
                                                             paste0("micro_c:`",paste0(c.vars, "`:`",
                                                                                       paste0(c.vars, "_f`")),
                                                                    collapse = " + ") ,
                                                             " + ",
                                                             paste0("micro_c_neg:`",paste0(c.vars, "`:`",
                                                                                           paste0(c.vars, "_f`")),
                                                                    collapse = " + "))), family = "poisson", model = F)
    
    base_model_mic_mac_mes_vars_10$data <- NULL
    
    
    ##################################################################
    ##################################################################
    ### To fit a goodman RC2 model, I first run a model with Macro/Meso Immobility Effects
    ##################################################################
    ##################################################################
    
    #if(c.sex == 1 & c.parent_sex == 1){
    base_model_goodman_baseline <- gnm(data = counts[sex == c.sex& parent_sex == c.parent_sex & micro_c == 0],
                                       N ~ as.factor(occ1950) + as.factor(focc1950) +
                                         macro_c:as.factor(macroocc) +
                                         meso_c:as.factor(mesoocc),
                                       #Mult(micro_c, as.factor(occ1950):as.factor(focc1950)),
                                       #    Mult(micro_c_neg, as.factor(occ1950):as.factor(focc1950)),
                                       verbose = T,
                                       iterMax = 100)
    
    
    ##################################################################
    ##################################################################
    ### I then use the values from above as  starting values for the RC2 model:
    ##################################################################
    ##################################################################

    # specify number of dimensions
    dims <- 2
    
    # fit the model
    mult2 <- residSVD(base_model_goodman_baseline, as.factor(occ1950), as.factor(focc1950), d = dims)
    RC2model <- update(base_model_goodman_baseline, . ~ . + instances(Mult(as.factor(occ1950), as.factor(focc1950)), dims),
                       start = c(coef(base_model_goodman_baseline), mult2), trace = TRUE, iterMax = 100)

    ##################################################################
    ##################################################################
    # because this model calculates log likelihood weirdly (DF reasons), 
    # refit the model using fittted coefficients
    # I extract the row and column fitted values for the latent variables
    # and I extract the effect sizes (phi/psi in the paper)
    # and I run the model normally, so that DF is calculated in a comparable way to other models
    # even though in reality more DF are used because each row/column expends a DF to estimate
    # the latent variable value for each dimension
    ##################################################################
    ##################################################################
    
    rc_coefs <- RC2model$coefficients %>% data.frame() %>% data.table(keep.rownames = T)
    rc_coefs <- rc_coefs[rn %flike% "inst = "]
    rc_coefs[, father_son := ifelse(str_sub(rn, 1, 25) %like% "focc", "son", "father")]
    rc_coefs[, dim := tstrsplit(rn, " = ", keep = 2)]
    rc_coefs[, dim := tstrsplit(dim, ").as.f", keep = 1)]
    rc_coefs[, occ := tstrsplit(rn, "occ1950)", keep = 3)]
    setnames(rc_coefs, ".", "value")
    rc_coefs[, dim := as.numeric(dim)]
    rc_coefs_son <- dcast(rc_coefs[father_son == "son"], occ  ~ dim, value.var = "value")
    rc_coefs_father <- dcast(rc_coefs[father_son == "father"], occ  ~ dim, value.var = "value")
    setnames(rc_coefs_son, as.character(1:dims), paste0("RC", 1:dims))
    setnames(rc_coefs_father, as.character(1:dims), paste0("RC", 1:dims, "_f"))
    rc_coefs_son[, occ := as.numeric(occ)]
    rc_coefs_father[, occ := as.numeric(occ)]
    
    counts_new <- merge(counts, rc_coefs_son, by.x ="occ1950", by.y = "occ", all.x = T)
    counts_new <- merge(counts_new, rc_coefs_father, by.x ="focc1950", by.y = "occ", all.x = T)
    counts_new[is.na(counts_new)] <- 0 # fill NA values for missing levels from RC model
    
    c.vars_rc <- paste0("RC", 1:dims)
    
    # this is the RC 2 model with fitted characteristics
    
    base_model_mic_mac_mes_vars_RC_10 <-  glm(data = counts_new[sex == c.sex& parent_sex == c.parent_sex & micro_c == 0],
                                              as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950) +
                                    macro_c:as.factor(macroocc) +
                                    meso_c:as.factor(mesoocc) +",
                                                                paste0("`",paste0(c.vars_rc, "`:`",
                                                                                  paste0(c.vars_rc, "_f`")),
                                                                       collapse = " + "))), family = "poisson", model = F)
    
    base_model_mic_mac_mes_vars_RC_10$data <- NULL
    
    # for comparability's sake this is the identical scaling model, but with only the 
    # same number of dimensions used for the RC model (2 in the paper)
    
    base_model_mic_mac_mes_vars_no_diag_RC_dupe <-  glm(data = counts_new[sex == c.sex& parent_sex == c.parent_sex & micro_c == 0],
                                                        as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950) +
                                    macro_c:as.factor(macroocc) +
                                    meso_c:as.factor(mesoocc) +",
                                                                          paste0("`",paste0(c.vars[1:dims], "`:`",
                                                                                            paste0(c.vars[1:dims], "_f`")),
                                                                                 collapse = " + "))), family = "poisson", model = F)
    
    base_model_mic_mac_mes_vars_no_diag_RC_dupe$data <- NULL

    # I also just save a copy of each row's and column's final set of characteristics
    # including fitted RC values
    sons_chars <- counts_new[,.SD, .SDcols = names(counts_new)[names(counts_new) %like% "^RC|_fac$|SEI" &
                                                                 #   names(counts_new) %like% "_fac$" &
                                                                 !names(counts_new) %like% "17|18|19|20|21" &
                                                                 !names(counts_new) %like% "f$"|names(counts_new) %like% "^occ1950$|^N$"
    ]]
    
    fathers_chars <- counts_new[,.SD, .SDcols = names(counts_new)[(names(counts_new) %like% "^RC|_fac_f$|SEI" &
                                                                     #   names(counts_new) %like% "_fac$" &
                                                                     !names(counts_new) %like% "17|18|19|20|21" &
                                                                     names(counts_new) %like% "f$")|names(counts_new) %like% "^focc1950$|^N$"
    ]] 
    
    ############################################################################
    ############################################################################
    ### Final model is to fit the mobility categorical model for comparison
    ### This model allows each meso class to interact with each other, allowing for 
    ### categorical "mobility" coefficients
    ### Since several mesoclasses are fully nested in a Macroclass, I consolidate them
    ### This also make sure that the final number of meso-class interactions
    ### is roughly comparable to the scaling models
    ############################################################################
    ###########################################################################
  
    # get rid of Primary macro and meso class for women and girls as there are so few cases
    counts_new[sex == 2 & parent_sex == 2 & mesoocc %like% "Farmers",  mesoocc := "Lower manual"]
    counts_new[sex == 2 & parent_sex == 2 & macroocc %like% "Primary",  macroocc := "Manual"]
    counts_new[sex == 2 & parent_sex == 2 & mesoocc_f %like% "Farmers",  mesoocc_f := "Lower manual"]
    counts_new[sex == 2 & parent_sex == 2 & macroocc_f %like% "Primary",  macroocc_f := "Manual"]
    
    
    base_model_compare_2 <- glm(data = counts_new[sex == c.sex & parent_sex == c.parent_sex],
                                N ~ as.factor(occ1950) + as.factor(focc1950) +
                                  micro_c:as.factor(microocc) +
                                  # meso_c:as.factor(mesoocc) +
                                  macro_c:as.factor(macroocc) +
                                  as.factor(mesoocc):as.factor(mesoocc_f),
                                family = "poisson", model = F)
    
    base_model_compare_2$data <- NULL
    
    
    #### save the models as an object
    save(base_model_mic_mac_mes, base_model_mic_mac_mes_status,base_model_vars_10_no_status,
         base_model_mac_mes, base_model_mac_mes_status, base_model_status,base_model_mic_mac_mes_vars_no_diag_RC_dupe,
         base_model_vars_10, base_model_mic_mac_mes_vars_10,base_model,base_model_compare_2, base_model_mic_mac_mes_vars_RC_10,
         sons_chars, fathers_chars, 
         file = paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_" ,model_version, ".Data"))

  }
}

################################################################################
### Check for attenuation of microclasses when you add scaling coefficients
###############################################################################

### define a function  to extract fitted coefficients from model and save as a data frame
attenuationr <- function(c.mod){
  d.mod <- c.mod
  dt1 <- summary(d.mod)$coefficients %>% data.table(keep.rownames = T)
  dt1[, model := deparse(substitute(c.mod))]
  return(dt1)
}

## loop over parent child gender combinations and see how coefficients change with and without scaling variables
## save outputs as two lists of data frames

out <- list()
out2 <- list()
i <- 0
for(c.sex in 1:2){
  for(c.parent_sex in 1:2){
    print(c.sex)
    i <- i + 1
    load(paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_" ,model_version, ".Data"))
    out2[[i]] <- attenuationr(base_model_mic_mac_mes_status)
    out[[i]] <- attenuationr(base_model_mic_mac_mes_vars_10)
    out2[[i]][, sex := c.sex]
    out2[[i]][, parent_sex := c.parent_sex]
    out[[i]][, sex := c.sex]
    out[[i]][, parent_sex := c.parent_sex]
  }
}

out[[1]][,model := "All (10) Components"]
out[[2]][,model :="All (10) Components"]
out[[3]][,model := "All (10) Components"]
out[[4]][,model := "All (10) Components"]
out <- rbindlist(out)

#out2 <-diff_mods_iter_no_micro[c(1, 2, 3, 4)] 
out2[[1]][,model := "All (10) Components No Micro"]
out2[[2]][,model :="All (10) Components No Micro"]
out2[[3]][,model := "All (10) Components No Micro"]
out2[[4]][,model := "All (10) Components No Micro"]


### combine everything into one dataframe

out2 <- rbindlist(out2)
out <- rbind(out, out2, fill = T)

dt <- out


### relable everything to make it pretty for plotting

dt <- dt[rn %like% "micro_c:as|macro_c|meso_c"]
dt[, microocc := gsub("micro_c:as.factor(microocc)", "", rn, fixed = T)]
dt[, microocc := gsub("macro_c:as.factor(macroocc)", "", microocc, fixed = T)]
dt[, microocc := gsub("meso_c:as.factor(mesoocc)", "", microocc, fixed = T)]
dt[rn %like% "micro", level := "Microclass"]
dt[rn %like% "meso", level := "Mesoclass"]
dt[rn %like% "macro", level := "Macroclass"]
dt[, name := gsub("Micro: |Macro: |Meso: ", "", microocc)]


### create CIs
dt[, upper  := Estimate + 1.96 * `Std. Error`]
dt[, lower  := Estimate - 1.96 * `Std. Error`]
dt[, significant := ifelse(`Pr(>|z|)` < .05, "Significant", "Not Significant")]


# create better model names for graph
dt[model == "All (10) Components No Micro", model2 := "Model 5: Microclass Immobility Effects + SEI"]
dt[model == "All (10) Components", model2 := "Model 6: Model 5 + 10 Factor Scaling Effects"]


# order effect sizes based on fathers/sons
microocc_levs <- dt[model == "All (10) Components No Micro" & sex == 1 & 
                      parent_sex == 1 & rn %like% "micro"] %>% 
  .[order(level, Estimate, sex, parent_sex), microocc]

dt[, name := as.character(name)]
dt[, name := factor(name, microocc_levs)]

# create grouping variables
dt[, `Both Insignificant` := ifelse(all(upper > 7 & lower < -7), "Not Significant",
                                    "Significant"), 
   by = .(sex, parent_sex, name)]


# rename facet labels
dt[parent_sex == 2, parent_sex_clean := "Mothers"]
dt[parent_sex == 1, parent_sex_clean := "Fathers"]
dt[sex == 1, sex_clean := "Sons"]
dt[sex == 2, sex_clean := "Daughters"]

dt[, parent_sex_clean := factor(parent_sex_clean, levels = c("Fathers", "Mothers"))]
dt[, sex_clean := factor(sex_clean, levels = c("Sons", "Daughters"))]

# assign signs
signs <- dt[level %like% "Micro" &
              model %like% "fact|PC|pca|base_model_mic_mac_mes|Eight|Six|All",.(N_signficant = length(Estimate[Estimate > 0 & significant == "Significant"])), 
            by = .(parent_sex, sex, model)]

signs <- signs[,.(new_label = paste0("Originally Significant: ", N_signficant[model == "All (10) Components No Micro"], "\n", 
                                     "With Scaling Effects: ", N_signficant[model == "All (10) Components"])), 
               by = .(parent_sex, sex)]

dt <- merge(dt, signs, by = c("parent_sex", "sex"))

dt[, parent_sex_sex_clean := paste0(parent_sex_clean, "/", sex_clean)]
dt[, parent_sex_sex_clean := factor(parent_sex_sex_clean, 
                                    levels = unique(as.character(parent_sex_sex_clean))[c(1,2,3,4)])]

# plot
gg <- ggplot(dt[level %like% "Micro" &
                  model %like% "fact|PC|pca|base_model_mic_mac_mes|Eight|Six|All"]
             %>% .[, model2 := factor(model2, levels = unique(model2)[c(2,1,3)])]) + 
  geom_pointrange(aes(x = name, y = Estimate, ymax = upper, ymin = lower, color = model2, group = model,
                      shape = as.factor(significant),
                      linetype = as.factor(`Both Insignificant`)), position = position_dodge(width = .7), size = .17) +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_y_continuous(#trans = scales::pseudo_log_trans(base = 10, sigma = 10),
    breaks = c(-4, -2, 0, 2, 4)) +
  scale_shape_manual(values = c(4,1)) + 
  coord_flip(ylim = c(-6,6)) + 
  #facet_nested(attenuation ~., scales = "free_y", space = "free_y") + 
  theme(strip.text.y.right = element_text(angle = 0), 
        axis.text.y.left = element_text(size= 8), 
        legend.text = element_text(size = 8)) + 
  #scale_color_viridis_d(begin = .25, end = .75) + 
  labs(x = "Micro-Class", color = "Model", shape = "Significance", 
       y = "Immobility Effect Parameter Estimate") + 
  theme_bw(base_size = 9, base_family = "serif") + 
  theme(legend.position = "bottom", legend.justification = c(0, 1), plot.title.position = "plot")+
  guides(color=guide_legend(nrow=2,byrow=TRUE), 
         shape=guide_legend(nrow=2,byrow=TRUE)) + 
  facet_grid(~ parent_sex_sex_clean + new_label) + 
  scale_linetype_manual(values = c(3,1))+
  theme(strip.background =element_rect(fill="white")) + 
  guides(linetype = FALSE, 
         alpha = F) + 
  scale_color_viridis_d(begin = .25, end = .7)# + theme_bw()

print(gg)


dir.create(paste0("../outputs/", model_version), recursive = T)

pdf(paste0("../outputs/", model_version, "/figure_1.pdf"), height = 6.95, width = 7.3)
print(gg)
dev.off()

tiff(paste0("../outputs/", model_version, "/figure_1.tiff"), height = 6.95, width = 7.3, res = 800, units = 'in')
print(gg)
dev.off()

gg <- ggplot(dt[level %like% "Micro" &
                  model %like% "fact|PC|pca|base_model_mic_mac_mes|Eight|Six|All"]
             %>% .[, model2 := factor(model2, levels = unique(model2)[c(2,1,3)])]) + 
  geom_pointrange(aes(x = name, y = Estimate, ymax = upper, ymin = lower, color = model2, group = model,
                      shape = as.factor(significant),
                      linetype = as.factor(`Both Insignificant`)), position = position_dodge(width = .7), size = .17) +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_y_continuous(#trans = scales::pseudo_log_trans(base = 10, sigma = 10),
    breaks = c(-4, -2, 0, 2, 4)) +
  scale_shape_manual(values = c(4,1)) + 
  coord_flip(ylim = c(-6,6)) + 
  #facet_nested(attenuation ~., scales = "free_y", space = "free_y") + 
  theme(strip.text.y.right = element_text(angle = 0), 
        axis.text.y.left = element_text(size= 8), 
        legend.text = element_text(size = 8)) + 
  #scale_color_viridis_d(begin = .25, end = .75) + 
  labs(x = "Micro-Class", color = "Model", shape = "Significance", 
       y = "Immobility Effect Parameter Estimate") + 
  theme_bw(base_size = 9, base_family = "serif") + 
  theme(legend.position = "bottom", legend.justification = c(0, 1), plot.title.position = "plot")+
  guides(color=guide_legend(nrow=2,byrow=TRUE), 
         shape=guide_legend(nrow=2,byrow=TRUE)) + 
  facet_grid(~ parent_sex_sex_clean + new_label) + 
  scale_linetype_manual(values = c(3,1))+
  theme(strip.background =element_rect(fill="white")) + 
  guides(linetype = FALSE, 
         alpha = F) + 
  scale_color_manual(values = c("black", "gray60"))# + theme_bw()

print(gg)


dir.create(paste0("../outputs/", model_version), recursive = T)

pdf(paste0("../outputs/", model_version, "/figure_1_bw.pdf"), height = 6.95, width = 7.3)
print(gg)
dev.off()

tiff(paste0("../outputs/", model_version, "/figure_1_bw.tiff"), height = 6.95, width = 7.3, res = 800, units = 'in')
print(gg)
dev.off()


#### Repeat for macro/meso attenuation

dt <- out

dt <- dt[rn %like% "micro_c:as|macro_c|meso_c"]
dt[, microocc := gsub("micro_c:as.factor(microocc)", "", rn, fixed = T)]
dt[, microocc := gsub("macro_c:as.factor(macroocc)", "", microocc, fixed = T)]
dt[, microocc := gsub("meso_c:as.factor(mesoocc)", "", microocc, fixed = T)]
dt[rn %like% "micro", level := "Microclass"]
dt[rn %like% "meso", level := "Mesoclass"]
dt[rn %like% "macro", level := "Macroclass"]
dt[, name := gsub("Micro: |Macro: |Meso: ", "", microocc)]

dt[, upper  := Estimate + 1.96 * `Std. Error`]
dt[, lower  := Estimate - 1.96 * `Std. Error`]
dt[, significant := ifelse(`Pr(>|z|)` < .05, "Significant", "Not Significant")]


dt[model == "All (10) Components No Micro", model2 := "Model 5: Microclass Immobility Effects + SEI"]
dt[model == "All (10) Components", model2 := "Model 6: Model 5 + 10 Factor Scaling Effects"]

microocc_levs <- dt[model == "All (10) Components No Micro" & sex == 1 & 
                      parent_sex == 1 & rn %like% "meso|macro"] %>% 
  .[order(level, Estimate, sex, parent_sex), microocc]

dt[, name := as.character(name)]
dt[, name := factor(name, microocc_levs)]

dt[, `Both Insignificant` := ifelse(all(upper > 7 & lower < -7), "Not Significant",
                                    "Significant"), 
   by = .(sex, parent_sex, name)]


dt[parent_sex == 2, parent_sex_clean := "Mothers"]
dt[parent_sex == 1, parent_sex_clean := "Fathers"]
dt[sex == 1, sex_clean := "Sons"]
dt[sex == 2, sex_clean := "Daughters"]

dt[, parent_sex_clean := factor(parent_sex_clean, levels = c("Fathers", "Mothers"))]
dt[, sex_clean := factor(sex_clean, levels = c("Sons", "Daughters"))]

signs <- dt[level %like% "Meso|Macro" &
              model %like% "fact|PC|pca|base_model_mic_mac_mes|Eight|Six|All",.(N_signficant = length(Estimate[Estimate > 0 & significant == "Significant"])), 
            by = .(parent_sex, sex, model)]

signs <- signs[,.(new_label = paste0("Originally Significant: ", N_signficant[model == "All (10) Components No Micro"], "\n", 
                                     "With Scaling Effects: ", N_signficant[model == "All (10) Components"])), 
               by = .(parent_sex, sex)]

dt <- merge(dt, signs, by = c("parent_sex", "sex"))

dt[, parent_sex_sex_clean := paste0(parent_sex_clean, "/", sex_clean)]
dt[, parent_sex_sex_clean := factor(parent_sex_sex_clean, 
                                    levels = unique(as.character(parent_sex_sex_clean))[c(1,2,3,4)])]


gg <- ggplot(dt[level %like% "Meso|Macro" &
                  model %like% "fact|PC|pca|base_model_mic_mac_mes|Eight|Six|All"]
             %>% .[, model2 := factor(model2, levels = unique(model2)[c(2,1,3)])]) + 
  geom_pointrange(aes(x = name, y = Estimate, ymax = upper, ymin = lower, color = model2, group = model,
                      shape = as.factor(significant),
                      linetype = as.factor(`Both Insignificant`)), position = position_dodge(width = .7), size = .17) +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10, sigma = .05),
    breaks = c(-4, -1, -.25, 0,.25, 1, 4), 
    labels = c("-4", "-1", "-.25", "0", ".25", "1", "4")) +
  scale_shape_manual(values = c(4,1)) + 
  coord_flip(ylim = c(-6,6)) + 
  #facet_nested(attenuation ~., scales = "free_y", space = "free_y") + 
  theme(strip.text.y.right = element_text(angle = 0), 
        axis.text.y.left = element_text(size= 10), 
        legend.text = element_text(size = 10)) + 
  #scale_color_viridis_d(begin = .25, end = .75) + 
  labs(x = "Macro-/Meso-Class", color = "Model", shape = "Significance", 
       y = "Immobility Effect Parameter Estimate") + 
 theme_bw(base_size = 10, base_family = "serif") + 
  theme(legend.position = "bottom", legend.justification = c(0, 1), plot.title.position = "plot")+
  guides(color=guide_legend(nrow=2,byrow=TRUE), 
         shape=guide_legend(nrow=2,byrow=TRUE)) + 
  facet_grid(level~ parent_sex_sex_clean + new_label, scales = "free", space = "free") + 
  scale_linetype_manual(values = c(3,1))+
  theme(strip.background =element_rect(fill="white")) + 
  guides(linetype = FALSE, 
         alpha = F) + 
  scale_color_viridis_d(begin = .25, end = .7)# + theme_bw()

print(gg)


dir.create(paste0("../outputs/", model_version), recursive = T)

pdf(paste0("../outputs/", model_version, "/appendix_figure_a6.pdf"), height = 6.25, width = 7)
print(gg)
dev.off()



# Plot actual coefficients
out3 <- list()
i <- 0
for(c.sex in 1:2){
  for(c.parent_sex in 1:2){
    print(c.sex)
    i <- i + 1
    load(paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_" ,model_version, ".Data"))
    out3[[i]] <- attenuationr(base_model_vars_10_no_status)
    out3[[i]][, sex := c.sex]
    out3[[i]][, parent_sex := c.parent_sex]
    
  }
}
dt <-rbindlist(out3)

dt <- dt[!rn %like% "micro_c:as|macro_c:as|mecro_c:as|as.factor|Intercept"]
dt[, variable := tstrsplit(rn, ":", keep = 2)]
dt[, variable := gsub("_f" , "", variable)]

dt[, variable := gsub("_" , " ", variable)]

dt[, variable := sub('^(\\w?)', '\\U\\1', variable, perl=T)]
dt[rn %like% "micro_c_neg", type := "Mobility Effects"]
dt[rn %like% "^micro_c|micro_c$", type := "Immobility Effects"]

dt[, upper  := Estimate + 1.96 * `Std. Error`]
dt[, lower  := Estimate - 1.96 * `Std. Error`]
dt[, significant := ifelse(`Pr(>|z|)` < .05, "Significant", "Not Significant")]
dt[, parent_sex := ifelse(parent_sex == 1, "Fathers", "Mothers")]
dt[, child_sex := ifelse(sex == 1, "Sons", "Daughters")]



dt[variable %like% "Education req", variable := "Required Educ./Training"]
var_order <- dt[type %like% "Mobility" & parent_sex %like% "Father" & child_sex %like% "Son"] %>% .[order(Estimate), variable]
dt[, `Parent/Child` := paste0(parent_sex, "/", child_sex)]

# dt[, variable := factor(variable, levels = var_order)]
# 
dt[, variable := gsub("fact |Fact ", "", variable)]
dt[, variable := gsub("`", "", variable)]
dt[, variable := gsub("ac$", "", variable)]
dt[, variable := gsub("Repetion", "Repetition", variable)]



dt[,variable :=   factor(variable, levels = unique(dt$variable)[order((as.numeric(substr(tstrsplit(unique(dt$variable), " ", keep = 2, type.convert = T)[[1]], 1,2))))][c(1:10)])]

dt[, `Parent/Child` := factor(`Parent/Child`, levels = unique(`Parent/Child`)[c(1,3,2,4)])]
library(ggthemes)
gg <- ggplot(dt) + 
  geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = .25)+
  geom_bar(aes(x = variable, y = Estimate,
               shape = as.factor(significant), 
               group =  `Parent/Child`, fill =  `Parent/Child`), stat = "identity", 
           width = .5,
           position = position_dodge(width = .5)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .2, base = 2), 
                     breaks = c(-10, -4, -1.5,-.5, 0, .5,1.5, 4, 10))+
  scale_fill_colorblind()+
  geom_crossbar(aes(x = variable, y = Estimate, ymax = upper, ymin = lower,
                    group =  `Parent/Child`,
                    shape = as.factor(significant)), width = 0, color = "gray50" ,
                fatten = .1,
                position = position_dodge(width = .5)) +
  theme(strip.background =element_rect(fill="white")) + 
  facet_grid(type ~.) +
  theme_bw(base_size = 10, base_family = "serif") +
  
  #scale_y_continuous(trans = scales::pseudo_log_trans()) +
  scale_shape_manual(values = c(4,1)) +
  labs(x = "Variable", y = "Estimate",# title = "Scaling Effects for Occupational Mobility and\nImmobility Seperately, by Sex", 
       shape = "Significance") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust =1), 
        panel.grid.minor = element_blank()) +
  theme(strip.background =element_rect(fill="white")) + 
  coord_cartesian(ylim = c(-10, 10)) #+ theme_bw()

#gg

pdf(paste0("../outputs/", model_version, "/figure_2.pdf"), height = 5, width = 7)
print(gg)
dev.off()

tiff(paste0("../outputs/", model_version, "/figure_2.tiff"), height = 5, width = 7, res = 800, units = 'in')
print(gg)
dev.off()


gg <- ggplot(dt) + 
  geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = .25)+
  geom_bar(aes(x = variable, y = Estimate,
               shape = as.factor(significant), 
               group =  `Parent/Child`, fill =  `Parent/Child`), stat = "identity", 
           width = .5,
           position = position_dodge(width = .5)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .2, base = 2), 
                     breaks = c(-10, -4, -1.5,-.5, 0, .5,1.5, 4, 10))+
  scale_fill_manual(values = c("gray20", "gray40", "gray60", "gray80"))+
  geom_crossbar(aes(x = variable, y = Estimate, ymax = upper, ymin = lower,
                    group =  `Parent/Child`,
                    shape = as.factor(significant)), width = 0, color = "gray50" ,
                fatten = .1,
                position = position_dodge(width = .5)) +
  theme(strip.background =element_rect(fill="white")) + 
  facet_grid(type ~.) +
  theme_bw(base_size = 10, base_family = "serif") +
  
  #scale_y_continuous(trans = scales::pseudo_log_trans()) +
  scale_shape_manual(values = c(4,1)) +
  labs(x = "Variable", y = "Estimate",# title = "Scaling Effects for Occupational Mobility and\nImmobility Seperately, by Sex", 
       shape = "Significance") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust =1), 
        panel.grid.minor = element_blank()) +
  theme(strip.background =element_rect(fill="white")) + 
  coord_cartesian(ylim = c(-10, 10)) #+ theme_bw()

#gg

pdf(paste0("../outputs/", model_version, "/figure_2_bw.pdf"), height = 5, width = 7)
print(gg)
dev.off()

tiff(paste0("../outputs/", model_version, "/figure_2_bw.tiff"), height = 5, width = 7, res = 800, units = 'in')
print(gg)
dev.off()



# Plot actual coefficients with SEI
out3 <- list()
i <- 0
for(c.sex in 1:2){
  for(c.parent_sex in 1:2){
    print(c.sex)
    i <- i + 1
    load(paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_" ,model_version, ".Data"))
    out3[[i]] <- attenuationr(base_model_vars_10)
    out3[[i]][, sex := c.sex]
    out3[[i]][, parent_sex := c.parent_sex]
    
  }
}
dt <-rbindlist(out3)

dt <- dt[!rn %like% "micro_c:as|macro_c:as|meso_c:as|as.factor|Intercept"]
dt[, variable := tstrsplit(rn, ":", keep = 2)]
dt[, variable := gsub("_f" , "", variable)]

dt[, variable := gsub("_" , " ", variable)]

dt[, variable := sub('^(\\w?)', '\\U\\1', variable, perl=T)]
dt[rn %like% "micro_c_neg", type := "Mobility Effects"]
dt[rn %like% "^micro_c:|micro_c$", type := "Immobility Effects"]

dt[, upper  := Estimate + 1.96 * `Std. Error`]
dt[, lower  := Estimate - 1.96 * `Std. Error`]
dt[, significant := ifelse(`Pr(>|z|)` < .05, "Significant", "Not Significant")]
dt[, parent_sex := ifelse(parent_sex == 1, "Fathers", "Mothers")]
dt[, child_sex := ifelse(sex == 1, "Sons", "Daughters")]



dt[variable %like% "Education req", variable := "Required Educ./Training"]
var_order <- dt[type %like% "Mobility" & parent_sex %like% "Father" & child_sex %like% "Son"] %>% .[order(Estimate), variable]
dt[, `Parent/Child` := paste0(parent_sex, "/", child_sex)]

# dt[, variable := factor(variable, levels = var_order)]
# 
dt[, variable := gsub("fact |Fact ", "", variable)]
dt[, variable := gsub("`", "", variable)]
dt[, variable := gsub("ac$", "", variable)]
dt[, variable := gsub("Repetion", "Repetition", variable)]



dt[,variable :=   factor(variable, levels = unique(dt$variable)[order((as.numeric(substr(tstrsplit(unique(dt$variable), " ", keep = 2, type.convert = T)[[1]], 1,2))))][c(1:12)])]

dt[, `Parent/Child` := factor(`Parent/Child`, levels = unique(`Parent/Child`)[c(1,3,2,4)])]
library(ggthemes)
gg <- ggplot(dt) + 
  geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = .25)+
  geom_bar(aes(x = variable, y = Estimate,
               shape = as.factor(significant), 
               group =  `Parent/Child`, fill =  `Parent/Child`), stat = "identity", 
           width = .5,
           position = position_dodge(width = .5)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .2, base = 2), 
                     breaks = c(-10, -4, -1.5,-.5, 0, .5,1.5, 4, 10))+
  scale_fill_colorblind()+
  geom_crossbar(aes(x = variable, y = Estimate, ymax = upper, ymin = lower,
                    group =  `Parent/Child`,
                    shape = as.factor(significant)), width = 0, color = "gray50" ,
                fatten = .1,
                position = position_dodge(width = .5)) +
  facet_grid(type ~.) +
  theme_bw(base_size = 10, base_family = "serif") +
  
  #scale_y_continuous(trans = scales::pseudo_log_trans()) +
  scale_shape_manual(values = c(4,1)) +
  labs(x = "Variable", y = "Estimate",# title = "Scaling Effects for Occupational Mobility and\nImmobility Seperately, by Sex", 
       shape = "Significance") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust =1), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim = c(-4, 5)) #+ theme_bw()

#gg

pdf(paste0("../outputs/", model_version, "/appendix_figure_a5.pdf"), height = 5, width = 7)
print(gg)
dev.off()


###########
# table 3 - model fit statistics and comparisons
##########

# create a function to extract model characteristics
model_descr <- function(d.mod){
  c.mod <- copy(d.mod)
  data.table(ll = logLik(c.mod) %>% as.numeric(),
             aic = AIC(c.mod),
             bic = BIC(c.mod),
             delta =  (sum(abs(c.mod$data$N-exp(predict((c.mod)))))/
                         (2*(sum(c.mod$data$N)))),
             df = c.mod$df.residual,
             q = c.mod$df.null - c.mod$df.residual
  )
}

# create a function to compare two models
lmr <- function(c.mod, c.mod_comp, inc_dem){
  dat <- lmtest::lrtest(c.mod, get(c.mod_comp))  %>% .[2,c(3,4,5)] %>% data.table()
  dat[, inc_dem := inc_dem]
  return(dat)
}

out_all_all <- data.table()

# loop over parent child gender combos
for(c.sex in 1:2){
  for(c.parent_sex in 1:2){
    rm(list = ls()[ls() %like% "base_model"])
    load(paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_" ,model_version, ".Data"))
    
    lapply(list(base_model, 
                base_model_mic_mac_mes, 
                base_model_compare_2,
                base_model_status,
                base_model_mic_mac_mes_status,
                base_model_mic_mac_mes_vars_10,
                #  base_model_mic_mac_mes_vars_RC_10,
                base_model_vars_10_no_status,
                base_model_vars_10), model_descr ) %>% rbindlist -> out1
    lapply(list(                                                    base_model_mic_mac_mes,
                                                                    base_model_compare_2,
                                                                    base_model_status,
                                                                    base_model_mic_mac_mes_status,
                                                                    base_model_mic_mac_mes_vars_10,
                                                                    #                base_model_mic_mac_mes_vars_RC_10,
                                                                    base_model_vars_10_no_status,
                                                                    base_model_vars_10),lmr,  c.mod_comp = "base_model", inc_dem = F) %>%
      rbindlist()-> out2
    out2 <- rbind(data.table(matrix(c(NA, NA, NA, NA), nrow = 1)), out2 , use.names = F)
    lapply(list(base_model_mic_mac_mes_status, 
                base_model_mic_mac_mes_vars_10,
                #base_model_mic_mac_mes_vars_RC_10,
                base_model_vars_10),lmr,
           c.mod_comp = "base_model_status", inc_dem = F) %>%
      rbindlist()-> out3
    out3 <- rbind(data.table(matrix(c(NA), nrow =4, ncol = 4)),
                  out3[1:2,] ,
                  data.table(matrix(c(NA), nrow = 1, ncol = 4)), 
                  out3[4,], 
                  data.table(matrix(c(NA), nrow = 0, ncol = 4)),
                  use.names = F)
    lapply(list(base_model_mic_mac_mes_vars_10#,
                #base_model_mic_mac_mes_vars_RC_10
    ),lmr,
    c.mod_comp = "base_model_mic_mac_mes_status", inc_dem = F) %>%
      rbindlist()-> out4
    out4 <- rbind(data.table(matrix(c(NA), nrow = 5, ncol = 4)),
                  out4 ,
                  data.table(matrix(c(NA), nrow = 2, ncol = 4)),use.names = F)
    lapply(list(base_model_vars_10),lmr,
           c.mod_comp = "base_model_vars_10_no_status", inc_dem = F) %>%
      rbindlist()-> out5
    out5 <- rbind(data.table(matrix(c(NA), nrow = 7, ncol = 4)),
                  out5,
                  data.table(matrix(c(NA), nrow = 0, ncol = 4)),
                  use.names = F)
    out_all <- cbind(out1, out2, out3, out4, out5)
    out_all[, sex := c.sex]
    out_all[, parent_sex := c.parent_sex]
    
    out_all_all <- rbind(out_all_all, out_all)
    
  }
}

table3 <- copy(out_all_all)

saveRDS(table3, paste0("../outputs/", model_version, "/table3.rds"))



####################
# Sensitivity analysis 1 - Change over time
####################
vars_list <- list( c(vars[vars %like% "^PC"]),
                   c(vars[vars %like% paste0(paste0(paste0("^fact_", 1:10, "_"),
                                                    collapse = "|"))]))
vars_list[[2]] <- c( vars_list[[2]])

c.vars <- vars_list[[2]]


# only do sensitivity analysis for men, broken up by terciles of the data

for(c.sex in 1){
  for(c.parent_sex in 1){
    for(c.tercile in 1:3){
      print(c.sex)
      print(c.parent_sex)
      print(c.tercile)
      if(c.tercile ==1){
        lower_bound <- 1898
        upper_bound <- 1937
      }else if(c.tercile ==2){
        lower_bound <- 1937
        upper_bound <- 1955
      }else{
        lower_bound <- 1955
        upper_bound <- 1994
      }
      
      counts <- gen[birthyear >= lower_bound & birthyear < upper_bound,.N, by = .(focc1950,occ1950, sex, parent_sex)]
      counts[, focc1950:= as.numeric(focc1950)]
      counts[,occ1950:= as.numeric(occ1950)]
      counts[, sex:= as.numeric(sex)]
      counts[, parent_sex:= as.numeric(parent_sex)]
      # expand grid for 0s
      new_counts <- expand.grid(occ1950 = unique(counts$occ1950),
                                focc1950 = unique(counts$focc1950),
                                sex = 1:2,
                                # fa_edu_recode = unique(counts$fa_edu_recode),
                                #edu_recode = unique(counts$edu_recode),
                                parent_sex = 1:2) %>% 
        data.table()
      counts <- merge(counts, new_counts, by = names(new_counts), all = T)
      counts[is.na(N), N := 0]
      counts <- counts[!occ1950 %in% c(999,997,995,595) & occ1950 < 979]
      counts <- counts[!focc1950 %in% c(999,997,995,595) & focc1950 < 979]
      
      #vars <- names(skills)[names(skills) %like% ".LV|prestige|status|PC"]
      counts <- merge(counts, skills[,.SD, .SDcols = c("OCC1950", vars, 
                                                       "microocc", "mesoocc", "macroocc")], by.x = "occ1950", by.y = "OCC1950")
      father_skills <- copy(skills)
      setnames(father_skills, names(father_skills), paste0(names(father_skills), "_f"))
      counts <- merge(counts, father_skills[,.SD, .SDcols = c("OCC1950_f", paste0(vars, "_f"), 
                                                              "microocc_f", "mesoocc_f", "macroocc_f")], by.x = "focc1950", by.y = "OCC1950_f")
      
      
      vars_to_loop <- vars
      for(c_var in vars_to_loop){
        counts[, paste0(c_var, "_diff") := (get(c_var) - get(paste0(c_var, "_f")))]
      }
      ######################################################
      ##### Regress
      ######################################################
      counts[,micro_c :=  ifelse(microocc == microocc_f, 1, 0)]
      counts[,micro_c_neg :=  ifelse(microocc == microocc_f, 0,1)]
      counts[,macro_c :=  ifelse(macroocc == macroocc_f, 1, 0)]
      counts[,meso_c :=  ifelse(mesoocc == mesoocc_f, 1, 0)]
      counts[,occ1950_c :=  ifelse(occ1950 == focc1950, 1, 0)]

      base_model_vars_10_no_status <-  glm(data = counts[sex == c.sex& parent_sex == c.parent_sex],
                                           as.formula(paste0("N ~ as.factor(occ1950) + as.factor(focc1950)  +", 
                                                             paste0("micro_c:`",paste0(c.vars[1:10], "`:`",
                                                                                       paste0(c.vars[1:10], "_f`")),
                                                                    collapse = " + ") ,
                                                             " + ",
                                                             paste0("micro_c_neg:`",paste0(c.vars[1:10], "`:`",
                                                                                           paste0(c.vars[1:10], "_f`")),
                                                                    collapse = " + "))), family = "poisson")
      
      save(base_model_vars_10_no_status, 
           file = paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_", c.tercile,"_" ,model_version, ".Data"))
    }
  }
}


# Plot actual coefficients
out3 <- list()
i <- 0
for(c.sex in 1){
  for(c.parent_sex in 1){
    for(c.tercile in 1:3){
      print(c.sex)
      i <- i + 1
      load(paste0("../ref/workspace", c.sex, "_", c.parent_sex,"_", c.tercile,"_" ,model_version, ".Data"))
      out3[[i]] <- attenuationr(base_model_vars_10_no_status)
      out3[[i]][, sex := c.sex]
      out3[[i]][, parent_sex := c.parent_sex]
      out3[[i]][, tercile := c.tercile]
    }
  }
}
dt <-rbindlist(out3)

dt <- dt[!rn %like% "micro_c:as|macro_c:as|mecro_c:as|as.factor|Intercept"]
dt[, variable := tstrsplit(rn, ":", keep = 2)]
dt[, variable := gsub("_f" , "", variable)]

dt[, variable := gsub("_" , " ", variable)]

dt[, variable := sub('^(\\w?)', '\\U\\1', variable, perl=T)]
dt[rn %like% "^micro_c|micro_c$", type := "Immobility Effects"]
dt[rn %like% "micro_c_neg", type := "Mobility Effects"]

dt[, upper  := Estimate + 1.96 * `Std. Error`]
dt[, lower  := Estimate - 1.96 * `Std. Error`]
dt[, significant := ifelse(`Pr(>|z|)` < .05, "Significant", "Not Significant")]
dt[, parent_sex := ifelse(parent_sex == 1, "Fathers", "Mothers")]
dt[, child_sex := ifelse(sex == 1, "Sons", "Daughters")]



dt[variable %like% "Education req", variable := "Required Educ./Training"]
var_order <- dt[type %like% "Mobility" & parent_sex %like% "Father" & child_sex %like% "Son"] %>% .[order(Estimate), variable]
dt[, `Parent/Child` := paste0(parent_sex, "/", child_sex)]

# dt[, variable := factor(variable, levels = var_order)]
# 
dt[, variable := gsub("fact |Fact ", "", variable)]
dt[, variable := gsub("`", "", variable)]
dt[, variable := gsub("ac$", "", variable)]
dt[, variable := gsub("Repetion", "Repetition", variable)]

dt[tercile == 1, Tercile := "1898-1936"]
dt[tercile == 2, Tercile := "1937-1954"]
dt[tercile == 3, Tercile := "1955-1993"]


dt[,variable :=   factor(variable, levels = unique(dt$variable)[order((as.numeric(substr(tstrsplit(unique(dt$variable), " ", keep = 2, type.convert = T)[[1]], 1,2))))][c(1:10)])]

dt[, `Parent/Child` := factor(`Parent/Child`, levels = unique(`Parent/Child`)[c(1,3,2,4)])]
library(ggthemes)
gg <- ggplot(dt) + 
  geom_hline(yintercept = 0, linetype = 2, color = "black", linewidth = .25)+
  geom_bar(aes(x = variable, y = Estimate,
               shape = as.factor(significant), 
               group =  `Tercile`, fill =  `Tercile`), stat = "identity", 
           width = .5,
           position = position_dodge(width = .5)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .2, base = 2), 
                     breaks = c(-10, -4, -1.5,-.5, 0, .5,1.5, 4, 10))+
  # scale_fill_colorblind()+
  geom_crossbar(aes(x = variable, y = Estimate, ymax = upper, ymin = lower,
                    group =  Tercile,
                    shape = as.factor(significant)), width = 0, color = "gray50" ,
                fatten = .1,
                position = position_dodge(width = .5)) +
  facet_grid(type ~.) +
  theme_bw(base_size = 10, base_family = "serif") +
  
  #scale_y_continuous(trans = scales::pseudo_log_trans()) +
  scale_shape_manual(values = c(4,1)) +
  scale_fill_manual(values = colorblind_pal()(8)[5:8])+
  labs(x = "Variable", y = "Estimate",# title = "Scaling Effects for Occupational Mobility and\nImmobility Seperately, by Sex", 
       shape = "Significance") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1, vjust =1), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim = c(-4, 7))# + theme_bw()

#gg

pdf(paste0("../outputs/", model_version, "/appendix_figure_a4.pdf"), height = 5, width = 7)
print(gg)
dev.off()




