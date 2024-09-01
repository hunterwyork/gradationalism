library(Rfast)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(readxl)
library(haven)
library(data.table)
library(gnm)


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

saveRDS(all_out_wide, "../ref/table_4_data.rds")


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


### save file and run code for plotting in separate file

saveRDS(dt, "../ref/figure_1_data.rds")







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

saveRDS(dt, "../ref/figure_2_data.RDS")




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

saveRDS(dt, "../ref/appendix_figure_a5_data.rds")


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
    
    
    ### base model characteristics
    lapply(list(base_model, 
                base_model_status,
                base_model_mic_mac_mes_status,
                base_model_mic_mac_mes_vars_10,
                base_model_vars_10_no_status,
                base_model_vars_10, 
                base_model_mic_mac_mes), model_descr ) %>% rbindlist -> out1
    lapply(list(                  base_model_status,
                                  base_model_mic_mac_mes_status,
                                  base_model_mic_mac_mes_vars_10,
                                  base_model_vars_10_no_status,
                                  base_model_vars_10,
                                  base_model_mic_mac_mes),lmr,  c.mod_comp = "base_model", inc_dem = F) %>%
      rbindlist()-> out2
    out2 <- rbind(data.table(matrix(c(NA, NA, NA, NA), nrow = 1)), out2 , use.names = F)
    
    # compare nested models to model with just sei
    lapply(list(base_model_mic_mac_mes_status, 
                base_model_mic_mac_mes_vars_10, 
                base_model_vars_10),lmr,
           c.mod_comp = "base_model_status", inc_dem = F) %>%
      rbindlist()-> out3
    # append to table
    out3 <- rbind(data.table(matrix(c(NA), nrow =2, ncol = 4)),
                  out3[1:2,] ,
                  data.table(matrix(c(NA), nrow = 1, ncol = 4)), 
                  out3[3,], 
                  data.table(matrix(c(NA), nrow = 1, ncol = 4)),
                  use.names = F)
    # compare nested model to mes/mic/mac immobility model
    lapply(list(base_model_mic_mac_mes_vars_10),lmr,
           c.mod_comp = "base_model_mic_mac_mes_status", inc_dem = F) %>%
      rbindlist()-> out4
    # append to table
    out4 <- rbind(data.table(matrix(c(NA), nrow = 3, ncol = 4)),
                  out4 ,
                  data.table(matrix(c(NA), nrow = 3, ncol = 4)),use.names = F)
    # compare nested model to pure gradational model
    lapply(list(base_model_vars_10),lmr,
           c.mod_comp = "base_model_vars_10_no_status", inc_dem = F) %>%
      rbindlist()-> out5
    out5 <- rbind(data.table(matrix(c(NA), nrow = 5, ncol = 4)),
                  out5,
                  data.table(matrix(c(NA), nrow = 1, ncol = 4)),
                  use.names = F)
    out_all <- cbind(out1, out2, out3, out4, out5)
    out_all[, sex := c.sex]
    out_all[, parent_sex := c.parent_sex]
    
    out_all_all <- rbind(out_all_all, out_all)
    
  }
}

table2 <- copy(out_all_all)

saveRDS(table2, paste0("../outputs/", model_version, "/table3.rds"))


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
      
      counts[, PC_control := sample(PC1, nrow(counts))]
      counts[, PC_control_f := sample(PC1, nrow(counts))]
      
      # Create difference variables between children and parents
      
      vars_to_loop <- vars
      vars_to_loop <- c(vars_to_loop, "PC_control")
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

saveRDS(dt, "../ref/appendix_figure_a4_data.rds")




