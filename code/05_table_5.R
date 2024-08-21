library(ggplot2)
library(data.table)
library(dplyr)

model_version <- "20240820_submission_final"

counts <- readRDS(  paste0("../ref/counts", model_version, ".Data"))
skills <- readRDS("../ref/skills_occ1950_new.rds")
vars <- names(skills)[(!names(skills) %like% "OCC|occ")]
skills[, OCC1950 := as.numeric(OCC1950)]


pop_weights <- counts[,.(pop_weight = sum(N)), by = .(occ1950, parent_sex, sex)]
skills_weights <- merge(skills, pop_weights, by.x = "OCC1950", by.y = "occ1950")
microoccs_by_skill <- skills_weights[,lapply(.SD, weighted.mean, w = pop_weight, na.rm = T),
                                     .SDcols = vars, by = .(microocc,mesoocc, macroocc)]
microoccs_by_skill_temp <- data.table()
for(c.parent_sex in 1:2){
  for(c.sex in 1:2){
    microoccs_by_skill[, parent_sex := c.parent_sex]
    microoccs_by_skill[, sex := c.sex]
    microoccs_by_skill_temp <- rbind(microoccs_by_skill_temp, microoccs_by_skill)
  }
}
microoccs_by_skill <- copy(microoccs_by_skill_temp)

microoccs_by_skill_dads <- copy(microoccs_by_skill)
setnames(microoccs_by_skill_dads, names(microoccs_by_skill_dads)[!names(microoccs_by_skill_dads) %like% "sex"],
         paste0(names(microoccs_by_skill_dads)[!names(microoccs_by_skill_dads) %like% "sex"], "_f"))
# microoccs_by_skill[, i := 1]
# microoccs_by_skill_dads[, i := 1]
# create microocc transition matrix
micro_trans <- merge(microoccs_by_skill,microoccs_by_skill_dads, allow.cartesian = T )
pop_weights <- counts[,.(N = sum(N)), by = .(microocc, microocc_f, sex, parent_sex)]
micro_trans <- merge(micro_trans, pop_weights, by = c("microocc", "microocc_f", "sex", "parent_sex"))
# micro_trans[, status := status/100]
# micro_trans[, status_f := status_f/100]
# micro_trans[, prestige := prestige/100]
# micro_trans[, prestige_f := prestige_f/100]
# 
for(c_var in vars){
  #  counts[, paste0(c_var, "_absdiff") := abs(get(c_var) - get(paste0(c_var, "_f")))]
  micro_trans[, paste0(c_var, "_diff") := abs(get(c_var) - get(paste0(c_var, "_f")))]
  #  gen[, paste0(c_var, "_absdiff") := abs(get(c_var) - get(paste0(c_var, "_f")))]
}

# save a meso trans version
meso_trans <- micro_trans[,lapply(.SD, weighted.mean, w = N), .SDcols = names(micro_trans)[names(micro_trans) %like% "_fac$"], by = .(mesoocc, parent_sex, sex)]
meso_trans_N <- micro_trans[,.(N = sum(N)), by = .(parent_sex, sex, mesoocc, mesoocc_f)]


# do some clustering algorithm and see how much information is shared
micro_trans[,euclid_diff := rowMeans(.SD), .SDcols = names(micro_trans)[names(micro_trans) %like% "_diff$" & names(micro_trans) %like% "fact_"]]

micro_trans <- micro_trans[!is.na(euclid_diff)]

micro_trans <- micro_trans[,.(macroocc, macroocc_f, mesoocc, mesoocc_f,microocc, microocc_f, N, euclid_diff, sex, parent_sex)]

micro_trans[, closest := frankv(euclid_diff, order = 1), by = .(microocc_f, sex, parent_sex)]

micro_trans[closest %in% c(2,69), .(microocc_f, microocc, mesoocc_f, mesoocc, closest)] %>% unique() %>% .[order(microocc_f, closest )] %>% 
  saveRDS(.,paste0( "../outputs/", model_version,"/euclid_dists.rds"))


father_res <- micro_trans[sex == 1 & parent_sex == 1,.(within_1 = sum(N[closest == 1]), 
                                                       within_2 = sum(N[closest <= 2]), 
                                                       within_3 = sum(N[closest <= 3]), 
                                                       #within_4 = sum(N[closest <= 4]), 
                                                       within_5 = sum(N[closest <= 5]), 
                                                       #within_7 = sum(N[closest <= 7]), 
                                                       within_10 = sum(N[closest <= 10]), 
                                                       #within_15 = sum(N[closest <= 15]),
                                                       within_20 = sum(N[closest <= 20]), 
                                                       total_father = sum(N)), by= .(microocc_f, sex, parent_sex)]


father_res[,paste0(names(father_res)[names(father_res) %like% "within"]) := lapply(.SD, function(x){x/total_father}), .SDcols = names(father_res)[names(father_res) %like% "within"]]

setnames(father_res, c("Micro-Class", "Child Sex", "Parent Sex", paste0("Within ", c(1,2,3,5,10,20)), "Total N"))
father_res[,`Child Sex` := as.character(`Child Sex`)]
father_res[`Child Sex` ==1, `Child Sex` :="Sons"]
father_res[`Child Sex` ==2, `Child Sex` :="Daughters"]
father_res[,`Parent Sex` := as.character(`Parent Sex`)]
father_res[`Parent Sex` ==1, `Parent Sex` :="Fathers"]
father_res[`Parent Sex` ==2, `Parent Sex` :="Mothers"]
father_res[order(`Parent Sex`, `Child Sex`)]



father_res <- micro_trans[,.(within_1 = sum(N[closest == 1]), 
                             within_2 = sum(N[closest <= 2]), 
                             within_3 = sum(N[closest <= 3]), 
                             #within_4 = sum(N[closest <= 4]), 
                             within_5 = sum(N[closest <= 5]), 
                             # within_7 = sum(N[closest <= 7]), 
                             within_10 = sum(N[closest <= 10]), 
                             #within_15 = sum(N[closest <= 15]),
                             within_20 = sum(N[closest <= 20]), 
                             total_father = sum(N)), by= .( sex, parent_sex)]


father_res[,paste0(names(father_res)[names(father_res) %like% "within"]) := lapply(.SD, function(x){x/total_father}), .SDcols = names(father_res)[names(father_res) %like% "within"]]

setnames(father_res, c("Child Sex", "Parent Sex", paste0("Within ", c(1,2,3,5,10,20)), "Total N"))
father_res[,`Child Sex` := as.character(`Child Sex`)]
father_res[`Child Sex` ==1, `Child Sex` :="Sons"]
father_res[`Child Sex` ==2, `Child Sex` :="Daughters"]
father_res[,`Parent Sex` := as.character(`Parent Sex`)]
father_res[`Parent Sex` ==1, `Parent Sex` :="Fathers"]
father_res[`Parent Sex` ==2, `Parent Sex` :="Mothers"]
father_res<- father_res[,.SD, .SDcols = names(father_res)[c(2,1,3,4,5,6,7,8,9)]]
father_res[order(`Parent Sex`, `Child Sex`)]

col_names <- paste0("Within ", c(1,2,3,5,10, 20))
father_res[,  (col_names) := lapply(.SD, formatC, format = "f", digits = 2), .SDcols = col_names]
father_res[,`Total N` :=  prettyNum(`Total N`, big.mark = ",")]

kable(father_res,  format = "latex",
      caption = "Movement Between Nearest Micro-Classes, Based On Euclidian Distance",
      row.names = F,longtable = T, escape = F,align = c("l", "l", rep("c", 7)),
      col.names = c("Parent Gender", "Child Gender", paste0("Within ", c(1,2,3,5,10, 20)), "Total N"), 
      booktabs = TRUE,
      label = "Euclidian",
      linesep = "") %>% 
  save_kable(file = paste0("../outputs/", model_version, "/table5.tex"))


