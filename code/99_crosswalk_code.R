occ50_xwalk <- readxl::read_xlsx("../ref/Census_integrated_occ_crosswalks.xlsx", sheet = 1) %>% data.table()
occ50_xwalk <- occ50_xwalk[,.(`OCC1950`, `ACS 2003-`, `Occupation category description`)]
setnames(occ50_xwalk, c("OCC1950", "CPS Code", "OCC1950_name"))

# occ50_xwalk[OCC1950 == 17, `CPS Code` := 2205]# teachers
# occ50_xwalk[OCC1950 == 25, `CPS Code` := 2205]
# occ50_xwalk[OCC1950 == 28, `CPS Code` := 2205]
# occ50_xwalk[OCC1950 == 29, `CPS Code` := 2205]
occ50_xwalk[OCC1950 == 35, `CPS Code` := 1545]
occ50_xwalk[OCC1950 == 63, `CPS Code` := 1935]
occ50_xwalk[OCC1950 == 75, `CPS Code` := 3090]
#occ50_xwalk[OCC1950 == 100, `CPS Code` := 205]#
#occ50_xwalk[OCC1950 == 123, `CPS Code` := 200]#
occ50_xwalk[OCC1950 == 360, `CPS Code` := 5040]
occ50_xwalk[OCC1950 == 365, `CPS Code` := 5040]
occ50_xwalk[OCC1950 == 410, `CPS Code` := NA]## auctioneer
occ50_xwalk[OCC1950 == 430, `CPS Code` := 4950]
occ50_xwalk[OCC1950 == 525, `CPS Code` := 8465]
occ50_xwalk[OCC1950 == 542, `CPS Code` := 9210]
occ50_xwalk[OCC1950 == 595, `CPS Code` := NA]# armed services
occ50_xwalk[OCC1950 == 614, `CPS Code` := NA]# apprentice
occ50_xwalk[OCC1950 == 615, `CPS Code` := NA]# apprentice
occ50_xwalk[OCC1950 == 625, `CPS Code` := 9122]
occ50_xwalk[OCC1950 == 630, `CPS Code` := 6441]
occ50_xwalk[OCC1950 == 675, `CPS Code` := 8365]
#occ50_xwalk[OCC1950 == 681, `CPS Code` := 9240]###
occ50_xwalk[OCC1950 == 751, `CPS Code` := 4655]
occ50_xwalk[OCC1950 == 910, `CPS Code` := 6100]#
occ50_xwalk[OCC1950 == 960, `CPS Code` := 9130]#
occ50_xwalk[OCC1950 == 960, `CPS Code` := 9750]#
occ50_xwalk[OCC1950 == 764, `CPS Code` := 4220]#

occ50_xwalk <- occ50_xwalk[OCC1950 != 635]
occ50_xwalk <- rbind(occ50_xwalk, 
                     data.table(OCC1950 = 600, `CPS Code` = 7160, 
                                OCC1950_name = "Apprentice auto Mmechanics"))
occ50_xwalk <- rbind(occ50_xwalk, 
                     data.table(OCC1950 = 600, `CPS Code` = 7200, 
                                OCC1950_name = "Apprentice auto Mmechanics"))
occ50_xwalk <- rbind(occ50_xwalk, 
                     data.table(OCC1950 = 600, `CPS Code` = 7210, 
                                OCC1950_name = "Apprentice auto Mmechanics"))
occ50_xwalk <- occ50_xwalk[OCC1950 != 4]
#occ50_xwalk <- occ50_xwalk[!is.na(`CPS Code`)]

namez <- occ50_xwalk[!is.na(OCC1950_name),.(OCC1950_name = OCC1950_name[1]), by = OCC1950] %>% unique()
namez <- namez[!is.na(OCC1950)]
occ50_xwalk <- merge(occ50_xwalk, namez, by = "OCC1950")
occ50_xwalk <- occ50_xwalk[OCC1950 %in% 0:990]
occ50_xwalk[, OCC1950_name := OCC1950_name.y]
occ50_xwalk[, c("OCC1950_name.x", "OCC1950_name.y") := NULL]



#occ50_xwalk <- occ50_xwalk[!is.na(`CPS Code`)]
#
# manually correct missing 
manual = data.table(OCC1950 = gen[,unique(occ1950)], 
                    `CPS Code`= NA_integer_, 
                    OCC1950_name = NA_character_)
manual[OCC1950 == 5, `CPS Code` := 272021]
manual[OCC1950 == 10, `CPS Code` := 119033]
manual[OCC1950 == 13, `CPS Code` := 251042]
manual[OCC1950 == 14, `CPS Code` := 251052]
manual[OCC1950 == 15, `CPS Code` := 251063]
manual[OCC1950 == 16, `CPS Code` := 251032]
manual[OCC1950 == 18, `CPS Code` := 251022]
manual[OCC1950 == 23, `CPS Code` := 251054]
manual[OCC1950 == 24, `CPS Code` := 251066]
manual[OCC1950 == 52, `CPS Code` := 259021]
manual[OCC1950 == 59, `CPS Code` := 291141]
manual[OCC1950 == 84, `CPS Code` := 193099]
manual[OCC1950 == 91, `CPS Code` := 251069]
manual[OCC1950 == 95, `CPS Code` := 519061]
manual[OCC1950 == 204, `CPS Code` := 132072]
manual[OCC1950 == 210, `CPS Code` := 131041]
manual[OCC1950 == 240, `CPS Code` := 535031]
manual[OCC1950 == 250, `CPS Code` := 112032]
manual[OCC1950 == 260, `CPS Code` := 119199]
manual[OCC1950 == 280, `CPS Code` := 131023]
manual[OCC1950 == 300, `CPS Code` := 131011]
manual[OCC1950 == 341, `CPS Code` := 439071]
manual[OCC1950 == 350, `CPS Code` := 439061]
#manual[OCC1950 == 365, `CPS Code` := 432099]
manual[OCC1950 == 430, `CPS Code` := NA]# Peddler
manual[OCC1950 == 490, `CPS Code` := 419099]
manual[OCC1950 == 504, `CPS Code` := 472021]
manual[OCC1950 == 510, `CPS Code` := 472031]
manual[OCC1950 == 514, `CPS Code` := 271025]
manual[OCC1950 == 522, `CPS Code` := 472071]
manual[OCC1950 == 525, `CPS Code` := 516099]
manual[OCC1950 == 532, `CPS Code` := 454023]
manual[OCC1950 == 535, `CPS Code` := 514033]
manual[OCC1950 == 543, `CPS Code` := 499041]
manual[OCC1950 == 545, `CPS Code` := 493011]
manual[OCC1950 == 553, `CPS Code` := 493043]
manual[OCC1950 == 561, `CPS Code` := 514072]
manual[OCC1950 == 564, `CPS Code` := 472141]
manual[OCC1950 == 571, `CPS Code` := 515111]
manual[OCC1950 == 574, `CPS Code` := 472152]
manual[OCC1950 == 584, `CPS Code` := 519195]
manual[OCC1950 == 590, `CPS Code` := 516052]
manual[OCC1950 == 591, `CPS Code` := 472211]
manual[OCC1950 == 594, `CPS Code` := 271012]

manual[OCC1950 == 601, `CPS Code` := 472021]
manual[OCC1950 == 602, `CPS Code` := 472031]
manual[OCC1950 == 603, `CPS Code` := 472111]
manual[OCC1950 == 604, `CPS Code` := 514041]
manual[OCC1950 == 605, `CPS Code` := 499071]
manual[OCC1950 == 610, `CPS Code` := 472152]
manual[OCC1950 == 611, `CPS Code` := 474099]
manual[OCC1950 == 613, `CPS Code` := 515112]
#manual[OCC1950 == 614, `CPS Code` := 473019]
manual[OCC1950 == 623, `CPS Code` := 536011]
manual[OCC1950 == 630, `CPS Code` := 173031]
manual[OCC1950 == 632, `CPS Code` := 533031]
manual[OCC1950 == 634, `CPS Code` := 516061]
manual[OCC1950 == 635, `CPS Code` := 519022]
manual[OCC1950 == 640, `CPS Code` := 452092]
manual[OCC1950 == 641, `CPS Code` := 514051]
manual[OCC1950 == 642, `CPS Code` := 519051]
manual[OCC1950 == 660, `CPS Code` := 537199]
manual[OCC1950 == 661, `CPS Code` := 534041]
manual[OCC1950 == 681, `CPS Code` := 534022]
manual[OCC1950 == 700, `CPS Code` := 372012]
manual[OCC1950 == 710, `CPS Code` := 516011]
manual[OCC1950 == 720, `CPS Code` := 399099]
manual[OCC1950 == 752, `CPS Code` := 119081]
manual[OCC1950 == 761, `CPS Code` := 393031]
manual[OCC1950 == 782, `CPS Code` := 331012]
manual[OCC1950 == 785, `CPS Code` := 536011]
manual[OCC1950 == 830, `CPS Code` := 452093]
manual[OCC1950 == 840, `CPS Code` := 452093]
manual[OCC1950 == 940, `CPS Code` := 537062]
manual[OCC1950 == 970, `CPS Code` := 537062]
manual[OCC1950 == 764, `CPS Code` := 372012]

manual[OCC1950 == 999, `CPS Code` := NA]

manual[OCC1950 == 3, `CPS Code` := 171011]
manual[OCC1950 == 12, `CPS Code` := 251041]

# manual[OCC1950 == 28, `CPS Code` := NA]# non-scientific subjects
# manual[OCC1950 == 29, `CPS Code` := NA]# subjet not specified
manual[OCC1950 == 33, `CPS Code` := 271021]
#manual[OCC1950 == 35, `CPS Code` := 173019]
manual[OCC1950 == 54, `CPS Code` := 119171]
manual[OCC1950 == 56, `CPS Code` := 254022]
manual[OCC1950 == 65, `CPS Code` := 192042]
manual[OCC1950 == 67, `CPS Code` := 152021]
manual[OCC1950 == 71, `CPS Code` := 291229]
#manual[OCC1950 == 75, `CPS Code` := 291062]
manual[OCC1950 == 77, `CPS Code` := 399032]
manual[OCC1950 == 82, `CPS Code` := 193033]
manual[OCC1950 == 91, `CPS Code` := 399031]
manual[OCC1950 == 96, `CPS Code` := 173029]
#manual[OCC1950 == 123, `CPS Code` := 119013]
manual[OCC1950 == 201, `CPS Code` := 131021]
manual[OCC1950 == 205, `CPS Code` := 411011]
manual[OCC1950 == 270, `CPS Code` := 119131]
manual[OCC1950 == 304, `CPS Code` := 396011]
manual[OCC1950 == 322, `CPS Code` := 435032]
manual[OCC1950 == 325, `CPS Code` := 435021]
#manual[OCC1950 == 365, `CPS Code` := 432099]
#manual[OCC1950 == 410, `CPS Code` := 419099]
manual[OCC1950 == 430, `CPS Code` := NA] # peddler huckster
manual[OCC1950 == 490, `CPS Code` := 412031]
manual[OCC1950 == 520, `CPS Code` := 515112]
manual[OCC1950 == 524, `CPS Code` := 514022]
#manual[OCC1950 == 525, `CPS Code` := 516099]
manual[OCC1950 == 531, `CPS Code` := 514191]
manual[OCC1950 == 541, `CPS Code` := 534011]
#manual[OCC1950 == 542, `CPS Code` := 534022]
manual[OCC1950 == 555, `CPS Code` := 513092]
manual[OCC1950 == 562, `CPS Code` := 393021]
manual[OCC1950 == 565, `CPS Code` := 472142]
manual[OCC1950 == 570, `CPS Code` := 514061]
manual[OCC1950 == 572, `CPS Code` := 499063]
manual[OCC1950 == 580, `CPS Code` := 514023]
manual[OCC1950 == 612, `CPS Code` := 472211]
#manual[OCC1950 == 614, `CPS Code` := 473019]
#manual[OCC1950 == 615, `CPS Code` := 473019]
manual[OCC1950 == 622, `CPS Code` := 475032]
#manual[OCC1950 == 624, `CPS Code` := 534022]
manual[OCC1950 == 625, `CPS Code` := 533051]
manual[OCC1950 == 631, `CPS Code` := 536061]
manual[OCC1950 == 645, `CPS Code` := 516099]
manual[OCC1950 == 660, `CPS Code` := 475044]
#manual[OCC1950 == 675, `CPS Code` := 516065]
#manual[OCC1950 == 681, `CPS Code` := 534022]
manual[OCC1950 == 684, `CPS Code` := 516063]
manual[OCC1950 == 690, `CPS Code` := NA]# Operative and kindred workers (n.e.c.)
manual[OCC1950 == 720, `CPS Code` := 399099]
#manual[OCC1950 == 751, `CPS Code` := 399099]
manual[OCC1950 == 760, `CPS Code` := 359031]
manual[OCC1950 == 771, `CPS Code` := 333051]
manual[OCC1950 == 772, `CPS Code` := 299099]
manual[OCC1950 == 790, `CPS Code` := 399099]
#manual[OCC1950 == 910, `CPS Code` := 453031]
manual[OCC1950 == 920, `CPS Code` := 537061]
manual[OCC1950 == 930, `CPS Code` := 373011]
#manual[OCC1950 == 960, `CPS Code` := 537199]

manual[OCC1950 == 100, `CPS Code` := 119013]
manual[OCC1950 == 123, `CPS Code` := 119013]
manual[OCC1950 == 624, `CPS Code` := 534022]
manual[OCC1950 == 751, `CPS Code` := 311122]
manual[OCC1950 == 910, `CPS Code` := 453031]

manual[OCC1950 == 17, `CPS Code` := 251064]
manual <- rbind(manual, data.table(OCC1950 = 17, `CPS Code` = 251053), fill = T)
manual[OCC1950 == 19, `CPS Code` := 251071]
manual <- rbind(manual, data.table(OCC1950 = 19, `CPS Code` = 251072), fill = T)
manual[OCC1950 == 25, `CPS Code` := 251022]
manual[OCC1950 == 26, `CPS Code` := 251051]
manual <- rbind(manual, data.table(OCC1950 = 26, `CPS Code` = 251043), fill = T)
manual[OCC1950 == 27, `CPS Code` := 251069]
manual <- rbind(manual, data.table(OCC1950 = 27, `CPS Code` = 251067), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 27, `CPS Code` = 251065), fill = T)
manual[OCC1950 == 4, `CPS Code` := 271013]
manual <- rbind(manual, data.table(OCC1950 = 4, `CPS Code` = 251121), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 4, `CPS Code` = 271014), fill = T)
manual[OCC1950 == 28, `CPS Code` := 251081]
manual <- rbind(manual, data.table(OCC1950 = 28, `CPS Code` = 251082), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 28, `CPS Code` = 251062), fill = T)
manual[OCC1950 == 29, `CPS Code` := 251011]
manual <- rbind(manual, data.table(OCC1950 = 29, `CPS Code` = 251031), fill = T)
manual[OCC1950 == 67, `CPS Code` := 152021]
manual[OCC1950 == 100, `CPS Code` := 119013]
manual <- rbind(manual, data.table(OCC1950 = 100, `CPS Code` = 452093), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 100, `CPS Code` = 452092), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 100, `CPS Code` = 452091), fill = T)
manual[OCC1950 == 123, `CPS Code` := 451011]
manual[OCC1950 == 360, `CPS Code` := 432021]
manual[OCC1950 == 365, `CPS Code` := 432021]
manual[OCC1950 == 410, `CPS Code` := 131022]# auctioneer
manual[OCC1950 == 501, `CPS Code` := 514122]
manual <- rbind(manual, data.table(OCC1950 = 501, `CPS Code` = 514192), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 501, `CPS Code` = 514193), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 501, `CPS Code` = 514194), fill = T)

manual[OCC1950 == 595, `CPS Code` := NA] #armed services
manual[OCC1950 == 612, `CPS Code` := 514122]
manual <- rbind(manual, data.table(OCC1950 = 612, `CPS Code` = 514192), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 612, `CPS Code` = 514193), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 612, `CPS Code` = 514194), fill = T)
manual[OCC1950 == 614, `CPS Code` := 339091]
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 514194), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 454011), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 472061), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473011), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473012), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473013), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473014), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473015), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473016), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473031), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 473051), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 474061), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 474071), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 513093), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 519198), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 537063), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 614, `CPS Code` = 537081), fill = T)

manual[OCC1950 == 615, `CPS Code` := 339091]
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 514194), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 454011), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 472061), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473011), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473012), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473013), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473014), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473015), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473016), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473031), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 473051), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 474061), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 474071), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 513093), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 519198), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 537063), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 615, `CPS Code` = 537081), fill = T)
manual[OCC1950 == 624, `CPS Code` := 534031]
manual[OCC1950 == 681, `CPS Code` := 534031]
manual[OCC1950 == 720, `CPS Code` := 371011]
manual[OCC1950 == 910, `CPS Code` := 453031]
manual[OCC1950 == 960, `CPS Code` := 537121]
manual <- rbind(manual, data.table(OCC1950 = 762, `CPS Code` = 332021), fill = T)
manual[OCC1950 == 57, `CPS Code` := 272041]
manual <- rbind(manual, data.table(OCC1950 = 57, `CPS Code` = 272042), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 290, `CPS Code` = 119111), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 63, `CPS Code` = 192042), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 95, `CPS Code` = 194099), fill = T)
manual <- rbind(manual, data.table(OCC1950 = 83, `CPS Code` = 439111), fill = T)
manual[OCC1950 == 582, `CPS Code` := 516041]


#
skills[, cps_codes := as.character(`CPS Code`)]
skills <- merge(skills, occ50_xwalk[!is.na(`CPS Code`)]#[!is.na(`CPS Code`),.(`OCC1950` = unique(`OCC1950`)[1], 
                #         `OCC1950_name` = unique(`OCC1950_name`)[1]), by = `CPS Code`]
                , by.x = "cps_codes", by.y = "CPS Code", all.x = T, allow.cartesian = T)
manual[, `CPS Code` := as.character(`CPS Code`)]
skills <- merge(skills, manual, by.x = "OCCSOC", by.y = "CPS Code", all.x = T, allow.cartesian = T)
temp <- skills[OCC1950.x != OCC1950.y]
temp1 <- copy(temp)
temp2 <- copy(temp)
temp1[, OCC1950 := OCC1950.x]
temp1[, OCC1950_name := OCC1950_name.x]
temp1[,c("OCC1950.x", "OCC1950.y", "OCC1950_name.y", "OCC1950_name.x") := NULL]
temp2[, OCC1950 := OCC1950.y]
temp2[, OCC1950_name := OCC1950_name.y]
temp2[,c("OCC1950.x", "OCC1950.y", "OCC1950_name.y", "OCC1950_name.x") := NULL]
skills[!is.na(OCC1950.y), OCC1950 := OCC1950.y]
skills[is.na(OCC1950.y), OCC1950 := OCC1950.x]
skills[is.na(OCC1950_name.x), OCC1950_name := OCC1950_name.y]
skills[!is.na(OCC1950_name.x), OCC1950_name := OCC1950_name.x]
skills[,c("OCC1950.x", "OCC1950.y", "OCC1950_name.y", "OCC1950_name.x") := NULL]
skills <- rbindlist(list(skills, temp1, temp2))
# collapse to OCCSOC
skills[is.na(OCC1950_name), OCC1950_name := `CPS Occupational Title`]
skills[OCC1950 == 250, OCC1950_name := "Officials and administrators (n.e.c.), public administration"]
skills[OCC1950 == 260, OCC1950_name := "Officials, lodge, society, union, etc."]
skills[OCC1950 == 280, OCC1950_name := "Purchasing agents and buyers (n.e.c.)"]
skills[OCC1950 == 611, OCC1950_name := "Apprentices, building trades (n.e.c.)"]

#

# try to rename again
occ1950_names <- fread("../ref/occ1950_recode.csv")
skills[, OCC1950 := as.character(OCC1950)]
skills <- merge(skills, occ1950_names[,.(V1, V2)], all.x = T, by.x = "OCC1950", by.y = "V1")
skills[, OCC1950_name := V2]
skills[, V2 := NULL]

skills[OCC1950 %in% 12:29, OCC1950_name := paste0("Post-Secondary Instructors/Deans: ", OCC1950_name)]

# 

# dont use any soc code twice
skills[,.(OCC1950,OCCSOC )] %>% unique() %>% 
  .[!is.na(OCC1950),.(OCCSOC,OCC1950 )] %>% .[duplicated(.[!is.na(OCC1950),.(OCCSOC)])] %>% unique() -> dupes
dupes <- dupes$OCCSOC
skills[OCCSOC %in% dupes & !is.na(OCC1950),.(OCCSOC, OCC1950_name, OCC1950)] %>% unique() %>% .[order(OCCSOC)]

skills <- skills[!(OCCSOC == 152021 & OCC1950 == 83)]
skills <- skills[!(OCCSOC == 119033 & OCC1950 == 93)]
skills <- skills[!(OCCSOC == 119081 & OCC1950 == 290)]
skills <- skills[!(OCCSOC == 119199 & OCC1950 == 290)] ###
skills <- skills[!(OCCSOC == 131011 & OCC1950 == 290)]
skills <- skills[!(OCCSOC == 132072 & OCC1950 == 290)]
#skills <- skills[!(OCCSOC == 152021 & OCC1950 == 67)] # check last round hunter
skills <- skills[!(OCCSOC == 172021 & OCC1950 == 12)]
skills <- skills[!(OCCSOC == 331012)]
skills <- skills[!(OCCSOC == 359031 & OCC1950 == 754)]
skills <- skills[!(OCCSOC == 411011 & OCC1950 == 290)]
skills <- skills[!(OCCSOC == 439061 & OCC1950 == 350)]
skills <- skills[!(OCCSOC == 452092 & OCC1950 == 820)]
skills <- skills[!(OCCSOC == 454023 & OCC1950 == 950)]
skills <- skills[!(OCCSOC == 472211 & OCC1950 == 612)]
skills <- skills[!(OCCSOC == 493043 & OCC1950 == 554)]

skills <- skills[!(OCCSOC == 499063 & OCC1950 == 534)]
skills <- skills[!(OCCSOC == 499062 & OCC1950 == 534)]
skills <- skills[!(OCCSOC == 499061 & OCC1950 == 554)]
skills <- skills[!(OCCSOC == 499071 & OCC1950 == 554)]
skills <- skills[!(OCCSOC == 499041 & OCC1950 == 554)]

skills <- skills[!(OCCSOC == 513092 & OCC1950 == 690)]
skills <- skills[!(OCCSOC == 514022 & OCC1950 == 554)]
skills <- skills[!(OCCSOC == 514033 & OCC1950 == 554)]
skills <- skills[!(OCCSOC == 514051 & OCC1950 == 680)]
skills <- skills[!(OCCSOC == 514061 & OCC1950 == 690)]
skills <- skills[!(OCCSOC == 514072 & OCC1950 == 690)]

skills <- skills[!(OCCSOC == 516052 & OCC1950 == 633)]
skills <- skills[!(OCCSOC == 516063 & OCC1950 == 675)]
skills <- skills[!(OCCSOC == 519051 & OCC1950 == 680)]
skills <- skills[!(OCCSOC == 519061 & OCC1950 == 690)]
skills <- skills[!(OCCSOC == 519195 & OCC1950 == 690)]
skills <- skills[!(OCCSOC == 533031 & OCC1950 == 683)]
skills <- skills[!(OCCSOC == 533031 & OCC1950 == 960)]
skills <- skills[!(OCCSOC == 499091 & OCC1950 == 632)]
skills <- skills[!(OCCSOC == 533032 & OCC1950 == 683)]
skills <- skills[!(OCCSOC == 533033 & OCC1950 == 960)]

skills <- skills[!(OCCSOC == 534011 & OCC1950 == 542)]
skills <- skills[!(OCCSOC == 535031 & OCC1950 == 240)]
skills <- skills[!(OCCSOC == 535031 & OCC1950 == 673)]
skills <- skills[!(OCCSOC == 537061 & OCC1950 == 970)]
skills <- skills[!(OCCSOC == 537062 & OCC1950 == 970)]
skills <- skills[!(OCCSOC == 493031 & OCC1950 == 550)]
skills <- skills[!(OCCSOC == 519022 & OCC1950 == 690)]
skills <- skills[!(OCCSOC == 371011 & OCC1950 == 770)]
skills <- skills[!(OCCSOC == 372011 & OCC1950 == 764)]
skills <- skills[!(OCCSOC == 131111 & OCC1950 == 45)]
skills <- skills[!(OCCSOC == 332021 & OCC1950 == 53)]
skills <- skills[!(OCCSOC == 119111 & OCC1950 == 58)]
skills <- skills[!(OCCSOC == 194099 & OCC1950 == 63)]
skills <- skills[!(OCCSOC == 439111 & OCC1950 == 390)]
skills <- skills[!(OCCSOC == 472041 & OCC1950 == 582)]
skills <- skills[!(OCCSOC == 472042 & OCC1950 == 582)]
skills <- skills[!(OCCSOC == 472043 & OCC1950 == 582)]
skills <- skills[!(OCCSOC == 472044 & OCC1950 == 582)]
skills <- skills[!(OCCSOC == 252059 & OCC1950 == 57)]
