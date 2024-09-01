library(ggplot2)
library(data.table)
library(dplyr)

# load in data and relable

dt <- readRDS( "../ref/figure_2_data.rds")

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
