library(ggplot2)
library(data.table)
library(dplyr)

# load in data and relable
model_version <- "20240820_submission_final"
dt <- readRDS( "../ref/figure_1_data.rds")

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

library(ggh4x)

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

tiff(paste0("../outputs/", model_version, "/figure_1.tiff"), height = 6.95, width = 7.3,res = 800, units = 'in')
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
  scale_color_manual(values = c("gray20", "gray70"))# + theme_bw()

print(gg)


dir.create(paste0("../outputs/", model_version), recursive = T)

pdf(paste0("../outputs/", model_version, "/figure_1_bw.pdf"), height = 6.95, width = 7.3)
print(gg)
dev.off()


tiff(paste0("../outputs/", model_version, "/figure_1_bw.tiff"), height = 6.95, width = 7.3,res = 800, units = 'in')
print(gg)
dev.off()
