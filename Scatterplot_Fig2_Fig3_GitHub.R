## Code for: "Soil Characterization for a Long-Term Soil Health Research Vineyard in Eastern Washington" - SSSAJ 
## Figures 2 and 3 ##



#### Packages ####
if(!require(tidyr)){
  install.packages("tidyr")
  require(tidyr)
}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
}
if(!require(stringr)){
  install.packages("stringr")
  require(stringr)
}
if(!require(emmeans)){
  install.packages("emmeans")
  require(emmeans)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(agridat)){
  install.packages("agridat")
  require(agridat)
}
if(!require(lme4)){
  install.packages("lme4")
  require(lme4)
}
if(!require(lmerTest)){
  install.packages("lmerTest")
  require(lmerTest)
}
if(!require(nlme)){
  install.packages("nlme")
  require(nlme)
}
if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}
if(!require(GGally)){
  install.packages("GGally")
  require(GGally)
}
if(!require(predictmeans)){
  install.packages("predictmeans")
  require(predictmeans)
}
if(!require(rcompanion)){
  install.packages("rcompanion")
  require(rcompanion)
}
if(!require(multcomp)){
  install.packages("multcomp")
  require(multcomp)
}
if(!require(mapview)){
  install.packages("mapview")
  require(mapview)
}
if(!require(ggtern)){
  install.packages("ggtern")
  require(ggtern)
}
if(!require(washi)){
  install.packages("washi")
  require(washi)
}
if(!require(ragg)){
  install.packages("ragg")
  require(ragg)
}
if(!require(systemfonts)){
  install.packages("systemfonts")
  require(systemfonts)
}
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
}
if(!require(forcats)){
  install.packages("forcats")
  require(forcats)
}
if(!require(patchwork)){
  install.packages("patchwork")
  require(patchwork)
}


#### Loading data and QC ####
## uploading data locally from source "GCH_PSR_Soil_SoilHealthData_Clean(SoilData)_check.xlsx"


SoilHealthData_Clean<- read_xlsx("~/PSR_Vineyard/Data/Characterization Paper 2025/PSR_Soil_SoilHealthData_Clean_02_21_2025.xlsx",
                                 sheet = "in",
                                 na = c("", 
                                        "ND")) %>% 
  mutate(totalN_percent = `totalN_%`, 
         totalC_percent = `totalC_%`,
         inorganicC_percent = `inorganicC_%`,
         TOC_percent = `TOC_%`,
         OM_percent = `OM_%`,
         minC = `24hrminC_mgC.kg.day`,
         sand_percent = `sand_%`,
         silt_percent = `silt_%`,
         clay_percent = `clay_%`,
         CEC = `CEC_meq.100g`,
         EC = `EC_mmhos.cm`,
         ACE_protein = `ace_g.protein.kg.soil`,
         PotentiallyMinNitrate = `pmN_nitrateN _mg.kg`,
         PotentiallyMinAmmonium = `pmN_ammN_mg.kg`,
         fact_replicateNo = as.factor(replicateNo),
         fact_TreatmentNo = as.factor(TreatmentNo)) %>%
    mutate(minC = if_else(minC > 300, NA, minC)) %>%
    mutate(poxC_mg.kg_AddZero = as.numeric(ifelse(str_starts(poxC_mg.kg, "<"), 
                                                0,
                                                poxC_mg.kg)),
         mineralizableNitrogen_AddZero = as.numeric(ifelse(str_starts(mineralizableNitrogen, "<"), 
                                                           0,
                                                           mineralizableNitrogen)),
  ) %>%
  mutate(SoilDepthRange = paste(UpperHorizonDepth, lowerHorizonDepth, sep="_")) %>% 
  mutate(GrowthCycle = ifelse(growthCycle == "preplant_2023", paste("PrePlant_2023"),
                              "Fall_2023"))

summary(SoilHealthData_Clean)

SoilPLFAData_Clean<- read_xlsx("~/PSR_Vineyard/Data/Characterization Paper 2025/ElCaGi_ALL_PLFA_clean_donottouch.xlsx", 
                               sheet = "Amnt",
) %>%
  mutate(fact_replicateNo = as.factor(replicateNo),
         fact_TreatmentNo = as.factor(TreatmentNo)) %>%
  rename(TotalPLFA_mass = `Total PLFA (pmols/g)`,
         TotalPLFA_count = `Total PLFA#`,
         SoilDepthRange = DepthRange,
         GrowthCycle = growthCycle) %>%
  mutate(GrowthCycle = ifelse(GrowthCycle == "preplant_2023", paste("PrePlant_2023"),
                              "Fall_2023"))



# Creating an analysis data frame (data selection and summary)
#Data Preparation for Analysis:

SoilHealthData_Clean_AnalysisDat<- SoilHealthData_Clean %>%
  dplyr::select(GrowthCycle, fieldID, variety, sampleLocation, 
                blockNo, fact_replicateNo, 
                fact_TreatmentNo, SoilDepthRange,
                totalC_percent, inorganicC_percent, TOC_percent, OM_percent, poxC_mg.kg_AddZero, molMnred.kg,
                minC,
                PotentiallyMinNitrate, PotentiallyMinAmmonium, mineralizableNitrogen_AddZero, totalN_percent,
                olsenP_mg.kg,
                pH, pH_Hconc,
                ACE_protein,
                CEC,
                EC,
                bd_g.cm3_soil, bd_g.cm3_rock) 
summary(SoilHealthData_Clean_AnalysisDat) 
head(SoilHealthData_Clean_AnalysisDat)


SoilPLFAData_Clean_AnalysisDat<- SoilPLFAData_Clean %>%
  dplyr::select(GrowthCycle, fieldID, variety, sampleLocation, 
                blockNo, fact_replicateNo, 
                fact_TreatmentNo, SoilDepthRange,
                TotalPLFA.nmol, TotalPLFA_mass, TotalPLFA_count) 

head(SoilPLFAData_Clean_AnalysisDat)
unique(SoilPLFAData_Clean_AnalysisDat$GrowthCycle)


##### Plotting data, QC, and Soil Texture #####
SoilPLFAData_Clean_AnalysisDat<- SoilPLFAData_Clean %>% 
  dplyr::select(GrowthCycle, fieldID, variety, sampleLocation,
                fact_replicateNo, fact_TreatmentNo, SoilDepthRange,
                TotalPLFA.nmol, TotalPLFA_mass, TotalPLFA_count)
summary(SoilPLFAData_Clean_AnalysisDat)


#### Data frames for "Field Baseline" - Soil and PLFA ####

SoilHealthData_Clean_FieldPrePlantDat<- SoilHealthData_Clean_AnalysisDat %>% 
  filter(GrowthCycle == "PrePlant_2023") %>%
  mutate(SoilDepthRange = recode(SoilDepthRange, 
                                 "0_15"="0-15",
                                 "15_30"="15-30",
                                 "30_60"="30-60",
                                 "60_90"="60-90"),
         SoilDepthRange = factor(SoilDepthRange, levels = c("60-90", "30-60", "15-30", "0-15")))
View(SoilHealthData_Clean_FieldPrePlantDat)


SoilPLFAData_Clean_FieldPrePlantDat<- SoilPLFAData_Clean_AnalysisDat %>% 
  filter(GrowthCycle == "PrePlant_2023") %>%
  mutate(SoilDepthRange = recode(SoilDepthRange, 
                                 "0_15"="0-15",
                                 "15_30"="15-30",
                                 "30_60"="30-60",
                                 "60_90"="60-90"),
         SoilDepthRange = factor(SoilDepthRange, levels = c("60-90", "30-60", "15-30", "0-15")))



##### Summary of pre plant data #####
SoilHealthData_Clean_FieldBaselineDat_sum<- SoilHealthData_Clean_FieldPrePlantDat %>%
  dplyr::select(fact_replicateNo,SoilDepthRange,
                totalC_percent,inorganicC_percent,TOC_percent,
                minC,
                poxC_mg.kg_AddZero, molMnred.kg,
                PotentiallyMinNitrate, PotentiallyMinAmmonium, mineralizableNitrogen_AddZero, totalN_percent,
                olsenP_mg.kg, olsenP_mg.kg,
                pH, pH_Hconc,
                ACE_protein,
                CEC,
                EC,
                bd_g.cm3_soil, bd_g.cm3_rock) %>%
  group_by(fact_replicateNo,SoilDepthRange) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

View(SoilHealthData_Clean_FieldBaselineDat_sum)

SoilPLFAData_Clean_FieldBaselineDat_sum<- SoilPLFAData_Clean_FieldPrePlantDat %>%
  dplyr::select(fact_replicateNo,SoilDepthRange,
                TotalPLFA.nmol, TotalPLFA_mass, TotalPLFA_count) %>%
  group_by(fact_replicateNo,SoilDepthRange) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

### Raw stats for summary data ###
raw_stats_long_preplant <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(SoilDepthRange, parameter) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_long_preplant)

raw_stats_table_preplant <- raw_stats_long_preplant %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange)

print(raw_stats_table_preplant)
raw_stats_table_preplant <- raw_stats_long_preplant %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange)

print(raw_stats_table_preplant)

library(writexl)
write_xlsx(raw_stats_table_preplant,
           "SoilHealth_RawStats_PrePlant.xlsx")

### Raw stats for PLFA under-vine summary data ###
raw_stats_longPLFA_preplant <- SoilPLFAData_Clean_FieldBaselineDat_sum %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(SoilDepthRange, parameter) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_longPLFA_preplant)

raw_stats_tablePLFA_preplant <- raw_stats_longPLFA_preplant %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange)

print(raw_stats_tablePLFA_preplant)

raw_stats_tablePLFA_preplant <- raw_stats_longPLFA_preplant %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange)

print(raw_stats_tablePLFA_preplant)
library(writexl)
write_xlsx(raw_stats_tablePLFA_preplant,
           "SoilHealth_RawStats_PrePlant_PLFA.xlsx")



## Graphs with standard deviation ##


# totalC_percent line graph with standard deviation
SoilHealthData_sum_totalC_percent_lm1 <- lmer(totalC_percent ~ SoilDepthRange + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldBaselineDat_sum)
residplot(SoilHealthData_sum_totalC_percent_lm1)
anova(SoilHealthData_sum_totalC_percent_lm1)

emm_totC    <- emmeans(SoilHealthData_sum_totalC_percent_lm1, ~ SoilDepthRange)
cld_df <- data.frame(cld(emm_totC, Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  group_by(SoilDepthRange) %>%
  summarise(
    emmean = mean(totalC_percent, na.rm = TRUE),
    SD_raw = sd(totalC_percent, na.rm = TRUE)
  )
print(raw_stats)

emm_df <- raw_stats %>%
    left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03

total_c_percent_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD),
                 height = 1) +
 
  geom_text(aes(x = upper.SD + pad, label = trimws(.group)),
            size = 4, fontface = "bold", hjust = 0) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    labels = ifelse(seq_along(seq(0, 1, 0.1)) %% 2 == 1,
                    sprintf("%.1f", seq(0, 1, 0.1)), ""),
    position = "top",
    limits = c(0, NA)
  ) +
  coord_cartesian(xlim = c(0, NA)) +
  labs(x = "Total C (%)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(total_c_percent_preplot)


# TOC_percent line graph (using SD)
SoilHealthData_sum_TOC_percent_lm1 <- lmer(
  TOC_percent ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_TOC_percent_lm1)
anova(SoilHealthData_sum_TOC_percent_lm1)


cld_df <- data.frame(cld(emmeans(SoilHealthData_sum_TOC_percent_lm1, ~ SoilDepthRange), Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  group_by(SoilDepthRange) %>%
  summarise(
    emmean = mean(TOC_percent, na.rm = TRUE),
    SD_raw = sd(TOC_percent, na.rm = TRUE)
  )
print(raw_stats)

emm_df <- raw_stats %>%
  
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
   
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)


vals <- seq(0, 1, 0.1)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03


x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad

TOC_percent_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = ifelse(seq_along(vals) %% 2 == 1, sprintf("%.1f", vals), ""),
    position = "top",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(x = "Total Organic Carbon (%)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(TOC_percent_preplot)


# inorganicC_percent (using SD)
SoilHealthData_sum_inorganicC_percent_lm1 <- lmer(
  inorganicC_percent ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_inorganicC_percent_lm1)
anova(SoilHealthData_sum_inorganicC_percent_lm1)


cld_df <- data.frame(cld(emmeans(SoilHealthData_sum_inorganicC_percent_lm1, ~ SoilDepthRange), Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  group_by(SoilDepthRange) %>%
  summarise(
    emmean = mean(inorganicC_percent, na.rm = TRUE),
    SD_raw = sd(inorganicC_percent, na.rm = TRUE)
  )
print(raw_stats)

emm_df <- raw_stats %>%
  
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm   = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)


vals <- seq(0, 1, 0.1)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03
x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad

inorganicC_percent_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = ifelse(seq_along(vals) %% 2 == 1, sprintf("%.1f", vals), ""),
    position = "top",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(x = "TIC (%)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(inorganicC_percent_preplot)

## Figure 2   Carbon with Depth graph   ##


y_offset_TIC = 1.6
y_offset_TOC = -1 #


SoilHealthData_sum_totalC_percent_lm1 <- lmer(totalC_percent ~ SoilDepthRange + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldBaselineDat_sum)
SoilHealthData_sum_TOC_percent_lm1 <- lmer(TOC_percent ~ SoilDepthRange + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldBaselineDat_sum)
SoilHealthData_sum_inorganicC_percent_lm1 <- lmer(inorganicC_percent ~ SoilDepthRange + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldBaselineDat_sum)


cld_totalC <- data.frame(cld(emmeans(SoilHealthData_sum_totalC_percent_lm1, ~ SoilDepthRange), Letters = LETTERS)) %>% dplyr::mutate(carbon_type = "TC") # CHANGED: "Total C" to "TC"
cld_TOC <- data.frame(cld(emmeans(SoilHealthData_sum_TOC_percent_lm1, ~ SoilDepthRange), Letters = LETTERS)) %>% dplyr::mutate(carbon_type = "TOC")
cld_TIC <- data.frame(cld(emmeans(SoilHealthData_sum_inorganicC_percent_lm1, ~ SoilDepthRange), Letters = LETTERS)) %>% dplyr::mutate(carbon_type = "TIC")


combined_cld_df <- bind_rows(cld_totalC, cld_TOC, cld_TIC) %>%
  dplyr::select(SoilDepthRange, carbon_type, .group)


raw_stats_totalC <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(totalC_percent, na.rm = TRUE),
    SD_raw = sd(totalC_percent, na.rm = TRUE)
  ) %>%
  dplyr::mutate(carbon_type = "TC") 

raw_stats_TOC <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(TOC_percent, na.rm = TRUE),
    SD_raw = sd(TOC_percent, na.rm = TRUE)
  ) %>%
  dplyr::mutate(carbon_type = "TOC")
print(raw_stats_totalC)
raw_stats_TIC <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(inorganicC_percent, na.rm = TRUE),
    SD_raw = sd(inorganicC_percent, na.rm = TRUE)
  ) %>%
  dplyr::mutate(carbon_type = "TIC")


raw_stats_combined <- bind_rows(raw_stats_totalC, raw_stats_TOC, raw_stats_TIC)


emm_df_combined <- raw_stats_combined %>%
  left_join(combined_cld_df, by = c("SoilDepthRange", "carbon_type")) %>%
  dplyr::mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    
    lower.SD_RemNeg = pmax(lower.SD, 0),
    
    
    y_text = dplyr::case_when(
      carbon_type == "TIC" ~ Depth_mid_cm - y_offset_TIC,
      carbon_type == "TOC" ~ Depth_mid_cm - y_offset_TOC,
      TRUE ~ Depth_mid_cm
    )
  ) %>%
  dplyr::arrange(Depth_mid_cm)


emm_df_combined <- emm_df_combined %>%
  dplyr::group_by(carbon_type) %>%
  dplyr::mutate(is_deepest = Depth_mid_cm == max(Depth_mid_cm)) %>%
  dplyr::ungroup()


vals <- seq(0, 1, 0.1)
pad    <- diff(range(emm_df_combined$emmean, na.rm = TRUE)) * 0.03
extra <- diff(range(emm_df_combined$emmean, na.rm = TRUE)) * 0.02
x_max_for_labels <- max(emm_df_combined$upper.SD, na.rm = TRUE) + 4 * pad


combined_carbon_plot_single <- ggplot(
  emm_df_combined,
  aes(
    x = emmean, y = Depth_mid_cm,
    color = carbon_type, linetype = carbon_type, shape = carbon_type
  )
) +
  geom_path(size = 0.8) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  

  geom_text(
    data = dplyr::filter(emm_df_combined, !(carbon_type == "TIC" & is_deepest)),
    aes(y = y_text, x = upper.SD + pad, label = trimws(.group), color = carbon_type),
    size = 3, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  
  geom_text(
    data = dplyr::filter(emm_df_combined, carbon_type == "TIC" & is_deepest),
    aes(y = y_text, x = upper.SD + pad, label = trimws(.group), color = carbon_type),
    nudge_x = -extra,
    size = 3, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  

  scale_linetype_manual(values = c("TC" = "solid", "TOC" = "dashed", "TIC" = "dotdash")) +
  scale_shape_manual(values    = c("TC" = 16,     "TOC" = 17,      "TIC" = 15)) +
  scale_color_manual(values    = c("TC" = "black", "TOC" = "black", "TIC" = "black")) +
  
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = ifelse(seq_along(vals) %% 2 == 1, sprintf("%.1f", vals), ""),
    minor_breaks = seq(0, 1, 0.05),
    position = "top",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  
  labs(
    x = "Carbon (%)",
    y = "Soil Depth (cm)",
    color = "Carbon Type",
    linetype = "Carbon Type",
    shape = "Carbon Type"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.98, 0.3),
    legend.justification = c("right", "top"),
    legend.direction = "vertical",
    legend.background = element_rect(fill = NA, colour = NA),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold"),
    axis.title.x.top = element_text(face = "bold", size = 12, margin = margin(b = 10))
  )

print(combined_carbon_plot_single) ## Figure 2 Carbon with Depth


# minC line graph - SD


SoilHealthData_sum_minC_lm1<- lmer(minC ~ SoilDepthRange + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldBaselineDat_sum)
cld_df <- data.frame(cld(emmeans(SoilHealthData_sum_minC_lm1, ~ SoilDepthRange), Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(minC, na.rm = TRUE),
    SD_raw = sd(minC, na.rm = TRUE)
  ) %>%
  
  dplyr::mutate(
    SD_raw = dplyr::if_else(is.na(SD_raw), 0, SD_raw)
  )
print(raw_stats) 
emm_df <- raw_stats %>%
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  dplyr::mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  dplyr::arrange(Depth_mid_cm)


max_tick_limit <- 120 
vals <- seq(0, 120, by = 20) 


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.05
if (!is.finite(pad) || pad == 0) pad <- 2 

x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 15 * pad


minC_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0
  ) +
  
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = sprintf("%.0f", vals),
    position = "top",
    limits = c(0, max_tick_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(xlim = c(0, max(max_tick_limit, x_max_for_labels))) + 
  
  
  labs(
    x = bquote(bold("MinC (mg/kg/day)")), 
    y = "Soil Depth (cm)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    
    
    axis.title = element_text(face = "bold", color = "black"), 
    
    axis.title.x.top = element_text(margin = margin(b = 10)) 
  )

print(minC_preplot)

## POXC with standard deviation

SoilHealthData_sum_poxC_mg.kg_AddZero_lm1 <- lmer(
  poxC_mg.kg_AddZero ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1)
anova(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1)

cld_df <- data.frame(cld(emmeans(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1, ~ SoilDepthRange), Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(poxC_mg.kg_AddZero, na.rm = TRUE),
    SD_raw = sd(poxC_mg.kg_AddZero, na.rm = TRUE)
  )
print(raw_stats) 

emm_df <- raw_stats %>%
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  dplyr::mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
   
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  dplyr::arrange(Depth_mid_cm)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03


fixed_x_max <- 400 
vals <- seq(0, fixed_x_max, by = 100) 


x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 2 * pad

poxC_mg.kg_AddZero_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    
    breaks = vals, 
    
    labels = sprintf("%.0f", vals),
    position = "top",
    
    limits = c(0, fixed_x_max), 
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(xlim = c(0, max(fixed_x_max, x_max_for_labels))) + 
  labs(x = "POXC (mg/kg)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(poxC_mg.kg_AddZero_preplot)


# POXC (molMnred.kg) — using SD

SoilHealthData_sum_molMnred.kg_lm1 <- lmer(
  molMnred.kg ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_molMnred.kg_lm1)
anova(SoilHealthData_sum_molMnred.kg_lm1)


cld_df <- data.frame(cld(emmeans(SoilHealthData_sum_molMnred.kg_lm1, ~ SoilDepthRange), Letters = LETTERS))


raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(molMnred.kg, na.rm = TRUE),
    SD_raw = sd(molMnred.kg, na.rm = TRUE)
  )
print(raw_stats) 
emm_df <- raw_stats %>%
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  dplyr::mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>%
      lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  dplyr::arrange(Depth_mid_cm)

pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03

fixed_x_max <- .04
vals <- seq(0, fixed_x_max, by = .01) 

x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + .05 * pad

molMnred.kg_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals, 
    labels = sprintf("%.2f", vals),
    position = "top",
    limits = c(0, fixed_x_max), 
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, max(fixed_x_max, x_max_for_labels))) + 
  labs(x = bquote(bold("POXC (mol MnO"[4]^"-"*"/kg)")),  
       y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(molMnred.kg_preplot)


# Potentially Min NO3 (PotentiallyMinNitrate) — using SD
PotentiallyMinNitrate_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  group_by(SoilDepthRange) %>%
  summarise(
    emmean = mean(PotentiallyMinNitrate, na.rm = TRUE), 
    SD = sd(PotentiallyMinNitrate, na.rm = TRUE),       
    .groups = 'drop'
  )
print(PotentiallyMinNitrate_stats)
SoilHealthData_sum_PotentiallyMinNitrate_lm1 <- lmer(
  PotentiallyMinNitrate ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_PotentiallyMinNitrate_lm1)
anova(SoilHealthData_sum_PotentiallyMinNitrate_lm1)

PotentiallyMinNitrate_cld <- data.frame(
  cld(emmeans(SoilHealthData_sum_PotentiallyMinNitrate_lm1, ~ SoilDepthRange), Letters = LETTERS)
) %>%
  dplyr::select(SoilDepthRange, .group)

emm_df <- left_join(PotentiallyMinNitrate_stats, PotentiallyMinNitrate_cld, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SD_RemNeg = pmax(emmean - SD, 0),  
    upper.SD        = emmean + SD
  ) %>%
  arrange(Depth_mid_cm)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03
x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad

PotentiallyMinNitrate_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks   = scales::breaks_extended(n = 8),
    labels   = scales::label_number(accuracy = 1),
    position = "top",
    expand   = expansion(mult = c(0, 0))  
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(
    x = expression("Potentially Mineralizable NO"[3]*"-N (mg/kg)"),
    y = "Soil Depth (cm)"
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(PotentiallyMinNitrate_preplot)


# Potentially Min NH4 (PotentiallyMinAmmonium) — using SD

PotentiallyMinAmmonium_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(PotentiallyMinAmmonium, na.rm = TRUE), 
    SD = sd(PotentiallyMinAmmonium, na.rm = TRUE),       
    .groups = 'drop'
  )
print(PotentiallyMinAmmonium_stats)

SoilHealthData_sum_PotentiallyMinAmmonium_lm1 <- lmer(
  PotentiallyMinAmmonium ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_PotentiallyMinAmmonium_lm1)
anova(SoilHealthData_sum_PotentiallyMinAmmonium_lm1)

PotentiallyMinAmmonium_cld <- data.frame(
  cld(emmeans(SoilHealthData_sum_PotentiallyMinAmmonium_lm1, ~ SoilDepthRange), Letters = LETTERS)
) %>%
  dplyr::select(SoilDepthRange, .group)

emm_df <- left_join(PotentiallyMinAmmonium_stats, PotentiallyMinAmmonium_cld, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SD_RemNeg = pmax(emmean - SD, 0),
    upper.SD        = emmean + SD
  ) %>%
  arrange(Depth_mid_cm)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03

x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad

max_x <- max(emm_df$emmean, emm_df$upper.SD, na.rm = TRUE)
vals  <- seq(0, ceiling(max_x), by = 0.5)

PotentiallyMinAmmonium_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks   = vals,
    labels   = ifelse(seq_along(vals) %% 2 == 1, format(round(vals, 0), nsmall = 0), ""),
    position = "top",
    limits   = c(0, NA),
    expand   = expansion(mult = c(0, 0))  
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(
    x = expression("Potentially Mineralizable NH"[4]*"-N (mg/kg)"),
    y = "Soil Depth (cm)"
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(PotentiallyMinAmmonium_preplot)


# Potentially Min N (mineralizableNitrogen_AddZero) — using SD

mineralizableNitrogen_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(mineralizableNitrogen_AddZero, na.rm = TRUE), 
    SD = sd(mineralizableNitrogen_AddZero, na.rm = TRUE),       
    .groups = 'drop'
  )
print(mineralizableNitrogen_stats)

SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1 <- lmer(
  mineralizableNitrogen_AddZero ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1)
anova(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1)

mineralizableNitrogen_cld <- data.frame(
  cld(emmeans(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1, ~ SoilDepthRange), Letters = LETTERS)
) %>%
  dplyr::select(SoilDepthRange, .group)

emm_df <- left_join(mineralizableNitrogen_stats, mineralizableNitrogen_cld, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SD_RemNeg = pmax(emmean - SD, 0),  
    upper.SD        = emmean + SD
  ) %>%
  arrange(Depth_mid_cm)


pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03
x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad

max_x <- max(emm_df$emmean, emm_df$upper.SD, na.rm = TRUE)
vals  <- seq(0, ceiling(max_x), by = 0.5)

mineralizableNitrogen_AddZero_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks   = vals,
    labels   = ifelse(seq_along(vals) %% 2 == 1, format(round(vals, 0), nsmall = 0), ""),
    position = "top",
    limits   = c(0, NA),
    expand   = expansion(mult = c(0, 0))  
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(x = "Potentially Mineralizable N (mg/kg)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

print(mineralizableNitrogen_AddZero_preplot)

# totalN_percent — using SE
SoilHealthData_sum_totalN_percent_lm1 <- lmer(
  totalN_percent ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_totalN_percent_lm1)
anova(SoilHealthData_sum_totalN_percent_lm1)

SoilHealthData_sum_totalN_percent_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_totalN_percent_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

emm_df <- SoilHealthData_sum_totalN_percent_emm %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SE_RemNeg = pmax(emmean - SE, 0),
    upper.SE        = emmean + SE
  ) %>%
  arrange(Depth_mid_cm)

max_x  <- max(emm_df$emmean, emm_df$upper.SE, na.rm = TRUE)
digits <- if (max_x < 1) 2 else if (max_x < 10) 1 else 0
vals   <- pretty(c(0, max_x), n = 8)

pad <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.1
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SE, na.rm = TRUE) * 0.03
x_max_for_labels <- max(emm_df$upper.SE, na.rm = TRUE) + 4 * pad

totalN_percent_preplot <- ggplot(emm_df, aes(x = emmean, y = Depth_mid_cm)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.SE_RemNeg, xmax = upper.SE), height = 2) +
  geom_text(
    aes(x = upper.SE + pad, label = trimws(.group)),
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks   = vals,
    labels   = ifelse(seq_along(vals) %% 2 == 1,
                      sprintf(paste0("%.", digits, "f"), vals), ""),
    position = "top",
    limits   = c(0, NA),
    expand   = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, x_max_for_labels)) +
  labs(x = "Total N (%)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black")
  )

totalN_percent_preplot

# ACE_protein — using SD

SoilHealthData_sum_ACE_protein_lm1 <- lmer(
  ACE_protein ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_ACE_protein_lm1)
anova(SoilHealthData_sum_ACE_protein_lm1)

SoilHealthData_sum_ACE_protein_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_ACE_protein_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

cld_df$SoilDepthRange <- trimws(cld_df$SoilDepthRange)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(ACE_protein, na.rm = TRUE), 
    SD_raw = sd(ACE_protein, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    SD_raw = dplyr::if_else(is.na(SD_raw), 0, SD_raw),
    SoilDepthRange = trimws(SoilDepthRange) 
  )
print(raw_stats)


emm_df <- raw_stats %>%
  left_join(cld_df[, c("SoilDepthRange", ".group")], by = "SoilDepthRange") %>%
  dplyr::mutate(
    Depth_mid_cm = as.numeric(str_extract_all(SoilDepthRange, "\\d+") %>%
                                lapply(as.numeric) %>% sapply(mean)),
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  dplyr::arrange(Depth_mid_cm)

pad_val <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.1
if (!is.finite(pad_val) || pad_val == 0) pad_val <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03
x_right <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad_val

emm_df_lab <- emm_df %>% mutate(x_label = upper.SD + pad_val)

max_tick_limit <- 3
num_ticks <- 6
step_size <- max_tick_limit / (num_ticks - 1)
vals <- seq(0, max_tick_limit, by = step_size)

ACE_protein_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = sprintf("%.1f", vals),
    position = "top",
    limits = c(0, max_tick_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, max(max_tick_limit, x_right))) +
  labs(x = "ACE Protein (g/kg)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

ACE_protein_preplot


## olsenP_mg.kg — using SE

SoilHealthData_sum_olsenP_mg.kg_lm1 <- lmer(
  olsenP_mg.kg ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_olsenP_mg.kg_lm1)
anova(SoilHealthData_sum_olsenP_mg.kg_lm1)

SoilHealthData_sum_olsenP_mg.kg_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_olsenP_mg.kg_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean_raw = mean(olsenP_mg.kg, na.rm = TRUE),
    SD_raw = sd(olsenP_mg.kg, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    SD_raw = dplyr::if_else(is.na(SD_raw), 0, SD_raw)
  )
print(raw_stats)

emm_df <- SoilHealthData_sum_olsenP_mg.kg_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    emmean = emmean_raw,
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SD_RemNeg = pmax(emmean - SD_raw, 0),
    upper.SD = emmean + SD_raw
  ) %>%
  arrange(Depth_mid_cm)

pad_val <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.06
if (!is.finite(pad_val) || pad_val == 0) pad_val <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03
x_right <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad_val

emm_df_lab <- emm_df %>% mutate(x_label = upper.SD + pad_val)

max_tick_limit <- 18
step_size <- 4
vals <- seq(0, max_tick_limit, by = step_size)

olsenP_mg.kg_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks = vals,
    labels = sprintf("%.0f", vals),
    position = "top",
    limits = c(0, max_tick_limit),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, max(max_tick_limit, x_right))) +
  labs(x = "Olsen P (mg/kg)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color = "black")
  )

olsenP_mg.kg_preplot


## pH — using SD (Standard Deviation)

SoilHealthData_sum_pH_lm1 <- lmer(
  pH ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_pH_lm1)
anova(SoilHealthData_sum_pH_lm1)

SoilHealthData_sum_pH_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_pH_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean_raw = mean(pH, na.rm = TRUE),
    SD_raw = sd(pH, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
        SoilDepthRange = trimws(SoilDepthRange)
  )

print(raw_stats)
emm_df <- SoilHealthData_sum_pH_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  
  dplyr::mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw
  ) %>%
  dplyr::arrange(Depth_mid_cm) %>%
  dplyr::mutate(.group = c("B","A","A","A"))


x_min <- 7
x_max <- 9
headroom <- 0.1
x_limit_max <- x_max + headroom
span <- x_max - x_min

emm_df <- emm_df %>%
  mutate(lower.SD_Clamped = pmax(lower.SD, x_min))

letter_pad <- 0.05 * span
emm_df_lab <- emm_df %>%
  mutate(x_label = pmin(upper.SD + letter_pad, x_limit_max - 0.02 * span))

ticks <- seq(x_min, x_max, by = 0.5)

pH_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) + 
  geom_errorbarh(aes(xmin = lower.SD_Clamped, xmax = upper.SD), height = 2) + 
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE, size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    limits = c(x_min, x_limit_max),
    breaks = ticks,
    labels = scales::number_format(accuracy = 0.1),
    position = "top",
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "pH", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold")
  )

pH_preplot


## pH Hcon — using SD

SoilHealthData_sum_pH_Hconc_lm1 <- lmer(
  pH_Hconc ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)

residplot(SoilHealthData_sum_pH_Hconc_lm1)
anova(SoilHealthData_sum_pH_Hconc_lm1)

SoilHealthData_sum_pH_Hconc_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_pH_Hconc_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

pH_Hconc_sd_df <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  group_by(SoilDepthRange) %>%
  summarise(
    SD = sd(pH_Hconc, na.rm = TRUE),
    .groups = "drop"
  )

emm_df <- SoilHealthData_sum_pH_Hconc_emm %>%
  left_join(pH_Hconc_sd_df, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    lower.SD_RemNeg = pmax(emmean - SD, 0),
    upper.SD        = emmean + SD
  ) %>%
  arrange(Depth_mid_cm)

x_min   <- 0
pad_val <- diff(range(emm_df$emmean, na.rm = TRUE)) * 0.03

if (!is.finite(pad_val) || pad_val == 0) {
  pad_val <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03
}

x_right <- max(emm_df$upper.SD, na.rm = TRUE) + 10 * pad_val  

emm_df_lab <- emm_df %>%
  mutate(
    x_label_raw = upper.SD + pad_val,
    x_label     = pmin(x_label_raw, x_right - 0.8 * pad_val)
  )

vals    <- pretty(c(x_min, x_right), n = 6)
sci_fmt <- function(x) format(x, scientific = TRUE, digits = 2)

pH_Hconc_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    limits   = c(x_min, x_right),                  
    breaks   = vals,
    labels   = ifelse(seq_along(vals) %% 2 == 1, sci_fmt(vals), ""),
    position = "top",
    expand   = expansion(mult = c(0, 0))
  ) +
  labs(
    x = expression(bold("H"^"+" * " ion concentration")),
    y = "Soil Depth (cm)"
  ) +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.margin = margin(t = 5, r = 25, b = 5, l = 5),
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black")
  )

pH_Hconc_preplot

## CEC with  standard deviation 

SoilHealthData_sum_CEC_lm1 <- lmer(
  CEC ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_CEC_lm1)
anova(SoilHealthData_sum_CEC_lm1)

SoilHealthData_sum_CEC_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_CEC_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean_raw = mean(CEC, na.rm = TRUE),
    SD_raw = sd(CEC, na.rm = TRUE) 
  ) 
print(raw_stats)
emm_df <- SoilHealthData_sum_CEC_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw
  ) %>%
  arrange(Depth_mid_cm)

x_min <- 8
x_max <- 13
span  <- x_max - x_min

emm_df <- emm_df %>%
  mutate(
    lower.SD_Clamped = pmax(lower.SD, x_min),
    upper.SD_Clamped = pmin(upper.SD, x_max)
  )

letter_pad <- 0.05 * span
emm_df_lab <- emm_df %>%
  mutate(x_label = pmin(upper.SD_Clamped + letter_pad, x_max - 0.02 * span))

ticks <- seq(x_min, x_max, by = 0.5)

CEC_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_Clamped, xmax = upper.SD_Clamped), height = 2) +
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    limits  = c(x_min, x_max),
    breaks  = ticks,
    labels  = ifelse(seq_along(ticks) %% 2 == 1, sprintf("%.0f", ticks), ""),
    position = "top",
    expand  = expansion(mult = c(0, 0))
  ) +
  labs(x = expression(bold("CEC (cmol"[c]*"/kg)")), y = "Soil Depth (cm)") +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black")
  )

CEC_preplot

## EC with standard deviation
SoilHealthData_sum_EC_lm1 <- lmer(
  EC ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_EC_lm1)
anova(SoilHealthData_sum_EC_lm1)

SoilHealthData_sum_EC_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_EC_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(EC, na.rm = TRUE), 
    SD_raw = sd(EC, na.rm = TRUE) 
  ) 
print(raw_stats)
emm_df <- SoilHealthData_sum_EC_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)

pad_val <- diff(range(emm_df$emmean.x, na.rm = TRUE)) * 0.1 
if (!is.finite(pad_val) || pad_val == 0) pad_val <- max(emm_df$upper.SD, na.rm = TRUE) * 1

max_axis_limit <- 1.0
x_right <- max(emm_df$upper.SD, na.rm = TRUE) + 10 * pad_val 

emm_df_lab <- emm_df %>% mutate(x_label = upper.SD + pad_val)

vals <- seq(0, max_axis_limit, by = 0.2)

EC_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean.x)) +
  geom_path(aes(group = 1), linewidth = 1) +
  
  geom_point(aes(x = emmean.x, y = Depth_mid_cm), size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    data = emm_df_lab,
    aes(x = x_label, y = Depth_mid_cm, label = trimws(.group)),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", hjust = 0
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks  = vals,
    labels  = sprintf("%.1f", vals),
    position = "top",
    limits  = c(0, max_axis_limit),
    expand  = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, max(max_axis_limit, x_right))) +
  labs(x = "EC (dS/m)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black")
  )

EC_preplot

### bulk density with standard deviation

SoilHealthData_sum_bd_g.cm3_soil_lm1 <- lmer(
  bd_g.cm3_soil ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_bd_g.cm3_soil_lm1)
anova(SoilHealthData_sum_bd_g.cm3_soil_lm1)

SoilHealthData_sum_bd_g.cm3_soil_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_bd_g.cm3_soil_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilHealthData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(bd_g.cm3_soil, na.rm = TRUE), 
    SD_raw = sd(bd_g.cm3_soil, na.rm = TRUE) 
  ) 
print(raw_stats)

emm_df <- SoilHealthData_sum_bd_g.cm3_soil_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)

pad <- diff(range(emm_df$emmean.x, na.rm = TRUE)) * 0.1 
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03 

new_x_max <- 1.8
new_y_max <- 90
vals_y_all <- seq(0, new_y_max, 5)

vals_x_all <- seq(1, new_x_max, by = 0.2)


bd_g.cm3_soil_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean.x)) + 
  geom_path(aes(group = 1), linewidth = 1) +
  
  geom_point(aes(x = emmean.x, y = Depth_mid_cm), size = 2) +
  
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)), 
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = vals_y_all,
    labels = function(x) ifelse(x %% 10 == 0, as.character(x), ""),
    limits = c(new_y_max, 0)
  ) +
  scale_x_continuous(
    breaks  = vals_x_all,
    labels = sprintf("%.1f", vals_x_all),
    position = "top",
    limits  = c(0, new_x_max),
    expand  = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(1, new_x_max)) +
  labs(x = bquote(bold("Bulk Density (g/cm"^3*")")), 
       y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black"),
    axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.length.x = unit(0.2, "cm")
  )

print(bd_g.cm3_soil_preplot)

## PLFA nmol with standard deviation 

SoilHealthData_sum_TotalPLFA.nmol_lm1 <- lmer(
  TotalPLFA.nmol ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilPLFAData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_TotalPLFA.nmol_lm1)
anova(SoilHealthData_sum_TotalPLFA.nmol_lm1)

SoilHealthData_sum_TotalPLFA.nmol_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_TotalPLFA.nmol_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilPLFAData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA.nmol, na.rm = TRUE), 
    SD_raw = sd(TotalPLFA.nmol, na.rm = TRUE) 
  ) 
print(raw_stats)

emm_df <- SoilHealthData_sum_TotalPLFA.nmol_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)

pad <- diff(range(emm_df$emmean.x, na.rm = TRUE)) * 0.1 
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03 

new_x_max <- 70
new_y_max <- 90
vals_y_all <- seq(0, new_y_max, 5)

vals_x_major <- seq(0, new_x_max, by = 10)

vals_x_minor <- seq(0, new_x_max, by = 5)
vals_x_minor <- vals_x_minor[!vals_x_minor %in% vals_x_major]


TotalPLFA.nmol_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean.x)) + 
  geom_path(aes(group = 1), linewidth = 1) +
  
  geom_point(aes(x = emmean.x, y = Depth_mid_cm), size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)), 
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = vals_y_all,
    labels = function(x) ifelse(x %% 10 == 0, as.character(x), ""),
    limits = c(new_y_max, 0)
  ) +
  scale_x_continuous(
    breaks = vals_x_major,          
    minor_breaks = vals_x_minor,    
    labels = sprintf("%.0f", vals_x_major),
    position = "top",
    limits  = c(0, new_x_max),
    expand  = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, new_x_max)) +
  labs(x = "PLFA Mass (nmols/g)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black"),
    axis.ticks.x = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.length.x = unit(0.15, "cm")
  )

print(TotalPLFA.nmol_preplot)

# TotalPLFA_count — using SD 

SoilHealthData_sum_TotalPLFA_count_lm1 <- lmer(
  TotalPLFA_count ~ SoilDepthRange + (1 | fact_replicateNo),
  data = SoilPLFAData_Clean_FieldBaselineDat_sum
)
residplot(SoilHealthData_sum_TotalPLFA_count_lm1)
anova(SoilHealthData_sum_TotalPLFA_count_lm1)

SoilHealthData_sum_TotalPLFA_count_emm <- data.frame(
  cld(emmeans(SoilHealthData_sum_TotalPLFA_count_lm1, ~ SoilDepthRange), Letters = LETTERS)
)

raw_stats <- SoilPLFAData_Clean_FieldBaselineDat_sum %>%
  dplyr::group_by(SoilDepthRange) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA_count, na.rm = TRUE), 
    SD_raw = sd(TotalPLFA_count, na.rm = TRUE) 
  ) 
print(raw_stats)
emm_df <- SoilHealthData_sum_TotalPLFA_count_emm %>%
  left_join(raw_stats, by = "SoilDepthRange") %>%
  mutate(
    Depth_mid_cm    = str_extract_all(SoilDepthRange, "\\d+") %>% lapply(as.numeric) %>% sapply(mean),
    
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  ) %>%
  arrange(Depth_mid_cm)

fixed_x_max <- 50 
vals <- seq(0, fixed_x_max, by = 10) 

pad <- diff(range(emm_df$emmean.x, na.rm = TRUE)) * 0.1
if (!is.finite(pad) || pad == 0) pad <- max(emm_df$upper.SD, na.rm = TRUE) * 0.03

x_max_for_labels <- max(emm_df$upper.SD, na.rm = TRUE) + 4 * pad 

TotalPLFA_count_preplot <- ggplot(emm_df, aes(y = Depth_mid_cm, x = emmean.x)) +
  geom_path(aes(group = 1), linewidth = 1) +
  geom_point(aes(x = emmean.x, y = Depth_mid_cm), size = 2) +
  geom_errorbarh(aes(xmin = lower.SD_RemNeg, xmax = upper.SD), height = 2) +
  geom_text(
    aes(x = upper.SD + pad, label = trimws(.group)), 
    size = 5, fontface = "bold", hjust = 0, show.legend = FALSE
  ) +
  scale_y_reverse(
    breaks = seq(0, 90, 5),
    labels = function(x) ifelse(x %% 15 == 0, as.character(x), ""),
    limits = c(90, 0)
  ) +
  scale_x_continuous(
    breaks  = vals,
    labels  = sprintf("%.0f", vals), 
    position = "top",
    limits  = c(0, fixed_x_max),
    expand  = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, max(fixed_x_max, x_max_for_labels))) + 
  labs(x = "PLFA Count (#)", y = "Soil Depth (cm)") +
  theme_classic(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text.y     = element_text(face = "bold", color = "black"),
    axis.text.x.top = element_text(face = "bold", color = "black"),
    axis.title      = element_text(face = "bold", color = "black")
  )

TotalPLFA_count_preplot


##  Figure 3 Plot

combined_preplot <- ggpubr::ggarrange(
  molMnred.kg_preplot, minC_preplot, 
  ACE_protein_preplot, 
  olsenP_mg.kg_preplot,
  pH_preplot, EC_preplot, CEC_preplot,
  bd_g.cm3_soil_preplot, 
  TotalPLFA.nmol_preplot, TotalPLFA_count_preplot,
  ncol = 4, nrow = 3,
  labels = "AUTO",
  align = "hv"
)

final_preplot <- annotate_figure(
 combined_preplot, 
  left = ggpubr::text_grob("Soil Depth (cm)", rot = 90, size = 18),
  
)

print(final_preplot)

