## LTARE Soil Health Vineyard ##
## Characterization paper code ##
# undervine #


#### loading packages ####
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
install.packages("showtext")
library(showtext)

font_add_google("Lato", "lato")
font_add_google("Poppins", "poppins")
showtext_auto()


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

#summary(SoilHealthData_Clean)


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


SoilHealthData_Clean_AnalysisDat<- SoilHealthData_Clean %>%
  dplyr::select(GrowthCycle, fieldID, variety, sampleLocation, 
                blockNo, fact_replicateNo, 
                fact_TreatmentNo, SoilDepthRange,
                totalC_percent, inorganicC_percent, TOC_percent, OM_percent, poxC_mg.kg_AddZero, molMnred.kg,
                minC,
                PotentiallyMinNitrate, PotentiallyMinAmmonium, mineralizableNitrogen_AddZero,
                olsenP_mg.kg,
                pH, pH_Hconc,
                ACE_protein,
                CEC,
                EC,
                bd_g.cm3_soil, bd_g.cm3_rock) 
summary(SoilHealthData_Clean_AnalysisDat) 

SoilPLFAData_Clean_AnalysisDat<- SoilPLFAData_Clean %>%
  dplyr::select(GrowthCycle, fieldID, variety, sampleLocation, 
                blockNo, fact_replicateNo, 
                fact_TreatmentNo, SoilDepthRange,
                TotalPLFA.nmol, TotalPLFA_mass, TotalPLFA_count) 

head(SoilPLFAData_Clean_AnalysisDat)
unique(SoilPLFAData_Clean_AnalysisDat$GrowthCycle)


#### UNDERVINE Data frames for "Fall 2023" - Soil and PLFA #### 
SoilHealthData_Clean_FieldPostPlantDat<- SoilHealthData_Clean_AnalysisDat %>% 
  filter(GrowthCycle == "Fall_2023") %>%
  filter(sampleLocation == "Undervine") %>%
  mutate(SoilDepthRange = recode(SoilDepthRange, 
                                 "0_15"="0-15",
                                 "15_30"="15-30",
                                 "30_60"="30-60",
                                 "60_90"="60-90"),
         SoilDepthRange = factor(SoilDepthRange, levels = c("60-90", "30-60", "15-30", "0-15")))
head(SoilHealthData_Clean_FieldPostPlantDat)
View(SoilHealthData_Clean_FieldPostPlantDat)

SoilPLFAData_Clean_FieldPostPlantDat<- SoilPLFAData_Clean_AnalysisDat %>% 
  filter(GrowthCycle == "Fall_2023") %>%
  filter(sampleLocation == "Undervine") %>%
  mutate(SoilDepthRange = recode(SoilDepthRange, 
                                 "0_15"="0-15",
                                 "15_30"="15-30",
                                 "30_60"="30-60",
                                 "60_90"="60-90"),
         SoilDepthRange = factor(SoilDepthRange, levels = c("60-90", "30-60", "15-30", "0-15")))



##### Summary of post plant data undervine #####
SoilHealthData_Clean_FieldPostPlantDat_sum<- SoilHealthData_Clean_FieldPostPlantDat %>%
  dplyr::select(fact_replicateNo,SoilDepthRange, variety,
                totalC_percent,inorganicC_percent,TOC_percent,
                poxC_mg.kg_AddZero, molMnred.kg,
                minC,
                PotentiallyMinNitrate, PotentiallyMinAmmonium, mineralizableNitrogen_AddZero,
                olsenP_mg.kg, olsenP_mg.kg,
                pH, pH_Hconc,
                ACE_protein,
                CEC,
                EC,
                bd_g.cm3_soil, bd_g.cm3_rock) %>%
  group_by(fact_replicateNo,SoilDepthRange, variety) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 

View(SoilHealthData_Clean_FieldPostPlantDat_sum)
head(SoilHealthData_Clean_FieldPostPlantDat_sum)

SoilPLFAData_Clean_FieldPostPlantDat_sum<- SoilPLFAData_Clean_FieldPostPlantDat %>%
  dplyr::select(fact_replicateNo,SoilDepthRange, variety,
                TotalPLFA.nmol, TotalPLFA_mass, TotalPLFA_count) %>%
  group_by(fact_replicateNo,SoilDepthRange, variety) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

View(SoilPLFAData_Clean_FieldPostPlantDat_sum)

### Raw stats for summary data ###
raw_stats_long <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(SoilDepthRange, variety, parameter) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_long)

raw_stats_table <- raw_stats_long %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange, variety)

print(raw_stats_table)
raw_stats_table <- raw_stats_long %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange, variety)

print(raw_stats_table)
library(writexl)
write_xlsx(raw_stats_table,
           "SoilHealth_RawStats_PostPlant.xlsx")

### Raw stats for PLFA under-vine summary data ###
raw_stats_longPLFA <- SoilPLFAData_Clean_FieldPostPlantDat_sum %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(SoilDepthRange, variety, parameter) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_longPLFA)

raw_stats_tablePLFA <- raw_stats_longPLFA %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange, variety)

print(raw_stats_tablePLFA)

raw_stats_tablePLFA <- raw_stats_longPLFA %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange, variety)

print(raw_stats_tablePLFA)
library(writexl)
write_xlsx(raw_stats_tablePLFA,
           "SoilHealth_RawStats_PostPlant_PLFA.xlsx")


## TC with variety using SD

SoilHealthData_sum_totalC_percent_lm1<- lmer(totalC_percent ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_totalC_percent_lm1)
anova(SoilHealthData_sum_totalC_percent_lm1)

SoilHealthData_sum_totalC_percent_emm<- data.frame(cld(emmeans(SoilHealthData_sum_totalC_percent_lm1, ~ SoilDepthRange * variety),
                                                       Letters = LETTERS))
View(SoilHealthData_sum_totalC_percent_emm)

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    n = sum(!is.na(totalC_percent)),    
    emmean = mean(totalC_percent, na.rm = TRUE),
    SD_raw = sd(totalC_percent, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats)

SoilHealthData_sum_totalC_percent_emm<- SoilHealthData_sum_totalC_percent_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_totalC_percent_plot <- ggplot(
  SoilHealthData_sum_totalC_percent_emm,
  aes(x = SoilDepthRange,
      y = emmean.x,
      fill = variety)
) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05, 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "TC (%)") +
  
  scale_y_continuous(position = "right") +
  
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(
    name = "Variety",
    values = c("Cabernet Sauvignon" = "#A60F2D",
               "Chardonnay" = "#CCC29C")
  )

print(SoilHealthData_sum_totalC_percent_plot)

# inorganicC_percent with variety as a factor
SoilHealthData_sum_inorganicC_percent_lm1<- lmer(inorganicC_percent ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_inorganicC_percent_lm1)
anova(SoilHealthData_sum_inorganicC_percent_lm1)

SoilHealthData_sum_inorganicC_percent_emm<- data.frame(cld(emmeans(SoilHealthData_sum_inorganicC_percent_lm1, ~ SoilDepthRange * variety),
                                                       Letters = LETTERS))

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(inorganicC_percent, na.rm = TRUE),
    SD_raw = sd(inorganicC_percent, na.rm = TRUE)
  )
print(raw_stats)
SoilHealthData_sum_inorganicC_percent_emm<- SoilHealthData_sum_inorganicC_percent_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_inorganicC_percent_plot <- ggplot(
  SoilHealthData_sum_inorganicC_percent_emm,
  aes(x = SoilDepthRange,
      y = emmean.x,
      fill = variety)
) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05, 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "TIC (%)") +
  
  scale_y_continuous(position = "right") +
  
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(
    name = "Variety",
    values = c("Cabernet Sauvignon" = "#A60F2D",
               "Chardonnay" = "#CCC29C")
  )

print(SoilHealthData_sum_inorganicC_percent_plot)


# TOC_percent with vareity as a factor
SoilHealthData_sum_TOC_percent_lm1<- lmer(TOC_percent ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_TOC_percent_lm1)
anova(SoilHealthData_sum_TOC_percent_lm1)

SoilHealthData_sum_TOC_percent_emm<- data.frame(cld(emmeans(SoilHealthData_sum_TOC_percent_lm1, ~ SoilDepthRange * variety),
                                                    Letters = LETTERS))

#print(emmeans(SoilHealthData_sum_TOC_percent_lm1, ~ SoilDepthRange * variety, adjustment='tukey')

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(TOC_percent, na.rm = TRUE),
    SD_raw = sd(TOC_percent, na.rm = TRUE)
  )
print(raw_stats)

SoilHealthData_sum_TOC_percent_emm<- SoilHealthData_sum_TOC_percent_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_TOC_percent_plot <- ggplot(SoilHealthData_sum_TOC_percent_emm,
                                              aes(x = SoilDepthRange,
                                                  y = emmean.x,
                                                  fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.08, 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "TOC (%)") +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_TOC_percent_plot

# #poxC_mg.kg_AddZero with variety 
SoilHealthData_sum_poxC_mg.kg_AddZero_lm1<- lmer(poxC_mg.kg_AddZero ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1)
anova(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1)

SoilHealthData_sum_poxC_mg.kg_AddZero_emm<- data.frame(cld(emmeans(SoilHealthData_sum_poxC_mg.kg_AddZero_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(poxC_mg.kg_AddZero, na.rm = TRUE),
    SD_raw = sd(poxC_mg.kg_AddZero, na.rm = TRUE)
  )
print(raw_stats)

SoilHealthData_sum_poxC_mg.kg_AddZero_emm<- SoilHealthData_sum_poxC_mg.kg_AddZero_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_poxC_mg.kg_AddZero_plot <- ggplot(SoilHealthData_sum_poxC_mg.kg_AddZero_emm,
                                                     aes(x = SoilDepthRange,
                                                         y = emmean.x,
                                                         fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 20, 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "POXC (mg/kg)") +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_poxC_mg.kg_AddZero_plot


# #molMnred.kg with variety 
SoilHealthData_sum_molMnred.kg_lm1<- lmer(molMnred.kg ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_molMnred.kg_lm1)
anova(SoilHealthData_sum_molMnred.kg_lm1)

SoilHealthData_sum_molMnred.kg_emm<- data.frame(cld(emmeans(SoilHealthData_sum_molMnred.kg_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(molMnred.kg, na.rm = TRUE),
    SD_raw = sd(molMnred.kg, na.rm = TRUE)
  )
print(raw_stats)

SoilHealthData_sum_molMnred.kg_emm<- SoilHealthData_sum_molMnred.kg_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_molMnred.kg_plot <- ggplot(SoilHealthData_sum_molMnred.kg_emm,
                                                     aes(x = SoilDepthRange,
                                                         y = emmean.x,
                                                         fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + .005, 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = bquote(bold("POXC (mol MnO"[4]^"-"*"/kg)"))) +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_molMnred.kg_plot


# #minC with variety 
SoilHealthData_sum_minC_lm1<- lmer(minC ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_minC_lm1)
anova(SoilHealthData_sum_minC_lm1)

SoilHealthData_sum_minC_emm<- data.frame(cld(emmeans(SoilHealthData_sum_minC_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(minC, na.rm = TRUE),
    SD_raw = sd(minC, na.rm = TRUE)
  )
print(raw_stats)

SoilHealthData_sum_minC_emm<- SoilHealthData_sum_minC_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_minC_plot <- ggplot(SoilHealthData_sum_minC_emm,
                                       aes(x = SoilDepthRange,
                                           y = emmean.x,
                                           fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 5,
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "MinC (mg/kg/day)") +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_minC_plot

# mineralizableNitrogen_AddZero with variety

SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1<- lmer(mineralizableNitrogen_AddZero ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1)
anova(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1)

SoilHealthData_sum_mineralizableNitrogen_AddZero_emm<- data.frame(cld(emmeans(SoilHealthData_sum_mineralizableNitrogen_AddZero_lm1, ~ SoilDepthRange * variety), Letters = LETTERS)) %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

SoilHealthData_sum_mineralizableNitrogen_AddZero_plot <- ggplot(SoilHealthData_sum_mineralizableNitrogen_AddZero_emm, 
                                                                aes(x = SoilDepthRange, 
                                                                    y = emmean, 
                                                                    fill = variety)) + 
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") + 
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SE+(mean(SoilHealthData_sum_mineralizableNitrogen_AddZero_emm$upper.SE)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = expression("Potentially Mineralizable N (mg/kg)"),
       title = "Under vine Post-Plant") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"), 
    axis.text = element_text(size = 12, family = "timesnewroman"), 
    legend.text = element_text(size = 12, family = "timesnewroman"), 
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) + 
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C")) 
SoilHealthData_sum_mineralizableNitrogen_AddZero_plot

ggsave(
  filename = "UV_SoilHealthData_sum_mineralizableNitrogen_AddZero_plot.pdf",
  plot = SoilHealthData_sum_mineralizableNitrogen_AddZero_plot,
  width = 8, height = 6, units = "in")


# PotentiallyMinNitrate with variety 

SoilHealthData_sum_PotentiallyMinNitrate_lm1<- lmer(PotentiallyMinNitrate ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_PotentiallyMinNitrate_lm1)
anova(SoilHealthData_sum_PotentiallyMinNitrate_lm1)

SoilHealthData_sum_PotentiallyMinNitrate_emm<- data.frame(cld(emmeans(SoilHealthData_sum_PotentiallyMinNitrate_lm1, ~ SoilDepthRange * variety), Letters = LETTERS)) %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

SoilHealthData_sum_PotentiallyMinNitrate_plot <- ggplot(SoilHealthData_sum_PotentiallyMinNitrate_emm, 
                                                        aes(x = SoilDepthRange, 
                                                            y = emmean, 
                                                            fill = variety)) + 
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") + 
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SE+(mean(SoilHealthData_sum_PotentiallyMinNitrate_emm$upper.SE)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = expression("Potentially Mineralizable NO"[3]*"-N (mg/kg)")) +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"), 
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"), 
    legend.text = element_text(size = 12, family = "timesnewroman"), 
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) + 
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C")) 
SoilHealthData_sum_PotentiallyMinNitrate_plot



# PotentiallyMinAmmonium with variety 
SoilHealthData_sum_PotentiallyMinAmmonium_lm1<- lmer(PotentiallyMinAmmonium ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_PotentiallyMinAmmonium_lm1)
anova(SoilHealthData_sum_PotentiallyMinAmmonium_lm1)

SoilHealthData_sum_PotentiallyMinAmmonium_emm<- data.frame(cld(emmeans(SoilHealthData_sum_PotentiallyMinAmmonium_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))%>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

SoilHealthData_sum_PotentiallyMinAmmonium_plot <- ggplot(SoilHealthData_sum_PotentiallyMinAmmonium_emm, 
                                                         aes(x = SoilDepthRange, 
                                                             y = emmean, 
                                                             fill = variety)) + 
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") + 
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(SoilHealthData_sum_PotentiallyMinAmmonium_emm$emmean)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = expression("Potentially Mineralizable NH"[4]*"-N (mg/kg)")) + 
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"), 
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"), 
    legend.text = element_text(size = 12, family = "timesnewroman"), 
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) + 
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C")) 
SoilHealthData_sum_PotentiallyMinAmmonium_plot



# #olsenP_mg.kg with variety 
SoilHealthData_sum_olsenP_mg.kg_lm1<- lmer(olsenP_mg.kg ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_olsenP_mg.kg_lm1)
anova(SoilHealthData_sum_olsenP_mg.kg_lm1)

SoilHealthData_sum_olsenP_mg.kg_emm<- data.frame(cld(emmeans(SoilHealthData_sum_olsenP_mg.kg_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_stats <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(olsenP_mg.kg, na.rm = TRUE),
    SD_raw = sd(olsenP_mg.kg, na.rm = TRUE)
  )
print(raw_stats)

SoilHealthData_sum_olsenP_mg.kg_emm<- SoilHealthData_sum_olsenP_mg.kg_emm %>%
  left_join(raw_stats, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean.x - SD_raw,
    upper.SD = emmean.x + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_olsenP_mg.kg_plot <- ggplot(SoilHealthData_sum_olsenP_mg.kg_emm,
                                               aes(x = SoilDepthRange,
                                                   y = emmean.x,
                                                   fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.7,
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "Available P (mg/kg)") +
  scale_y_continuous(limits = c(0, 15),
                     breaks = seq(0, 15, 3),
                     position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety", values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))


SoilHealthData_sum_olsenP_mg.kg_plot


### pH 
SoilHealthData_sum_pH_lm1 <- lmer(
  pH ~ SoilDepthRange * variety + (1 | fact_replicateNo),
  data = SoilHealthData_Clean_FieldPostPlantDat_sum
)
residplot(SoilHealthData_sum_pH_lm1)
anova(SoilHealthData_sum_pH_lm1)

SoilHealthData_sum_pH_emm <- data.frame(
  cld(
    emmeans(SoilHealthData_sum_pH_lm1, ~ SoilDepthRange * variety),
    Letters = LETTERS
  )
)

raw_sd_data <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    mean_raw = mean(pH, na.rm = TRUE),
    SD_raw   = sd(pH, na.rm = TRUE),
    .groups  = "drop"
  )
print(raw_sd_data)
SoilHealthData_sum_pH_emm <- SoilHealthData_sum_pH_emm %>%
  dplyr::left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  dplyr::mutate(
    lower.SD        = emmean - SD_raw,
    upper.SD        = emmean + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )


combos <- SoilHealthData_sum_pH_emm %>%
  dplyr::select(SoilDepthRange, variety) %>%
  dplyr::distinct() %>%
  dplyr::arrange(SoilDepthRange, variety)

label_lookup <- combos %>%
  dplyr::mutate(
    label_cld = c("A", "B", "A", "AB", "AB", "AB", "B", "B")
  )

SoilHealthData_sum_pH_emm <- SoilHealthData_sum_pH_emm %>%
  dplyr::left_join(label_lookup, by = c("SoilDepthRange", "variety"))

new_min  <- 5
new_max  <- 9
vals_all <- seq(new_min, new_max, by = 1)

SoilHealthData_sum_pH_plot <- ggplot(
  SoilHealthData_sum_pH_emm,
  aes(
    x    = SoilDepthRange,
    y    = emmean,
    fill = variety
  )
) +
  geom_bar(
    stat = "identity",
    color = "black",
    position = position_dodge(width = 0.9)
  ) +
  
  geom_errorbar(
    aes(ymin = lower.SD_RemNeg, ymax = upper.SD),
    width = 0.3,
    position = position_dodge(width = 0.9)
  ) +
  
  geom_text(
    aes(
      y     = pmin(upper.SD + 0.35, new_max - 0.05),  
            label = trimws(label_cld)
    ),
    size = 4,
    position = position_dodge(width = .9),
    fontface = "bold"
  ) +
  
  labs(
    x = "Soil Depth (cm)",
    y = "pH"
  ) +
  
  scale_y_continuous(
    breaks   = vals_all,
    labels   = sprintf("%.1f", vals_all),
    position = "right"   
  ) +
  
  coord_flip(ylim = c(new_min, new_max)) +
  
  theme_bw() +
  theme(
    plot.title    = element_text(size = 12, family = "timesnewroman",
                                 face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title    = element_text(size = 12, family = "timesnewroman"),
    axis.title.y  = element_blank(),
    axis.text     = element_text(size = 12, family = "timesnewroman"),
    legend.text   = element_text(size = 12, family = "timesnewroman"),
    legend.title  = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(
    values = c(
      "Cabernet Sauvignon" = "#A60F2D",
      "Chardonnay"         = "#CCC29C"
    )
  )

SoilHealthData_sum_pH_plot



# pH_Hconc with variety 
SoilHealthData_sum_pH_Hconc_lm1<- lmer(pH_Hconc ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_pH_Hconc_lm1)
anova(SoilHealthData_sum_pH_Hconc_lm1)

SoilHealthData_sum_pH_Hconc_emm<- data.frame(cld(emmeans(SoilHealthData_sum_pH_Hconc_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))%>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

SoilHealthData_sum_pH_Hconc_plot <- ggplot(SoilHealthData_sum_pH_Hconc_emm, 
                                           aes(x = SoilDepthRange, 
                                               y = emmean, 
                                               fill = variety)) + 
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") + 
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(SoilHealthData_sum_pH_Hconc_emm$emmean)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = expression("H"^"+"*"ion concentration")) +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"), 
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"), 
    legend.text = element_text(size = 12, family = "timesnewroman"), 
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) + 
  scale_fill_manual(name = "Variety",
                    values = c("#A60F2D", "#CCC29C")) 
SoilHealthData_sum_pH_Hconc_plot



# # bd_g.cm3_soil with variety 
SoilHealthData_sum_bd_g.cm3_soil_lm1<- lmer(bd_g.cm3_soil ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_bd_g.cm3_soil_lm1)
anova(SoilHealthData_sum_bd_g.cm3_soil_lm1)

SoilHealthData_sum_bd_g.cm3_soil_emm<- data.frame(cld(emmeans(SoilHealthData_sum_bd_g.cm3_soil_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(bd_g.cm3_soil, na.rm = TRUE),
    SD_raw = sd(bd_g.cm3_soil, na.rm = TRUE)
  )
print(raw_sd_data)

SoilHealthData_sum_bd_g.cm3_soil_emm<- SoilHealthData_sum_bd_g.cm3_soil_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_bd_g.cm3_soil_plot <- ggplot(SoilHealthData_sum_bd_g.cm3_soil_emm,
                                                aes(x = SoilDepthRange,
                                                    y = emmean,
                                                    fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.12), 
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = bquote(bold("Bulk Density (g/cm"^3*")"))) +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman", face = "bold"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_bd_g.cm3_soil_plot


# CEC with variety 
SoilHealthData_sum_CEC_lm1<- lmer(CEC ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_CEC_lm1)
anova(SoilHealthData_sum_CEC_lm1)

SoilHealthData_sum_CEC_emm<- data.frame(cld(emmeans(SoilHealthData_sum_CEC_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(CEC, na.rm = TRUE),
    SD_raw = sd(CEC, na.rm = TRUE)
  )
print(raw_sd_data)
SoilHealthData_sum_CEC_emm<- SoilHealthData_sum_CEC_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_CEC_plot <- ggplot(SoilHealthData_sum_CEC_emm,
                                      aes(x = SoilDepthRange,
                                          y = emmean,
                                          fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.15),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = expression(bold("CEC (cmol"[c]*"/kg)"))) +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_CEC_plot


# # EC with variety 
SoilHealthData_sum_EC_lm1<- lmer(EC ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_EC_lm1)
anova(SoilHealthData_sum_EC_lm1)

SoilHealthData_sum_EC_emm<- data.frame(cld(emmeans(SoilHealthData_sum_EC_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(EC, na.rm = TRUE),
    SD_raw = sd(EC, na.rm = TRUE)
  )
print(raw_sd_data)

SoilHealthData_sum_EC_emm<- SoilHealthData_sum_EC_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

new_min <- 0.0
new_max <- 1.5 
vals_all <- seq(new_min, new_max, by = 0.25) 

SoilHealthData_sum_EC_plot <- ggplot(SoilHealthData_sum_EC_emm,
                                     aes(x = SoilDepthRange,
                                         y = emmean,
                                         fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.05),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "EC (dS/m)") +
  scale_y_continuous(
    breaks = vals_all,
    labels = sprintf("%.1f", vals_all), 
    limits = c(new_min, new_max),
    position = "right"
  ) +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_EC_plot


# # ACE Protein with variety 
SoilHealthData_sum_ACE_protein_lm1<- lmer(ACE_protein ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilHealthData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_ACE_protein_lm1)
anova(SoilHealthData_sum_ACE_protein_lm1)

SoilHealthData_sum_ACE_protein_emm<- data.frame(cld(emmeans(SoilHealthData_sum_ACE_protein_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilHealthData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(ACE_protein, na.rm = TRUE),
    SD_raw = sd(ACE_protein, na.rm = TRUE)
  )
print(raw_sd_data)

SoilHealthData_sum_ACE_protein_emm<- SoilHealthData_sum_ACE_protein_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_ACE_protein_plot <- ggplot(SoilHealthData_sum_ACE_protein_emm,
                                              aes(x = SoilDepthRange,
                                                  y = emmean,
                                                  fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.15),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "ACE Protein (g/kg)") +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_ACE_protein_plot

# # PLFA TotalPLFA.nmols variety 
SoilHealthData_sum_TotalPLFA.nmol_lm1<- lmer(TotalPLFA.nmol ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilPLFAData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_TotalPLFA.nmol_lm1)
anova(SoilHealthData_sum_TotalPLFA.nmol_lm1)

SoilHealthData_sum_TotalPLFA.nmol_emm<- data.frame(cld(emmeans(SoilHealthData_sum_TotalPLFA.nmol_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilPLFAData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA.nmol, na.rm = TRUE),
    SD_raw = sd(TotalPLFA.nmol, na.rm = TRUE)
  )
print(raw_sd_data)

SoilHealthData_sum_TotalPLFA.nmol_emm<- SoilHealthData_sum_TotalPLFA.nmol_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

new_min <- 0.0
new_max <- 60.0 
vals_all <- seq(new_min, new_max, by = 20) 

SoilHealthData_sum_TotalPLFA.nmol_plot <- ggplot(SoilHealthData_sum_TotalPLFA.nmol_emm,
                                                 aes(x = SoilDepthRange,
                                                     y = emmean,
                                                     fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.1),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "PLFA Mass (nmols/g)") +
  scale_y_continuous(
    breaks = vals_all,
    labels = sprintf("%.0f", vals_all),
    limits = c(new_min, new_max),
    position = "right"
  ) +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_TotalPLFA.nmol_plot



# # PLFA TotalPLFA_count variety 
SoilHealthData_sum_TotalPLFA_count_lm1<- lmer(TotalPLFA_count ~ SoilDepthRange * variety + (1|fact_replicateNo), data=SoilPLFAData_Clean_FieldPostPlantDat_sum)
residplot(SoilHealthData_sum_TotalPLFA_count_lm1)
anova(SoilHealthData_sum_TotalPLFA_count_lm1)

SoilHealthData_sum_TotalPLFA_count_emm<- data.frame(cld(emmeans(SoilHealthData_sum_TotalPLFA_count_lm1, ~ SoilDepthRange * variety), Letters = LETTERS))

raw_sd_data <- SoilPLFAData_Clean_FieldPostPlantDat_sum %>%
  dplyr::group_by(SoilDepthRange, variety) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA_count, na.rm = TRUE),
    SD_raw = sd(TotalPLFA_count, na.rm = TRUE)
  )
print(raw_sd_data)

SoilHealthData_sum_TotalPLFA_count_emm<- SoilHealthData_sum_TotalPLFA_count_emm %>%
  dplyr::select(-emmean) %>%
  left_join(raw_sd_data, by = c("SoilDepthRange", "variety")) %>%
  mutate(
    lower.SD = emmean - SD_raw,
    upper.SD = emmean + SD_raw,
    lower.SD_RemNeg = pmax(lower.SD, 0)
  )

SoilHealthData_sum_TotalPLFA_count_plot <- ggplot(SoilHealthData_sum_TotalPLFA_count_emm,
                                                  aes(x = SoilDepthRange,
                                                      y = emmean,
                                                      fill = variety)) +
  geom_bar(stat = "identity",
           color = "black",
           position = "dodge") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + (upper.SD * 0.1),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  labs(x = "Soil Depth (cm)",
       y = "PLFA Count (#)") +
  scale_y_continuous(position = "right") +
  coord_fixed(ratio = 1) +
  coord_flip() +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12 , family = "timesnewroman", face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "timesnewroman", hjust = 0.5),
    axis.title = element_text(size = 12, family = "timesnewroman"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12, family = "timesnewroman"),
    legend.text = element_text(size = 12, family = "timesnewroman"),
    legend.title = element_text(size = 12, family = "timesnewroman")
  ) +
  scale_fill_manual(name = "Variety",
                    values = c("Cabernet Sauvignon" = "#A60F2D", "Chardonnay" = "#CCC29C"))
SoilHealthData_sum_TotalPLFA_count_plot


plotsUV <- list(
  SoilHealthData_sum_totalC_percent_plot,
  SoilHealthData_sum_TOC_percent_plot,
  SoilHealthData_sum_inorganicC_percent_plot,
  #SoilHealthData_sum_poxC_mg.kg_AddZero_plot,
  SoilHealthData_sum_molMnred.kg_plot,
  SoilHealthData_sum_minC_plot,
  #SoilHealthData_sum_PotentiallyMinNitrate_plot,
  #SoilHealthData_sum_PotentiallyMinAmmonium_plot,
  SoilHealthData_sum_ACE_protein_plot,
  SoilHealthData_sum_olsenP_mg.kg_plot,
  SoilHealthData_sum_pH_plot,
  #SoilHealthData_sum_pH_Hconc_plot,
  SoilHealthData_sum_EC_plot,
  SoilHealthData_sum_CEC_plot,
  SoilHealthData_sum_bd_g.cm3_soil_plot,
  #SoilHealthData_sum_TotalPLFA_mass_plot,
  SoilHealthData_sum_TotalPLFA.nmol_plot,
  SoilHealthData_sum_TotalPLFA_count_plot
)

strip_titles <- function(p) {
  p + labs(title = NULL, subtitle = NULL, tag = NULL) +
    theme(
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      plot.tag      = element_blank()
    )
}
plots_nt <- lapply(plotsUV, strip_titles)

style_axis_titles <- function(p) {
  p + theme(
    axis.title.x = element_text(face = "bold", inherit.blank = TRUE),
    axis.title.y = element_blank()
  )
}
plots_nt_styled <- lapply(plots_nt, style_axis_titles)

combined_plot_UV_postplant <- ggarrange(
  plotlist = plots_nt_styled,   
  ncol = 3, nrow = 5,
  labels = "AUTO",
  align = "hv",
  common.legend = TRUE,
  legend = "bottom",
  font.label = list(size = 14, face = "bold", color = "black")
)

final_plotUV <- annotate_figure(
  combined_plot_UV_postplant,
  left = text_grob("Soil Depth (cm)", rot = 90, size = 12)
)

print(final_plotUV)

# Save to PDF
panel_w <- 4.0  
panel_h <- 3.2  
pdf_w   <- 3 * panel_w + 2.0   
pdf_h   <- 5 * panel_h + 3.0   

ggsave(
  filename = "SoilHealth_Plots_UV.pdf",
  plot = final_plotUV,
  width = pdf_w, height = pdf_h, units = "in"
  )

