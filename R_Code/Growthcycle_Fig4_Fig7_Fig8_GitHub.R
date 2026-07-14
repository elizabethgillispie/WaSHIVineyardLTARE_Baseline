
## Code for "Soil Characterization for a Long-Term Soil Health Research Vineyard in Eastern Washington"
## Alley and Undervine over Time and treatment anlaysis 
## Figures 7 and 8 



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
if(!require(patchwork)){
  install.packages("patchwork")
  require(patchwork)
}


SoilHealthData_Clean<- read_xlsx("~/PSR_Vineyard/Data/Characterization Paper 2025/PSR_Soil_SoilHealthData_Clean_02_21_2025.xlsx",
                                 sheet = "in",
                                 na = c("", 
                                        "ND")) %>% 
  mutate(totalN_percent = `totalN_%`, 
         totalC_percent = `totalC_%`,
         inorganicC_percent = `inorganicC_%`,
         TOC_percent = `TOC_%`,
         minC = `24hrminC_mgC.kg.day`,
         OM_percent = `OM_%`,
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


SoilPLFAData_Clean<- read_xlsx("~/PSR_Vineyard/Data/Characterization Paper 2025/ElCaGi_ALL_PLFA_clean_donottouch.xlsx", 
) %>%
  mutate(fact_replicateNo = as.factor(replicateNo),
         fact_TreatmentNo = as.factor(TreatmentNo)) %>%
  rename(TotalPLFA_mass = `Total PLFA (pmols/g)`,
         TotalPLFA_count = `Total PLFA#`,
         SoilDepthRange = DepthRange,
         GrowthCycle = growthCycle) %>%
  mutate(GrowthCycle = ifelse(GrowthCycle == "preplant_2023", paste("PrePlant_2023"),
                              "Fall_2023"))

#Data Preparation for Analysis:

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
                TotalPLFA_mass, TotalPLFA.nmol, TotalPLFA_count) 

head(SoilPLFAData_Clean_AnalysisDat)
unique(SoilPLFAData_Clean_AnalysisDat$GrowthCycle)


### SUMMARIZED DATA SO N=4 ######

TimeDat_DepthGC_SoilHealthData<- SoilHealthData_Clean_AnalysisDat %>%
  filter(sampleLocation != "Alleyway") %>% 
  group_by(GrowthCycle, fact_replicateNo, fact_TreatmentNo, SoilDepthRange) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(SoilDepthRange_inch = recode(SoilDepthRange, 
                                      "0_15"="0-15",
                                      "15_30"="15-30",
                                      "30_60"="30-60",
                                      "60_90"="60-90"),
         SoilDepthRange_inch = factor(SoilDepthRange_inch, levels = c("60-90", "30-60", "15-30", "0-15")))

xtabs( ~ GrowthCycle+fact_replicateNo+SoilDepthRange,
       data = TimeDat_DepthGC_SoilHealthData)
View(TimeDat_DepthGC_SoilHealthData)

TimeDat_DepthGC_SoilPLFAData<- SoilPLFAData_Clean_AnalysisDat %>%
  filter(sampleLocation != "Alleyway") %>%  
  group_by(GrowthCycle, fact_replicateNo, SoilDepthRange, fact_TreatmentNo) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(SoilDepthRange_inch = recode(SoilDepthRange, 
                                      "0_15"="0-15",
                                      "15_30"="15-30",
                                      "30_60"="30-60",
                                      "60_90"="60-90"),
         SoilDepthRange_inch = factor(SoilDepthRange_inch, levels = c("60-90", "30-60", "15-30", "0-15")))


xtabs( ~ GrowthCycle+fact_replicateNo+SoilDepthRange,
       data = TimeDat_DepthGC_SoilPLFAData)

### Raw stats for summary data ###
raw_stats_long <- TimeDat_DepthGC_SoilHealthData %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(GrowthCycle, parameter, SoilDepthRange) %>%
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
  arrange(parameter, SoilDepthRange)

print(raw_stats_table)
raw_stats_table <- raw_stats_long %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_table)
library(writexl)
write_xlsx(raw_stats_table,
           "SoilHealth_RawStats_GrowthCycle_UV.xlsx")

### Raw stats for PLFA under-vine summary data ###
raw_stats_longPLFA <- TimeDat_DepthGC_SoilPLFAData %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(GrowthCycle, parameter, SoilDepthRange) %>%
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
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_tablePLFA)

raw_stats_tablePLFA <- raw_stats_longPLFA %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_tablePLFA)
library(writexl)
write_xlsx(raw_stats_tablePLFA,
           "SoilHealth_RawStats_GrowthCycle_PLFA_UV.xlsx")

## Figure 7 - Undervine - Changes over time 

## Inorganic C and standard deviation

inorganicC_percent_stats <- TimeDat_DepthGC_SoilHealthData %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  summarise(
    n     = sum(!is.na(inorganicC_percent)),        
    emmean  = mean(inorganicC_percent, na.rm = TRUE), 
    SD    = sd(inorganicC_percent, na.rm = TRUE),   
    .groups = 'drop'
  )

print(inorganicC_percent_stats)


TimeDat_DepthGC_inorganicC_percent_lm1 <- lmer(
  inorganicC_percent ~ GrowthCycle * SoilDepthRange_inch +
    (1 | fact_replicateNo) +
    (1 | fact_replicateNo:GrowthCycle),
  data = TimeDat_DepthGC_SoilHealthData
)

residplot(TimeDat_DepthGC_inorganicC_percent_lm1)

anova(TimeDat_DepthGC_inorganicC_percent_lm1, type = "II")

inorganicC_percent_cld <- data.frame(
  cld(
    emmeans(
      TimeDat_DepthGC_inorganicC_percent_lm1,
      ~ GrowthCycle * SoilDepthRange_inch,
      type = "response"
    ),
    Letters = LETTERS,
    adjust = "none",
    remove.space = FALSE
  )
) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_inorganicC_percent_emm1 <- dplyr::left_join(inorganicC_percent_stats, inorganicC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_inorganicC_percent_emm1 <- ggplot(
  TimeDat_DepthGC_inorganicC_percent_emm1,
  aes(
    x = SoilDepthRange_inch,
    y = emmean, 
    fill = GrowthCycle
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = lower.SD_RemNeg, 
      ymax = upper.SD,        
      group = GrowthCycle
    ),
    width = 0.3,
    position = position_dodge(width = 0.9)
  ) +
  geom_text(
    aes(
      y = upper.SD + 0.05, 
      label = trimws(.group),
      group = GrowthCycle
    ),
    size = 4,
    position = position_dodge(width = 0.9),
    fontface = "bold"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant")
  ) +
  labs(
    x = "Soil Depth (cm)",
    y = "TIC (%)",
    fill = "Sample Time",
    title = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  coord_flip() +
  scale_x_discrete(position = "bottom") + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_blank(),
    legend.text = element_text(size = 12)
  )

print(UVTimeDat_DepthGC_inorganicC_percent_emm1)


#### TOC and standard deviation 

TOC_percent_stats <- TimeDat_DepthGC_SoilHealthData %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  summarise(
    emmean = mean(TOC_percent, na.rm = TRUE), 
    SD = sd(TOC_percent, na.rm = TRUE), 
    .groups = 'drop'
  )
print(TOC_percent_stats)

TimeDat_DepthGC_TOC_percent_lm1<- lmer(TOC_percent ~ GrowthCycle * SoilDepthRange_inch + 
                                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                       data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_TOC_percent_lm1)
anova(TimeDat_DepthGC_TOC_percent_lm1, type = "II")

TOC_percent_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_TOC_percent_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_TOC_percent_emm1 <- left_join(TOC_percent_stats, TOC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_TOC_percent_emm1 <- ggplot(TimeDat_DepthGC_TOC_percent_emm1, aes(x = SoilDepthRange_inch,
                                                                                   y = emmean, 
                                                                                   fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD),       
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05, 
                label = trimws(.group),
                group = GrowthCycle), 
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "TOC (%)",
       fill = "Sample Time", 
  )+
  coord_flip()+
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_TOC_percent_emm1)


## Total Carbon and standard deviation

totalC_percent_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(totalC_percent, na.rm = TRUE), 
    SD = sd(totalC_percent, na.rm = TRUE), 
    .groups = 'drop'
  )
print(totalC_percent_stats)

TimeDat_DepthGC_totalC_percent_lm1<- lmer(totalC_percent ~ GrowthCycle * SoilDepthRange_inch + 
                                            (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                          data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_totalC_percent_lm1)
anova(TimeDat_DepthGC_totalC_percent_lm1, type = "II")

totalC_percent_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_totalC_percent_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_totalC_percent_emm1 <- dplyr::left_join(totalC_percent_stats, totalC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_totalC_percent_emm1 <- ggplot(TimeDat_DepthGC_totalC_percent_emm1, aes(x = SoilDepthRange_inch,
                                                                                         y = emmean, 
                                                                                         fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "TC (%)",
       fill = "Sample Time",
  )+
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_totalC_percent_emm1)



# minC and standard deviation

minC_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(minC, na.rm = TRUE), 
    SD = sd(minC, na.rm = TRUE), 
    .groups = 'drop'
  )
print(minC_stats)

TimeDat_DepthGC_minC_lm1<- lmer(minC ~ GrowthCycle * SoilDepthRange_inch + 
                                  (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_minC_lm1)
anova(TimeDat_DepthGC_minC_lm1, type = "II")

minC_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_minC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_minC_emm1 <- dplyr::left_join(minC_stats, minC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_minC_emm1 <- ggplot(TimeDat_DepthGC_minC_emm1, aes(x = SoilDepthRange_inch,
                                                                     y = emmean, 
                                                                     fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 10,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "MinC (mg/kg/day)",
       fill = "Sample Time",
  )+
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, 20),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_minC_emm1)


# POXC and standard deviation

poxC_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    n = sum(!is.na(poxC_mg.kg_AddZero)),              
    emmean = mean(poxC_mg.kg_AddZero, na.rm = TRUE),  
    SD = sd(poxC_mg.kg_AddZero, na.rm = TRUE),        
    .groups = 'drop'
  )

print(poxC_stats)

TimeDat_DepthGC_poxC_mg.kg_AddZero_lm1<- lmer(poxC_mg.kg_AddZero ~ GrowthCycle * SoilDepthRange_inch + 
                                                (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                              data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_poxC_mg.kg_AddZero_lm1)
anova(TimeDat_DepthGC_poxC_mg.kg_AddZero_lm1, type = "II")

poxC_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_poxC_mg.kg_AddZero_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_poxC_mg.kg_AddZero_emm1 <- dplyr::left_join(poxC_stats, poxC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_poxC_mg.kg_AddZero_emm1 <- ggplot(TimeDat_DepthGC_poxC_mg.kg_AddZero_emm1, aes(x = SoilDepthRange_inch,
                                                                                                 y = emmean, 
                                                                                                 fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 30,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "POXC (mg/kg)",
       fill = "Sample Time",
  )+
  scale_y_continuous(position = "right") +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_poxC_mg.kg_AddZero_emm1)



# POXC molMn/kg and standard deviation

poxC2_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    n = sum(!is.na(molMnred.kg)),    
    emmean = mean(molMnred.kg, na.rm = TRUE), 
    SD = sd(molMnred.kg, na.rm = TRUE), 
    .groups = 'drop'
  )
print(poxC2_stats)

TimeDat_DepthGC_molMnred.kg_lm1<- lmer(molMnred.kg ~ GrowthCycle * SoilDepthRange_inch + 
                                                (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                              data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_molMnred.kg_lm1)
anova(TimeDat_DepthGC_molMnred.kg_lm1, type = "II")

poxC2_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_molMnred.kg_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_molMnred.kg_emm1 <- dplyr::left_join(poxC2_stats, poxC2_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_molMnred.kg_emm1 <- ggplot(TimeDat_DepthGC_molMnred.kg_emm1, aes(x = SoilDepthRange_inch,
                                                                                                 y = emmean, 
                                                                                                 fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + .002,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = bquote(bold("POXC (mol MnO"[4]^"-"*"/kg)")),
       fill = "Sample Time",
  )+
  scale_y_continuous(position = "right") +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_molMnred.kg_emm1)

## TREATMENT EFFECT GRAPH Figure 4A


# molMnred.kg - treatment effect UNDERVINE

names(TimeDat_DepthGC_SoilHealthData
      )

molMnred.kg_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(fact_TreatmentNo) %>%
  dplyr::summarise(
    emmean = mean(molMnred.kg, na.rm = TRUE), 
    SD = sd(molMnred.kg, na.rm = TRUE), 
    .groups = 'drop'
  )
print(molMnred.kg_stats)

TimeDat_DepthGC_molMnred.kg_lm1<- lmer(molMnred.kg ~ fact_TreatmentNo * GrowthCycle * SoilDepthRange_inch + 
                                         (1|fact_replicateNo) + (1|fact_replicateNo:fact_TreatmentNo) + (1|fact_replicateNo:fact_TreatmentNo:GrowthCycle),
                                       data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_molMnred.kg_lm1)
anova(TimeDat_DepthGC_molMnred.kg_lm1, type = "II")


molMnred.kg_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_molMnred.kg_lm1, ~ fact_TreatmentNo, type = "response"), Letters = LETTERS, adjust = "none")) %>%
  dplyr::select(fact_TreatmentNo, .group) 

TimeDat_DepthGC_molMnred.kg_emm2 <- dplyr::left_join(molMnred.kg_stats, molMnred.kg_cld, by = "fact_TreatmentNo") %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

TimeDat_DepthGC_molMnred.kg_plot2<- ggplot(TimeDat_DepthGC_molMnred.kg_emm2, aes(x = fact_TreatmentNo,
                                                                                 y = emmean, 
                                                                                 fill = fact_TreatmentNo))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD),       
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + .002,
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "plain") +
  scale_fill_manual(values=c("grey30", "grey50", "grey65", "grey80", "grey95"),
                    breaks = c("1","2","3","4","5"),
                    labels = c("TR1","TR2",
                               "TR3","TR4",
                               "TR5"))+
  labs(x = "Treatment",
       y = bquote("POXC (mol MnO"[4]^"-"*"/kg)"), 
       fill = "Treatment") +
  scale_y_continuous(position = "left") + 
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(TimeDat_DepthGC_molMnred.kg_plot2)

# PotentiallyMinNitrate and standard error - not used in graph

TimeDat_DepthGC_PotentiallyMinNitrate_lm1<- lmer(PotentiallyMinNitrate ~ GrowthCycle * SoilDepthRange_inch + 
                                                   (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                                 data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_PotentiallyMinNitrate_lm1)
anova(TimeDat_DepthGC_PotentiallyMinNitrate_lm1, type = "II") 

TimeDat_DepthGC_PotentiallyMinNitrate_emm1<- data_frame(cld(emmeans(TimeDat_DepthGC_PotentiallyMinNitrate_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none"))  %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

UVTimeDat_DepthGC_PotentiallyMinNitrate_emm1 <- ggplot(TimeDat_DepthGC_PotentiallyMinNitrate_emm1, aes(x = SoilDepthRange_inch,
                                                                                                       y = emmean,
                                                                                                       fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(TimeDat_DepthGC_PotentiallyMinNitrate_emm1$emmean)*0.4),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("PMN-NO"[3]*" (mg/kg)"), 
       fill = "Sample Time", 
      )+
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_PotentiallyMinNitrate_emm1)


# PotentiallyMinAmmonium and standard error - not used in graph

TimeDat_DepthGC_PotentiallyMinAmmonium_lm1<- lmer(PotentiallyMinAmmonium ~ GrowthCycle * SoilDepthRange_inch + 
                                                   (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                                 data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_PotentiallyMinAmmonium_lm1)
anova(TimeDat_DepthGC_PotentiallyMinAmmonium_lm1, type = "II") 

TimeDat_DepthGC_PotentiallyMinAmmonium_emm1<- data_frame(cld(emmeans(TimeDat_DepthGC_PotentiallyMinAmmonium_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none"))  %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

UVTimeDat_DepthGC_PotentiallyMinAmmonium_emm1 <- ggplot(TimeDat_DepthGC_PotentiallyMinAmmonium_emm1, aes(x = SoilDepthRange_inch,
                                                                                                       y = emmean,
                                                                                                       fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(TimeDat_DepthGC_PotentiallyMinAmmonium_emm1$emmean)*0.4),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("PMN-NO"[3]*" (mg/kg)"), 
       fill = "Sample Time", 
      )+
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_PotentiallyMinAmmonium_emm1)


## ACE Protein and standard deviation

ACE_protein_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    n = sum(!is.na(ACE_protein)),    
    emmean = mean(ACE_protein, na.rm = TRUE), 
    SD = sd(ACE_protein, na.rm = TRUE), 
    .groups = 'drop'
  )
print(ACE_protein_stats)
TimeDat_DepthGC_ACE_protein_lm1<- lmer(ACE_protein ~ GrowthCycle * SoilDepthRange_inch + 
                                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                       data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_ACE_protein_lm1)
anova(TimeDat_DepthGC_ACE_protein_lm1, type = "II")

ACE_protein_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_ACE_protein_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_ACE_protein_emm1 <- dplyr::left_join(ACE_protein_stats, ACE_protein_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_ACE_protein_emm1 <- ggplot(TimeDat_DepthGC_ACE_protein_emm1, 
                                             aes(x = SoilDepthRange_inch,
                                                 y = emmean,
                                                 fill = GrowthCycle)) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD,
                    group = GrowthCycle),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.1,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant")
  ) +
  labs(x = "Soil Depth (cm)",
       y = "ACE Protein (g/kg)",
       fill = "Sample Time") +
  scale_y_continuous(
    position = "right",
    breaks  = seq(0, 4, by = 0.5),                      
    labels  = ifelse(seq(0, 4, by = 0.5) %% 1 == 0,     
                     seq(0, 4, by = 0.5),
                     "")
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title   = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.text    = element_text(size = 12),
    title        = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    legend.text  = element_text(size = 12)
  )


print(UVTimeDat_DepthGC_ACE_protein_emm1)


# olsenP_mg.kg and standard deviation

olsenP_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    n = sum(!is.na(olsenP_mg.kg)),                
    emmean = mean(olsenP_mg.kg, na.rm = TRUE),     
    SD   = sd(olsenP_mg.kg, na.rm = TRUE),       
    .groups = "drop"
  )

print(olsenP_stats)

TimeDat_DepthGC_olsenP_mg.kg_lm1<- lmer(olsenP_mg.kg ~ GrowthCycle * SoilDepthRange_inch + 
                                          (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                        data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_olsenP_mg.kg_lm1)
anova(TimeDat_DepthGC_olsenP_mg.kg_lm1, type = "II")

olsenP_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_olsenP_mg.kg_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_olsenP_mg.kg_emm1 <- dplyr::left_join(olsenP_stats, olsenP_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_olsenP_mg.kg_emm1 <- ggplot(TimeDat_DepthGC_olsenP_mg.kg_emm1, aes(x = SoilDepthRange_inch,
                                                                                     y = emmean, 
                                                                                     fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 1, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "Available P (mg/kg)",
       fill = "Sample Time",
  )+
  scale_y_continuous(position = "right", limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_olsenP_mg.kg_emm1)


# pH - standard deviation from pH HCon plot is used in the Figure

pH_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(pH, na.rm = TRUE),
    SD = sd(pH, na.rm = TRUE), 
    .groups = 'drop'
  )
print(pH_stats)

TimeDat_DepthGC_pH_lm1<- lmer(pH ~ GrowthCycle * SoilDepthRange_inch +
                                (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                              data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_pH_lm1)
anova(TimeDat_DepthGC_pH_lm1, type = "II")

pH_cld_structure <- data.frame(cld(emmeans(TimeDat_DepthGC_pH_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch)

TimeDat_DepthGC_pH_emm1 <- dplyr::left_join(pH_cld_structure, pH_stats, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    .group = c("E","D","BC","ABC","C","ABC","AB","A"),
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_pH_emm1 <- ggplot(TimeDat_DepthGC_pH_emm1, aes(x = SoilDepthRange_inch,
                                                                 y = emmean, 
                                                                 fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.4,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = ("pH"), 
       fill = "Sample Time",
  )+
  scale_y_continuous(position = "right", breaks = seq(5, 9, by = 1)) +
  coord_flip(ylim = (c(5,9)))+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_pH_emm1)


#pH H+ ion concentration and standard deviation
TimeDat_DepthGC_pH_Hconc_lm1<- lmer(pH_Hconc ~ GrowthCycle * SoilDepthRange_inch + 
                                      (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                    data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_pH_Hconc_lm1)
anova(TimeDat_DepthGC_pH_Hconc_lm1, type = "II") 

TimeDat_DepthGC_pH_Hconc_emm1<- data_frame(cld(emmeans(TimeDat_DepthGC_pH_Hconc_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

UVTimeDat_DepthGC_pH_Hconc_emm1 <- ggplot(TimeDat_DepthGC_pH_Hconc_emm1, aes(x = SoilDepthRange_inch,
                                                                             y = emmean,
                                                                             fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(TimeDat_DepthGC_pH_Hconc_emm1$emmean)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("H"^"+"*"ion concentration"), 
       fill = "Sample Time", 
      )+
  scale_y_continuous(position = "right") +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_pH_Hconc_emm1)

## Bulk Denisty and standard deviation

bd_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(bd_g.cm3_soil, na.rm = TRUE), 
    SD = sd(bd_g.cm3_soil, na.rm = TRUE), 
    .groups = 'drop'
  )
print(bd_stats)

TimeDat_DepthGC_bd_g.cm3_soil_lm1<- lmer(bd_g.cm3_soil ~ GrowthCycle * SoilDepthRange_inch + 
                                           (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                         data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_bd_g.cm3_soil_lm1)
anova(TimeDat_DepthGC_bd_g.cm3_soil_lm1, type = "II")

bd_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_bd_g.cm3_soil_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_bd_g.cm3_soil_emm1 <- dplyr::left_join(bd_stats, bd_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )


UVTimeDat_DepthGC_bd_g.cm3_soil_emm1 <- ggplot(TimeDat_DepthGC_bd_g.cm3_soil_emm1,
                                               aes(x = SoilDepthRange_inch,
                                                   y = emmean,
                                                   fill = GrowthCycle)) +
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg,
                    ymax = upper.SD,
                    group = GrowthCycle),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.2,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant")
  ) +
  labs(
    x = "Soil Depth (cm)",
    y = bquote(bold("Bulk Density (g/cm"^3*")")),
    fill = "Sample Time"
  ) +
  scale_y_continuous(
    position = "right",
    breaks = seq(0, 2.0, by = 0.5),
    labels = sprintf("%.1f", seq(0, 2.0, by = 0.5)),
    limits = c(0, 2.0)       
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title   = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank(),
    axis.text    = element_text(size = 12),
    title        = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5),
    legend.text  = element_text(size = 12)
  )

print(UVTimeDat_DepthGC_bd_g.cm3_soil_emm1)


## EC and standard deviation

EC_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(EC, na.rm = TRUE), 
    SD = sd(EC, na.rm = TRUE),       
    .groups = 'drop'
  )
print(EC_stats)

TimeDat_DepthGC_EC_lm1<- lmer(EC ~ GrowthCycle * SoilDepthRange_inch + 
                                (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                              data = TimeDat_DepthGC_SoilHealthData)

EC_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_EC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_EC_emm1 <- dplyr::left_join(EC_stats, EC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_EC_emm1 <- ggplot(TimeDat_DepthGC_EC_emm1, aes(x = SoilDepthRange_inch,
                                                                 y = emmean, 
                                                                 fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = ("EC (dS/m)"), 
       fill = "Sample Time",
  )+
  scale_y_continuous(position = "right") +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_EC_emm1)

## CEC and standard deviation

CEC_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(CEC, na.rm = TRUE), 
    SD = sd(CEC, na.rm = TRUE),       
    .groups = 'drop'
  )
print(CEC_stats)

TimeDat_DepthGC_CEC_lm1<- lmer(CEC ~ GrowthCycle * SoilDepthRange_inch + 
                                 (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                               data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_CEC_lm1)
anova(TimeDat_DepthGC_CEC_lm1, type = "II")

CEC_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_CEC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_CEC_emm1 <- dplyr::left_join(CEC_stats, CEC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_CEC_emm1 <- ggplot(TimeDat_DepthGC_CEC_emm1, aes(x = SoilDepthRange_inch,
                                                                   y = emmean, 
                                                                   fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 1,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression(bold("CEC (cmol"[c]*"/kg)")), 
       fill = "Sample Time",
  )+
  scale_y_continuous(
    position = "right",
    breaks = seq(0, 15, by = 3),                 
    labels = sprintf("%.0f", seq(0, 15, by = 3)),
    limits = c(0, 15)
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_CEC_emm1)


# TotalPLFA_mass and standard deviation (not used in figure)

TotalPLFA_stats <- TimeDat_DepthGC_SoilPLFAData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA_mass, na.rm = TRUE), 
    SD = sd(TotalPLFA_mass, na.rm = TRUE),       
    .groups = 'drop'
  )
print(TotalPLFA_stats)

TimeDat_DepthGC_TotalPLFA_mass_lm1<- lmer(TotalPLFA_mass ~ GrowthCycle * SoilDepthRange_inch + 
                                            (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                          data = TimeDat_DepthGC_SoilPLFAData)
residplot(TimeDat_DepthGC_TotalPLFA_mass_lm1)
anova(TimeDat_DepthGC_TotalPLFA_mass_lm1, type = "II")

TotalPLFA_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_TotalPLFA_mass_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_TotalPLFA_mass_emm1 <- dplyr::left_join(TotalPLFA_stats, TotalPLFA_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_TotalPLFA_mass_emm1 <- ggplot(TimeDat_DepthGC_TotalPLFA_mass_emm1, aes(x = SoilDepthRange_inch,
                                                                                         y = emmean, 
                                                                                         fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black"
  ) +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 2000,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = 0.9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant")) +
  labs(x = "Soil Depth (inch)",
       y = "PLFA mass (pmols/g)",
       fill = "Sample Time",
  ) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12))

print(UVTimeDat_DepthGC_TotalPLFA_mass_emm1)


# TotalPLFA.nmol_plot and standard deviation

TotalPLFA.nmol_stats <- TimeDat_DepthGC_SoilPLFAData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA.nmol, na.rm = TRUE), 
    SD = sd(TotalPLFA.nmol, na.rm = TRUE),       
    .groups = 'drop'
  )
print(TotalPLFA.nmol_stats)

TimeDat_DepthGC_TotalPLFA.nmol_lm1<- lmer(TotalPLFA.nmol ~ GrowthCycle * SoilDepthRange_inch + 
                                            (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                          data = TimeDat_DepthGC_SoilPLFAData)
residplot(TimeDat_DepthGC_TotalPLFA.nmol_lm1)
anova(TimeDat_DepthGC_TotalPLFA.nmol_lm1, type = "II")

TotalPLFA.nmol_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_TotalPLFA.nmol_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_TotalPLFA.nmol_emm1 <- dplyr::left_join(TotalPLFA.nmol_stats, TotalPLFA.nmol_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_TotalPLFA.nmol_emm1 <- ggplot(TimeDat_DepthGC_TotalPLFA.nmol_emm1, aes(x = SoilDepthRange_inch,
                                                                                         y = emmean, 
                                                                                         fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black"
  ) +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 3,
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = 0.9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant")) +
  labs(x = "Soil Depth (inch)",
       y = "PLFA mass (nmols/g)",
       fill = "Sample Time",
  ) +
  scale_y_continuous(
    position = "right",
    breaks  = seq(0, 70, by = 5),                                      
    labels  = ifelse(seq(0, 70, by = 5) %% 20 == 0,                    
                     sprintf("%.0f", seq(0, 70, by = 5)), "")          
  ) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12))

print(UVTimeDat_DepthGC_TotalPLFA.nmol_emm1)


# TotalPLFA_count and standard deviation

TotalPLFA_count_stats <- TimeDat_DepthGC_SoilPLFAData %>%
  dplyr::group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA_count, na.rm = TRUE), 
    SD = sd(TotalPLFA_count, na.rm = TRUE),       
    .groups = 'drop'
  )
print(TotalPLFA_count_stats)

TimeDat_DepthGC_TotalPLFA_count_lm1<- lmer(TotalPLFA_count ~ GrowthCycle * SoilDepthRange_inch + 
                                             (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                           data = TimeDat_DepthGC_SoilPLFAData)
residplot(TimeDat_DepthGC_TotalPLFA_count_lm1)
anova(TimeDat_DepthGC_TotalPLFA_count_lm1, type = "II")

TotalPLFA_count_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_TotalPLFA_count_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group) 

TimeDat_DepthGC_TotalPLFA_count_emm1 <- dplyr::left_join(TotalPLFA_count_stats, TotalPLFA_count_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

UVTimeDat_DepthGC_TotalPLFA_count_emm1 <- ggplot(TimeDat_DepthGC_TotalPLFA_count_emm1, aes(x = SoilDepthRange_inch,
                                                                                           y = emmean, 
                                                                                           fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  ) +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD,       
                    group = GrowthCycle), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 4,
                label = trimws(.group),
                group = GrowthCycle),
            position = position_dodge(width = 0.9),
            size = 4, 
            fontface = "bold") +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (inch)",
       y = "PLFA count (#)",
       fill = "Sample Time",
  )+
  scale_y_continuous(
    position = "right",
    breaks  = seq(0, 50, by = 5),        
    labels  = ifelse(seq(0, 50, by = 5) %in% seq(0, 50, by = 10),
                     sprintf("%.0f", seq(0, 50, by = 5)),
                     "")
  ) +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(UVTimeDat_DepthGC_TotalPLFA_count_emm1)


#### Combined Graph for Under Vine Depth  x Time - Figure 7


plots <- list(
  UVTimeDat_DepthGC_totalC_percent_emm1,
  UVTimeDat_DepthGC_TOC_percent_emm1,
  UVTimeDat_DepthGC_inorganicC_percent_emm1,
  UVTimeDat_DepthGC_molMnred.kg_emm1,
  UVTimeDat_DepthGC_minC_emm1,
  UVTimeDat_DepthGC_ACE_protein_emm1,
  UVTimeDat_DepthGC_olsenP_mg.kg_emm1,
  UVTimeDat_DepthGC_pH_emm1,
  UVTimeDat_DepthGC_EC_emm1,
  UVTimeDat_DepthGC_CEC_emm1,
  UVTimeDat_DepthGC_bd_g.cm3_soil_emm1,
  UVTimeDat_DepthGC_TotalPLFA.nmol_emm1,
  UVTimeDat_DepthGC_TotalPLFA_count_emm1
)


strip_titles <- function(p) {
  p +
    labs(title = NULL, subtitle = NULL, tag = NULL, x = NULL) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.tag = element_blank()
    )
}

plots_nt <- lapply(plots, strip_titles)

combined_plot_UV_Time <- ggarrange(
  plotlist = plots_nt,
  ncol = 3, nrow = 5,
  labels = "AUTO",
  align = "hv",
  common.legend = TRUE,
  legend = "bottom"
)

combined_plot_UV_Time <- ggpar(
  combined_plot_UV_Time,
  font.legend = c(size = 14, face = "bold", color = "black"),
  font.x      = c(size = 14, face = "bold", color = "black"),
  font.y      = c(size = 14, face = "bold", color = "black")
)

final_plot <- annotate_figure(
  combined_plot_UV_Time,
  left = text_grob("Soil Depth (cm)", rot = 90, size = 14)
)

print(final_plot)



##### Figure 8 - Alleyway - Changes over time 

SoilHealthData_Clean_AlleyDat<- SoilHealthData_Clean_AnalysisDat %>%
  filter(sampleLocation != "Undervine") %>% #removing undervine samples from the analysis***
  group_by(GrowthCycle, fact_replicateNo, SoilDepthRange, fact_TreatmentNo) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(SoilDepthRange_inch = recode(SoilDepthRange, 
                                      "0_15"="0-15",
                                      "15_30"="15-30",
                                      "30_60"="30-60",
                                      "60_90"="60-90"),
         SoilDepthRange_inch = factor(SoilDepthRange_inch, levels = c("60-90", "30-60", "15-30", "0-15")))

xtabs( ~ GrowthCycle+fact_replicateNo+SoilDepthRange,
       data = SoilHealthData_Clean_AlleyDat)
head(SoilHealthData_Clean_AlleyDat)


SoilHealthData_Clean_AlleyDataPLFA<- SoilPLFAData_Clean_AnalysisDat %>%
  filter(sampleLocation != "Undervine") %>%  #removing undervine samples from the analysis***
  group_by(GrowthCycle, fact_replicateNo, SoilDepthRange, fact_TreatmentNo) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(SoilDepthRange_inch = recode(SoilDepthRange, 
                                      "0_15"="0-15",
                                      "15_30"="15-30",
                                      "30_60"="30-60",
                                      "60_90"="60-90"),
         SoilDepthRange_inch = factor(SoilDepthRange_inch, levels = c("60-90", "30-60", "15-30", "0-15")))


xtabs( ~ GrowthCycle+fact_replicateNo+SoilDepthRange,
       data = SoilHealthData_Clean_AlleyDataPLFA)

### Raw stats for summary data ###
raw_stats_longA <- SoilHealthData_Clean_AlleyDat %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(GrowthCycle, parameter, SoilDepthRange) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_longA)

raw_stats_tableA <- raw_stats_longA %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(parameter, SoilDepthRange)

print(raw_stats_tableA)
raw_stats_tableA <- raw_stats_longA %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_tableA)
library(writexl)
write_xlsx(raw_stats_tableA,
           "SoilHealth_RawStats_GrowthCycle_A.xlsx")

### Raw stats for PLFA alleyway summary data ###
raw_stats_longPLFA_A <- SoilHealthData_Clean_AlleyDataPLFA %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(GrowthCycle, parameter, SoilDepthRange) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(raw_stats_longPLFA_A)

raw_stats_tablePLFA_A <- raw_stats_longPLFA_A %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_tablePLFA_A)

raw_stats_tablePLFA_A <- raw_stats_longPLFA_A %>%
  mutate(
    mean = round(mean, 3),
    sd   = round(sd, 3)
  ) %>%
  arrange(GrowthCycle, parameter, SoilDepthRange)

print(raw_stats_tablePLFA_A)
library(writexl)
write_xlsx(raw_stats_tablePLFA_A,
           "SoilHealth_RawStats_GrowthCycle_PLFA_A.xlsx")


## Inorganic C Percent and standard deviation

AlleyDat_inorganicC_percent_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(inorganicC_percent, na.rm = TRUE),
    SD = sd(inorganicC_percent, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_inorganicC_percent_stats)

AlleyDat_inorganicC_percent_lm1<- lmer(inorganicC_percent ~ GrowthCycle * SoilDepthRange_inch +
                                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                       data = SoilHealthData_Clean_AlleyDat)

residplot(AlleyDat_inorganicC_percent_lm1)
anova(AlleyDat_inorganicC_percent_lm1, type = "II") 

AlleyDat_inorganicC_percent_cld <- data.frame(
  cld(
    emmeans(
      AlleyDat_inorganicC_percent_lm1,
      ~ GrowthCycle * SoilDepthRange_inch,
      type = "response"
    ),
    Letters = LETTERS,
    adjust = "none",
    remove.space = FALSE
  )
) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_inorganicC_percent_emm1 <- dplyr::left_join(AlleyDat_inorganicC_percent_stats, AlleyDat_inorganicC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_TIC <- ggplot(AlleyDat_DepthGC_inorganicC_percent_emm1, aes(x = SoilDepthRange_inch,
                                                                     y = emmean, 
                                                                     fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD),  
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "TIC (%)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_TIC)



## TOC_percent and standard deviation

AlleyDat_TOC_percent_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TOC_percent, na.rm = TRUE),
    SD = sd(TOC_percent, na.rm = TRUE),
    .groups = 'drop'
  )


AlleyDat_TOC_percent_lm1<- lmer(sqrt(TOC_percent) ~ GrowthCycle * SoilDepthRange_inch +
                                  (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                data = SoilHealthData_Clean_AlleyDat)

residplot(AlleyDat_TOC_percent_lm1)
anova(AlleyDat_TOC_percent_lm1, type = "II") 

AlleyDat_TOC_percent_cld <- data.frame(
  cld(
    emmeans(
      AlleyDat_TOC_percent_lm1,
      ~ GrowthCycle * SoilDepthRange_inch,
      type = "response"
    ),
    Letters = LETTERS,
    adjust = "none",
    remove.space = FALSE
  )
) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_TOC_percent_emm1 <- dplyr::left_join(AlleyDat_TOC_percent_stats, AlleyDat_TOC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    response = emmean, 
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_TOC <- ggplot(AlleyDat_DepthGC_TOC_percent_emm1, aes(x = SoilDepthRange_inch,
                                                              y = response, 
                                                              fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "TOC (%)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_TOC)


## Total c percent and standard deviation

AlleyDat_totalC_percent_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(totalC_percent, na.rm = TRUE),
    SD = sd(totalC_percent, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_totalC_percent_stats)

AlleyDat_totalC_percent_lm1<- lmer(totalC_percent ~ GrowthCycle * SoilDepthRange_inch +
                                     (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                   data = SoilHealthData_Clean_AlleyDat)

residplot(AlleyDat_totalC_percent_lm1)
anova(AlleyDat_totalC_percent_lm1, type = "II") 

AlleyDat_totalC_percent_cld <- data.frame(
  cld(
    emmeans(
      AlleyDat_totalC_percent_lm1,
      ~ GrowthCycle * SoilDepthRange_inch,
      type = "response"
    ),
    Letters = LETTERS,
    adjust = "none",
    remove.space = FALSE
  )
) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_totalC_percent_emm1 <- dplyr::left_join(AlleyDat_totalC_percent_stats, AlleyDat_totalC_percent_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_DepthGC_totalC_percent_plot1<- ggplot(AlleyDat_DepthGC_totalC_percent_emm1, aes(x = SoilDepthRange_inch,
                                                                                         y = emmean, 
                                                                                         fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "TC (%)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, 0.25),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_DepthGC_totalC_percent_plot1)


# molMnred.kg (POXC) and standard deviation

AlleyDat_molMnred.kg_lm1<- lmer(molMnred.kg ~ GrowthCycle * SoilDepthRange_inch + 
                                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                       data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_molMnred.kg_lm1)
anova(AlleyDat_molMnred.kg_lm1, type = "II") 

AlleyDat_molMnred.kg_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(molMnred.kg, na.rm = TRUE),
    SD = sd(molMnred.kg, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_molMnred.kg_stats)

AlleyDat_molMnred.kg_cld <- data.frame(
  cld(
    emmeans(
      AlleyDat_molMnred.kg_lm1, 
      ~ GrowthCycle * SoilDepthRange_inch,
      type = "response"
    ),
    Letters = LETTERS,
    adjust = "none",
    remove.space = FALSE
  )
) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)

AlleyDat_DepthGC_molMnred.kg_emm1 <- dplyr::left_join(AlleyDat_molMnred.kg_stats, AlleyDat_molMnred.kg_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_molMnred.kg <- ggplot(AlleyDat_DepthGC_molMnred.kg_emm1, aes(x = SoilDepthRange_inch,
                                                                      y = emmean, 
                                                                      fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.005,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = bquote(bold("POXC (mol MnO"[4]^"-"*"/kg)")),
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_molMnred.kg)


# poxC_mg.kg_AddZero (not used in figure)

AlleyDat_poxC_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(poxC_mg.kg_AddZero, na.rm = TRUE),
    SD = sd(poxC_mg.kg_AddZero, na.rm = TRUE),
    .groups = 'drop'
  )

AlleyDat_poxC_mg.kg_AddZero_lm1<- lmer(poxC_mg.kg_AddZero ~ GrowthCycle * SoilDepthRange_inch +
                                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                       data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_poxC_mg.kg_AddZero_lm1)
anova(AlleyDat_poxC_mg.kg_AddZero_lm1, type = "II") 

AlleyDat_poxC_mg.kg_AddZero_cld <- data.frame(cld(emmeans(AlleyDat_poxC_mg.kg_AddZero_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_poxC_mg.kg_AddZero_emm1 <- dplyr::left_join(AlleyDat_poxC_stats, AlleyDat_poxC_mg.kg_AddZero_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_POXC <- ggplot(AlleyDat_DepthGC_poxC_mg.kg_AddZero_emm1, aes(x = SoilDepthRange_inch,
                                                                      y = emmean,
                                                                      fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "POXC (mg/kg)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_POXC)



# minC and standard deviation 

AlleyDat_minC_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(minC, na.rm = TRUE),
    SD = sd(minC, na.rm = TRUE),
    .groups = 'drop'
  )

AlleyDat_minC_lm1<- lmer(minC ~ GrowthCycle * SoilDepthRange_inch +
                           (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                         data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_minC_lm1)
anova(AlleyDat_minC_lm1, type = "II") 

AlleyDat_minC_cld <- data.frame(cld(emmeans(AlleyDat_minC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_minC_emm1 <- dplyr::left_join(AlleyDat_minC_stats, AlleyDat_minC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_minC <- ggplot(AlleyDat_DepthGC_minC_emm1, aes(x = SoilDepthRange_inch,
                                                        y = emmean,
                                                        fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 7, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "Mineralizable C (mg/kg/day)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_minC)


# PotentiallyMinNitrate (not used in figure)

AlleyDat_PotentiallyMinNitrate_lm1<- lmer(log(PotentiallyMinNitrate) ~ GrowthCycle * SoilDepthRange_inch + 
                                            (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                          data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_PotentiallyMinNitrate_lm1)
anova(AlleyDat_PotentiallyMinNitrate_lm1, type = "II") 

AlleyDat_DepthGC_PotentiallyMinNitrate_emm1<- data_frame(cld(emmeans(AlleyDat_PotentiallyMinNitrate_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  mutate(lower.SE = response-SE,
         upper.SE = response+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

AlleyDat_PMNNitrate <- ggplot(AlleyDat_DepthGC_PotentiallyMinNitrate_emm1, aes(x = SoilDepthRange_inch,
                                                                               y = response,
                                                                               fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SE+(mean(AlleyDat_DepthGC_PotentiallyMinNitrate_emm1$upper.SE)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("PMN-NO"[3]*" (mg/kg)"),
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_PMNNitrate)



# PotentiallyMinAmmonium (not used in Figure)
AlleyDat_PotentiallyMinAmmonium_lm1<- lmer(sqrt(PotentiallyMinAmmonium) ~ GrowthCycle * SoilDepthRange_inch + 
                                             (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                                           data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_PotentiallyMinAmmonium_lm1)
anova(AlleyDat_PotentiallyMinAmmonium_lm1, type = "II") 

AlleyDat_DepthGC_PotentiallyMinAmmonium_emm1<- data_frame(cld(emmeans(AlleyDat_PotentiallyMinAmmonium_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  mutate(lower.SE = response-SE,
         upper.SE = response+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )
AlleyDat_PMNAmm <- ggplot(AlleyDat_DepthGC_PotentiallyMinAmmonium_emm1, aes(x = SoilDepthRange_inch,
                                                                            y = response,
                                                                            fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                  
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SE+(mean(AlleyDat_DepthGC_PotentiallyMinAmmonium_emm1$upper.SE)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("PMN-NH"[4]*" (mg/kg)"),
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_PMNAmm)


# olsenP_mg.kg and standard deviation

AlleyDat_olsenP_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(olsenP_mg.kg, na.rm = TRUE),
    SD = sd(olsenP_mg.kg, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_olsenP_stats)


AlleyDat_olsenP_mg.kg_lm1<- lmer(olsenP_mg.kg ~ GrowthCycle * SoilDepthRange_inch +
                                   (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                 data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_olsenP_mg.kg_lm1)
anova(AlleyDat_olsenP_mg.kg_lm1, type = "II") 

AlleyDat_olsenP_mg.kg_cld <- data.frame(cld(emmeans(AlleyDat_olsenP_mg.kg_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_olsenP_mg.kg_emm1 <- dplyr::left_join(AlleyDat_olsenP_stats, AlleyDat_olsenP_mg.kg_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    response = emmean,
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_P <- ggplot(AlleyDat_DepthGC_olsenP_mg.kg_emm1, aes(x = SoilDepthRange_inch,
                                                             y = response, 
                                                             fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 2,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "Available P (mg/kg)", 
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_P)


# pH (using pH HCon standard deviation letters)

AlleyDat_pH_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(pH, na.rm = TRUE),
    SD = sd(pH, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_pH_stats)


AlleyDat_pH_lm1<- lmer(log(pH) ~ GrowthCycle * SoilDepthRange_inch +
                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                       data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_pH_lm1)
anova(AlleyDat_pH_lm1, type = "II") 

AlleyDat_pH_cld1 <- data.frame(cld(emmeans(AlleyDat_pH_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch) %>% 
  mutate(.group = c("C","C","B","B","AB","AB","A","A")) 

AlleyDat_DepthGC_pH_emm1 <- dplyr::left_join(AlleyDat_pH_stats, AlleyDat_pH_cld1, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    response = emmean,
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_pH <- ggplot(AlleyDat_DepthGC_pH_emm1, aes(x = SoilDepthRange_inch,
                                                    y = response,
                                                    fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + .4, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "pH",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(position = "right", breaks = seq(5, 9, by = 1)) +
  coord_flip(ylim = (c(5,9)))+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_pH)


#pH H+ ion concentration standard deviation

AlleyDat_pH_Hconc_lm1<- lmer(pH_Hconc ~ GrowthCycle * SoilDepthRange_inch + 
                               (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle), 
                             data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_pH_Hconc_lm1)
anova(AlleyDat_pH_Hconc_lm1, type = "II") 

AlleyDat_pH_Hconc_emm1<- data_frame(cld(emmeans(AlleyDat_pH_Hconc_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  mutate(lower.SE = emmean-SE,
         upper.SE = emmean+SE) %>%
  mutate(lower.SE_RemNeg = if_else(lower.SE < 0, 0, lower.SE),
         lower.CL_RemNeg = if_else(lower.CL < 0 ,0, lower.CL)
  )

AlleyDat_pH_Hconc <- ggplot(AlleyDat_pH_Hconc_emm1, aes(x = SoilDepthRange_inch,
                                                        y = emmean,
                                                        fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SE_RemNeg, 
                    ymax = upper.SE),
                width = 0.3,                   
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = emmean+(mean(AlleyDat_pH_Hconc_emm1$emmean)*0.25),
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression("H"^"+"*"ion concentration"), 
       fill = "Sample Time", 
       title = "Alleyway")+
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_pH_Hconc)


# bd_g.cm3_soil and standard deviation

AlleyDat_bd_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(bd_g.cm3_soil, na.rm = TRUE),
    SD = sd(bd_g.cm3_soil, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_bd_stats)

AlleyDat_bd_g.cm3_soil_lm1<- lmer(bd_g.cm3_soil ~ GrowthCycle * SoilDepthRange_inch +
                                    (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                  data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_bd_g.cm3_soil_lm1)
anova(AlleyDat_bd_g.cm3_soil_lm1, type = "II") 

AlleyDat_bd_g.cm3_soil_cld <- data.frame(cld(emmeans(AlleyDat_bd_g.cm3_soil_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_bd_g.cm3_soil_emm1 <- dplyr::left_join(AlleyDat_bd_stats, AlleyDat_bd_g.cm3_soil_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_BD <- ggplot(AlleyDat_DepthGC_bd_g.cm3_soil_emm1, aes(x = SoilDepthRange_inch,
                                                               y = emmean, 
                                                               fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.2,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression(bold(Bulk~Density~(g/cm^3))),
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 2.0),                          
    breaks = seq(0, 2.0, by = 0.5),               
    labels = sprintf("%.1f", seq(0, 2.0, 0.5)),  
    position = "right"
  ) +
  
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_BD)


# EC and standard deviation

AlleyDat_EC_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(EC, na.rm = TRUE),
    SD = sd(EC, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_EC_stats)


AlleyDat_EC_lm1<- lmer(EC ~ GrowthCycle * SoilDepthRange_inch +
                         (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                       data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_EC_lm1)
anova(AlleyDat_EC_lm1, type = "II") 

AlleyDat_EC_cld <- data.frame(cld(emmeans(AlleyDat_EC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)

AlleyDat_DepthGC_EC_emm1 <- dplyr::left_join(AlleyDat_EC_stats, AlleyDat_EC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_EC <- ggplot(AlleyDat_DepthGC_EC_emm1, aes(x = SoilDepthRange_inch,
                                                    y = emmean, 
                                                    fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "EC (dS/m)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, 0.5),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_EC)


# CEC and standard deviation

AlleyDat_CEC_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(CEC, na.rm = TRUE),
    SD = sd(CEC, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_CEC_stats)

AlleyDat_CEC_lm1<- lmer(CEC ~ GrowthCycle * SoilDepthRange_inch +
                          (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                        data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_CEC_lm1)
anova(AlleyDat_CEC_lm1, type = "II") 

AlleyDat_CEC_cld <- data.frame(cld(emmeans(AlleyDat_CEC_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_CEC_emm1 <- dplyr::left_join(AlleyDat_CEC_stats, AlleyDat_CEC_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_CEC <- ggplot(AlleyDat_DepthGC_CEC_emm1, aes(x = SoilDepthRange_inch,
                                                      y = emmean, 
                                                      fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 1, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = expression(bold("CEC (cmol"[c]*"/kg)")),
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 14),
    breaks = seq(0, 14, 2),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_CEC)


# ACE PROTEIN and standard deviation

AlleyDat_ACE_protein_stats <- SoilHealthData_Clean_AlleyDat %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(ACE_protein, na.rm = TRUE),
    SD = sd(ACE_protein, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_ACE_protein_stats)

AlleyDat_ACE_protein_lm1<- lmer(ACE_protein ~ GrowthCycle * SoilDepthRange_inch +
                                  (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_ACE_protein_lm1)
anova(AlleyDat_ACE_protein_lm1, type = "II") 

AlleyDat_ACE_protein_cld <- data.frame(cld(emmeans(AlleyDat_ACE_protein_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_DepthGC_ACE_protein_emm1 <- dplyr::left_join(AlleyDat_ACE_protein_stats, AlleyDat_ACE_protein_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_ACE_protein <- ggplot(AlleyDat_DepthGC_ACE_protein_emm1, aes(x = SoilDepthRange_inch,
                                                                      y = emmean, 
                                                                      fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.1,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(
    values = c("#3E3D3D", "#F2F0E6"),
    labels = c("Post Plant", "Pre Plant"))+
  labs(x = "Soil Depth (cm)",
       y = "ACE Protein (g/kg)",
       fill = "Sample Time",
       title = "Alleyway")+
  scale_y_continuous(
    limits = c(0, 3),
    breaks = seq(0, 3, by = 0.5),          
    labels = ifelse(seq(0, 3, by = 0.5) %% 1 == 0,
                    seq(0, 3, by = 0.5),   
                    ""),
    position = "right"
  ) +
  coord_flip()+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(AlleyDat_ACE_protein)


# TotalPLFA.nmol and standard deviation

AlleyDat_TotalPLFA_stats <- SoilHealthData_Clean_AlleyDataPLFA %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA.nmol, na.rm = TRUE),
    SD = sd(TotalPLFA.nmol, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_TotalPLFA_stats)


AlleyDat_TotalPLFA.nmol_lm1<- lmer(TotalPLFA.nmol ~ GrowthCycle * SoilDepthRange_inch +
                                     (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                   data = SoilHealthData_Clean_AlleyDataPLFA)
residplot(AlleyDat_TotalPLFA.nmol_lm1)
anova(AlleyDat_TotalPLFA.nmol_lm1, type = "II") 

AlleyDat_TotalPLFA.nmol_cld <- data.frame(cld(emmeans(AlleyDat_TotalPLFA.nmol_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)

AlleyDat_TotalPLFA.nmol_emm1 <- dplyr::left_join(AlleyDat_TotalPLFA_stats, AlleyDat_TotalPLFA.nmol_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_TotalPLFA.nmol_plot <- ggplot(AlleyDat_TotalPLFA.nmol_emm1, aes(x = SoilDepthRange_inch,
                                                                         y = emmean, 
                                                                         fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 3,  
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = 0.9),
            fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#3E3D3D", "#F2F0E6")) +
  labs(x = "Soil Depth (inch)",
       y = "PLFA mass (nmols/g)",
       fill = "Sample Time",
       title = "alleyway") +
  scale_y_continuous(
    position = "right"
  ) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12))

print(AlleyDat_TotalPLFA.nmol_plot)


# TotalPLFA_count and standard deviation

AlleyDat_TotalPLFA_count_stats <- SoilHealthData_Clean_AlleyDataPLFA %>%
  group_by(GrowthCycle, SoilDepthRange_inch) %>%
  dplyr::summarise(
    emmean = mean(TotalPLFA_count, na.rm = TRUE),
    SD = sd(TotalPLFA_count, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_TotalPLFA_count_stats)


AlleyDat_TotalPLFA_count_lm1<- lmer(TotalPLFA_count ~ GrowthCycle * SoilDepthRange_inch +
                                      (1|fact_replicateNo) + (1|fact_replicateNo:GrowthCycle),
                                    data = SoilHealthData_Clean_AlleyDataPLFA)
residplot(AlleyDat_TotalPLFA_count_lm1)
anova(AlleyDat_TotalPLFA_count_lm1, type = "II") 

AlleyDat_TotalPLFA_count_cld <- data.frame(cld(emmeans(AlleyDat_TotalPLFA_count_lm1, ~ GrowthCycle * SoilDepthRange_inch, type = "response"), Letters = LETTERS, adjust = "none", remove.space=F)) %>%
  dplyr::select(GrowthCycle, SoilDepthRange_inch, .group)


AlleyDat_TotalPLFA_count_emm1 <- dplyr::left_join(AlleyDat_TotalPLFA_count_stats, AlleyDat_TotalPLFA_count_cld, by = c("GrowthCycle", "SoilDepthRange_inch")) %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_TotalPLFA_count_plot <- ggplot(AlleyDat_TotalPLFA_count_emm1, aes(x = SoilDepthRange_inch,
                                                                           y = emmean, 
                                                                           fill = GrowthCycle))+
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9),
           color = "black") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 3, 
                label = trimws(.group),
                group = GrowthCycle),
            size = 4,
            position = position_dodge(width = 0.9),
            fontface = "bold") +
  scale_fill_manual(values = c("#3E3D3D", "#F2F0E6"))+
  labs(x = "Soil Depth (inch)",
       y = "PLFA count (#)",
       fill = "Sample Time",
       title = "alleyway")+
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10),
    position = "right"
  ) +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))
print(AlleyDat_TotalPLFA_count_plot)

## Figure 8 ###

plots <- list(
  AlleyDat_DepthGC_totalC_percent_plot1,
  AlleyDat_TOC,
  AlleyDat_TIC,
  AlleyDat_molMnred.kg,
  AlleyDat_minC,
  AlleyDat_ACE_protein,
  AlleyDat_P,
  AlleyDat_pH,
  AlleyDat_EC,
  AlleyDat_CEC,
  AlleyDat_BD,
  AlleyDat_TotalPLFA.nmol_plot,
  AlleyDat_TotalPLFA_count_plot
)

strip_titles <- function(p) {
  p + labs(title = NULL, subtitle = NULL, tag = NULL) +
    theme(plot.title = element_blank(),
          plot.subtitle = element_blank(),
          plot.tag = element_blank())
}
plots_nt <- lapply(plots, strip_titles)

plots_nt <- lapply(plots_nt, function(p) {
  p + theme(axis.title.x = element_text(face = "bold"))
})

combined_plot_ALLEYWAY_Time <- ggarrange(
  plotlist = plots_nt,
  ncol = 3, nrow = 5,
  labels = "AUTO",
  align = "hv",
  common.legend = TRUE,
  legend = "bottom"
)

final_plot_aw <- annotate_figure(
  combined_plot_ALLEYWAY_Time,
  left = text_grob("Soil Depth (cm)", rot = 90, size = 14)
)

print(final_plot_aw)


##---------------------------------------------------
### Figure 4: POXC Treatment Effect

## TREATMENT EFFECT GRAPHS


# molMnred.kg - treatment effect UNDERVINE
## Figure 4A 
molMnred.kg_stats <- TimeDat_DepthGC_SoilHealthData %>%
  dplyr::group_by(fact_TreatmentNo) %>%
  dplyr::summarise(
    emmean = mean(molMnred.kg, na.rm = TRUE), 
    SD = sd(molMnred.kg, na.rm = TRUE), # Raw Standard Deviation
    .groups = 'drop'
  )
print(molMnred.kg_stats)

TimeDat_DepthGC_molMnred.kg_lm1<- lmer(molMnred.kg ~ fact_TreatmentNo * GrowthCycle * SoilDepthRange_inch + 
                                         (1|fact_replicateNo) + (1|fact_replicateNo:fact_TreatmentNo) + (1|fact_replicateNo:fact_TreatmentNo:GrowthCycle),
                                       data = TimeDat_DepthGC_SoilHealthData)
residplot(TimeDat_DepthGC_molMnred.kg_lm1)
anova(TimeDat_DepthGC_molMnred.kg_lm1, type = "II")


molMnred.kg_cld <- data.frame(cld(emmeans(TimeDat_DepthGC_molMnred.kg_lm1, ~ fact_TreatmentNo, type = "response"), Letters = LETTERS, adjust = "none")) %>%
  dplyr::select(fact_TreatmentNo, .group) 

TimeDat_DepthGC_molMnred.kg_emm2 <- dplyr::left_join(molMnred.kg_stats, molMnred.kg_cld, by = "fact_TreatmentNo") %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

TimeDat_DepthGC_molMnred.kg_plot2<- ggplot(TimeDat_DepthGC_molMnred.kg_emm2, aes(x = fact_TreatmentNo,
                                                                                 y = emmean, 
                                                                                 fill = fact_TreatmentNo))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black"
  )+
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD),       
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + .002,
                label = trimws(.group)),
            size = 4,
            position = position_dodge(width = .9),
            fontface = "plain") +
  scale_fill_manual(values=c("grey30", "grey50", "grey65", "grey80", "grey95"),
                    breaks = c("1","2","3","4","5"),
                    labels = c("TR1","TR2",
                               "TR3","TR4",
                               "TR5"))+
  labs(x = "Treatment",
       y = bquote("POXC (mol MnO"[4]^"-"*"/kg)"), 
       fill = "Treatment") +
  scale_y_continuous(position = "left") + 
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=12))

print(TimeDat_DepthGC_molMnred.kg_plot2)


## Figure 4B: Total carbon treatment effect

AlleyDat_totalC_percent_stats_Trt <- SoilHealthData_Clean_AlleyDat %>%
  group_by(fact_TreatmentNo) %>% 
  dplyr::summarise(
    emmean = mean(totalC_percent, na.rm = TRUE),
    SD = sd(totalC_percent, na.rm = TRUE),
    .groups = 'drop'
  )
print(AlleyDat_totalC_percent_stats_Trt)


AlleyDat_totalC_percent_lm1<- lmer(totalC_percent ~ fact_TreatmentNo * GrowthCycle * SoilDepthRange_inch +
                                     (1|fact_replicateNo) + (1|fact_replicateNo:fact_TreatmentNo) + (1|fact_replicateNo:fact_TreatmentNo:GrowthCycle), # I am not sure about this parameterization***
                                   data = SoilHealthData_Clean_AlleyDat)
residplot(AlleyDat_totalC_percent_lm1)
anova(AlleyDat_totalC_percent_lm1, type = "II") 

TimeDat_DepthGC_totalC_percent_cld2 <- data.frame(cld(emmeans(AlleyDat_totalC_percent_lm1, ~ fact_TreatmentNo, type = "response"), Letters = LETTERS, adjust = "none")) %>%
  dplyr::select(fact_TreatmentNo, .group)


TimeDat_DepthGC_totalC_percent_emm2 <- dplyr::left_join(AlleyDat_totalC_percent_stats_Trt, TimeDat_DepthGC_totalC_percent_cld2, by = "fact_TreatmentNo") %>%
  dplyr::mutate(
    lower.SD = emmean - SD,
    upper.SD = emmean + SD
  ) %>%
  dplyr::mutate(
    lower.SD_RemNeg = if_else(lower.SD < 0, 0, lower.SD)
  )

AlleyDat_DepthGC_totalC_percent_plot2 <- ggplot(TimeDat_DepthGC_totalC_percent_emm2,
                                                aes(x = fact_TreatmentNo, y = emmean, fill = fact_TreatmentNo)) + 
  geom_col(width = 0.8, color = "black") +
  geom_errorbar(aes(ymin = lower.SD_RemNeg, 
                    ymax = upper.SD), 
                width = 0.3, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = upper.SD + 0.05,  
                label = trimws(.group)),
            size = 4, position = position_dodge(width = .9), fontface = "plain") +
  scale_fill_manual(name = "Treatment",
                    values = c("grey30","grey50","grey65","grey80","grey95"),
                    breaks = c("1","2","3","4","5"),
                    labels = c("TR1","TR2","TR3","TR4","TR5")) +
  labs(x = "Treatment", y = "TC (%)") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 12))
print(AlleyDat_DepthGC_totalC_percent_plot2)


### TREATMENT EFFECT GRAPH Figure 4A & B

TimeDat_DepthGC_molMnred.kg_plot2  <- TimeDat_DepthGC_molMnred.kg_plot2 +
  theme(aspect.ratio = NULL)

AlleyDat_DepthGC_totalC_percent_plot2 <- AlleyDat_DepthGC_totalC_percent_plot2 +
  theme(aspect.ratio = NULL)

treatment_effect_plot <- ggarrange(
  TimeDat_DepthGC_molMnred.kg_plot2,
  AlleyDat_DepthGC_totalC_percent_plot2,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("A", "B"),
  label.x = 0.02,    
  label.y = 1,       
  font.label = list(size = 12, face = "bold")
)

print(treatment_effect_plot)

