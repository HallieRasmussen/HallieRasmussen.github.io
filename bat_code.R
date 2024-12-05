library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(easystats)
library(modelr)
library(broom)
library(patchwork)



bat.dat = read.csv("C:/Users/halli/Downloads/Website/HallieRasmussen.github.io/NPS Bat Hg_2023_Data.csv")
View(bat.dat)
skim(bat.dat)
head(bat.dat)
colnames(bat.dat)

keepers = c("CommonName","WingPunch_MeHg_ng.g_dw", "WholeBlood_THg_ng.g_ww", 
            "Sex", "CollectionDate", "NationalParkUnit")

clean.bat.dat = bat.dat %>%
  select(all_of(keepers), "WingPunch_MeHg_ng.g_dw", "WholeBlood_THg_ng.g_ww") %>%
  rename(Tissue_Methyl_Hg = WingPunch_MeHg_ng.g_dw,
         Blood_Total_Hg = WholeBlood_THg_ng.g_ww)
view(clean.bat.dat)




# Plots of mercury concentration vs location
plot.1 = clean.bat.dat %>%
  ggplot(aes(x = Tissue_Methyl_Hg, y = NationalParkUnit, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Hg Concentration',
       y = 'National Park Unit',
       title = 'Mercury concentrations in Bat Tissues vs Location') +
  theme(axis.text.y = element_text(angle = 55))
plot.1

plot.2 = clean.bat.dat %>%
  ggplot(aes(x = Blood_Total_Hg, y = NationalParkUnit, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Hg Concentration',
       y = 'National Park Unit',
       title = 'Mercury concentrations in Bat Blood vs Location') +
  theme(axis.text.y = element_text(angle = 55))
plot.2



# Plots of mercury concentration vs time
plot.3 = clean.bat.dat %>%
  ggplot(aes(x = CollectionDate, y = Tissue_Methyl_Hg, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Collection Date',
       y = 'Hg Concentration',
       title = 'Mercury concentrations in Bat Tissue vs Time')
plot.3

plot.4 = clean.bat.dat %>%
  ggplot(aes(x = CollectionDate, y = Blood_Total_Hg, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Collection Date',
       y = 'Hg Concentration',
       title = 'Mercury concentrations in Bat Blood vs Time')
plot.4



# Plots of mercury concentration vs species
plot.5 = clean.bat.dat %>%
  ggplot(aes(x = CommonName, y = Tissue_Methyl_Hg, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Common Name',
       y = 'Hg Concentration',
       title = 'Mercury concentrations in Bat Tissue vs Species') +
  theme(axis.text.x = element_text(angle = 20))
plot.5

plot.6 = clean.bat.dat %>%
  ggplot(aes(x = CommonName, y = Blood_Total_Hg, fill = Sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = 'Common Name',
       y = 'Hg Concentration',
       title = 'Mercury concentrations in Bat Blood vs Species') +
  theme(axis.text.x = element_text(angle = 20))
plot.6


# Significance analysis for tissue Hg
sd(clean.bat.dat$Tissue_Methyl_Hg, na.rm = TRUE)
var(clean.bat.dat$Tissue_Methyl_Hg, na.rm = TRUE)
t.test(clean.bat.dat$Tissue_Methyl_Hg)

# Significance analysis for blood Hg
sd(clean.bat.dat$Blood_Total_Hg, na.rm = TRUE)
var(clean.bat.dat$Blood_Total_Hg, na.rm = TRUE)
t.test(clean.bat.dat$Blood_Total_Hg)


summary(clean.bat.dat)

t.test(cm ~ Sex, data = dat.1)







