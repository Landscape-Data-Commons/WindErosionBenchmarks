# Quantile approach to setting indicator benchmarks
library(tidyverse)
library(ggplot2)

# Read in data
aero <- read.csv("data/indicators/aero.csv")
BLM_indicators <- read.csv("data/indicators/BLM_indicators.csv")
indicators <- read.csv("data/indicators/indicators.csv")

names(indicators)

indicators <- dplyr::select(indicators, -X)

# Make historgram to look at shape of distribution
ggplot2::ggplot(indicators, aes(BareSoilCover)) + geom_histogram()

# Find quantiles
quantile(indicators$BareSoilCover, 0.75)
# 48% is the 75th quantile
# 75% of plots have less than or equal to 48% bare ground
quantile(indicators$BareSoilCover, c(0.25, 0.5, 0.75))
quantile(analysis.table$LargeGaps, c(0.25, 0.5, 0.75))
quantile(analysis.table$TotalFoliarCover, c(0.25, 0.5, 0.75))


# 25% of plots have less than 9% bare ground, 50% of plots have less than 28%
# bare ground, and 75% of plots have less than 45% bare ground
summary(indicators$BareSoilCover)


# Find quantiles by group (ecological site)
# Build dataframe with ecological site included
# Table names are offset - shift to the left
names(BLM_indicators) = names(BLM_indicators)[-1]
names(BLM_indicators)
esplots <- dplyr::select(BLM_indicators, PrimaryKey, EcologicalSiteId, EcoSiteId_Stripped)
indicators <- dplyr::left_join(indicators, esplots)
# Find quantiles by site
baresoilquant <- indicators %>%
  group_by(EcologicalSiteId) %>%
  summarise(count = n(),
            quantile25 = quantile(BareSoilCover, probs = 0.25))

baresoilquant <- indicators %>%
  group_by(EcoSiteId_Stripped) %>%
  summarise(count = n(),
            quantile25 = quantile(BareSoilCover, probs = 0.25))


totalfoliarquant <- indicators %>%
  group_by(EcologicalSiteId) %>%
  summarise(count = n(),
            quantile75 = quantile(TotalFoliarCover, probs = 0.75))

gaps100quant <- indicators %>%
  group_by(EcologicalSiteId) %>%
  summarise(count = n(),
            quantile75 = quantile(GapCover_101_200, probs = 0.25))
