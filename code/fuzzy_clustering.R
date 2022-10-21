library(tidyverse)
library(vegan)
library(cluster)
library(labdsv)
library(NbClust)

# Read in data
aero <- read.csv("data/indicators/aero.csv")
BLM_indicators <- read.csv("data/indicators/BLM_indicators.csv")
indicators <- read.csv("data/indicators/indicators.csv")

### DATA PREP
# Create reference analysis table with functional group and structure data
# Table names are offset - shift to the left
names(BLM_indicators) = names(BLM_indicators)[-1]
names(BLM_indicators)
analysis.table <- dplyr::select(BLM_indicators, 1, 25:28, 31:34, 46, 15, 19:20)
# Add row number as lookup index
analysis.table <- dplyr::mutate(analysis.table, ID = rownames(analysis.table))
names(analysis.table)
# Create table for ordination and clustering
fg.foliar <- dplyr::select(BLM_indicators, 25:28, 31:34, 46, 15, 19:20)
# Double check variables
names(fg.foliar)

### ORDINATION
# Make dissimilarity index
fg.foliar.dist <- vegan::vegdist(fg.foliar, method = "bray", binary = FALSE)
# Ordinate (PCoA) on two axes
vegPCA <- cmdscale(fg.foliar.dist, k = 2)
# Plot ordination
plot(vegPCA)
fit <- vegan::envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")


### FUZZY CLUSTERING
# Test cluster number metrics
fg.KM.cascade <- cascadeKM(fg.foliar.dist, inf.gr = 3, sup.gr = 20, iter = 100, criterion = "ssi")
plot(fg.KM.cascade, sortg = TRUE)
NbClust(fg.foliar, diss = fg.foliar.dist, distance = NULL, min.nc = 3, max.nc = 20, method = "kmeans", index = "all")
par(mfrow=c(1,1))


# Adjust fuzziness/crispness with membership exponent approaching 2 for fuzzier classification
veg.fanny <- cluster::fanny(fg.foliar.dist, k = 9, memb.exp = 1.2, maxit = 1000, keep.diss = TRUE)
# Display's Dunn's partition coefficient (low coeff = very fuzzy, near 1 = crisp)
veg.fanny$coeff
# Build a dataframe of membership values
fanny.mems <- as.data.frame(veg.fanny$membership)
fanny.mems <- fanny.mems %>%
  dplyr::mutate_if(is.numeric, round, digits = 3)
# Plot clusters in ordination space
colors <- c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
            "indianred3", "tan3", "royalblue3", "snow4")
plot(vegPCA)
stars(veg.fanny$membership, locatio = vegPCA, draw.segm = TRUE, add = TRUE, scale = FALSE, len = 0.10,
      col.segments = colors, labels = NULL)
ordihull(vegPCA, veg.fanny$clustering, col = "black")
ordispider(vegPCA, veg.fanny$clustering, col = "gray", label = T)
# Fit vectors over plot
fit <- vegan::envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")
fit
# Assign plots to clusters by top membership value
names(fanny.mems)
topmems <- fanny.mems %>%
  dplyr::mutate(ID = rownames(fanny.mems)) %>%
  tidyr::gather(Cluster, MemVal, V1:V10) %>%
  dplyr::group_by(ID) %>%
  dplyr::arrange(MemVal) %>%
  dplyr::slice(which.max(MemVal)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Cluster, MemVal)
topmems <- dplyr::left_join(topmems, analysis.table)
# This process relies on the ID column to link plots to clustering results
# QC to make sure ID's are linked properly between topmems and analysis.table
# by crossreferencing clusters, indicator values, and envfit vectors
names(topmems)
clustercheck <- topmems %>%
  dplyr::filter(Cluster == "V2" | Cluster == "V5" | Cluster == "V3" | Cluster == "V4") %>%
  dplyr::select(PrimaryKey, Cluster, BareSoilCover, AH_NonNoxPerenGrassCover, GapCover_200_plus)
# Summarize indicators within clusters
names(topmems)
topmem.summary <- topmems %>%
  dplyr::select(AH_NonNoxPerenForbCover:GapCover_200_plus, Cluster) %>%
  tidyr::gather(Indicator, MeanCover, AH_NonNoxPerenForbCover:GapCover_200_plus, factor_key = FALSE) %>%
  dplyr::group_by(Cluster, Indicator) %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::summarise_all(funs(mean))
# Sort by highest
topmem.summary <- topmem.summary %>%
  dplyr::group_by(Indicator) %>%
  dplyr::arrange(desc(MeanCover), .by_group = TRUE)
# Write to csv
write.csv(topmem.summary, "mlra48.cluster.summary.csv")

# Separate plots that have high membership values to a cluster from "fuzzy" plots
high.mems <- filter(topmems, MemVal > 0.6)
low.mems <- filter(topmems, MemVal < 0.5)
# What is the percentage of plots with high membership values vs. percentage of fuzzy plots?
(576*100)/678
# 576 are high (85%)
(101*100)/678
# 101 low (15%)
# Save to csv

###### CREATE MODAL CONCEPTS
# Summarize minima, maxima, and mean functional group covers
# for each cluster based on plots with high membership value (> 0.5)
# Fuzzy plots will be compared to these modal concepts after they are defined
# Gather means
names(high.mems)
high.means <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:GapCover_200_plus, Cluster) %>%
  gather(Indicator, MeanCover, AH_NonNoxPerenForbCover:GapCover_200_plus, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(mean))
# Gather minima
high.mins <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:GapCover_200_plus, Cluster) %>%
  gather(Indicator, MinCover, AH_NonNoxPerenForbCover:GapCover_200_plus, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(min))
# Gather maxima
high.maxs <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:GapCover_200_plus, Cluster) %>%
  gather(Indicator, MaxCover, AH_NonNoxPerenForbCover:GapCover_200_plus, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(max))
# Join summary tables
high.sums <- high.means %>%
  inner_join(high.mins) %>%
  inner_join(high.maxs) %>%
  filter(MeanCover > 0 | MinCover > 0 | MaxCover > 0) %>%
  mutate_if(is.numeric, round, digits = 2)
# Save to csv
write.csv(high.sums, "high.sums.csv")


# Visualize
names(high.mems)
ggplot(high.mems, aes(x = Cluster, y = BareSoilCover)) +
  geom_boxplot()

ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxPerenGrassCover, fill = Cluster)) +
  geom_violin() +
  scale_fill_manual(values = c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
                               "indianred3", "tan3", "royalblue3", "snow4", "tan4"))


ggplot(high.mems, aes(x = Cluster, y = GapCover_200_plus, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
                               "indianred3", "tan3", "royalblue3", "snow4", "tan4"))

ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxShrubCover, fill = Cluster)) +
  geom_violin() +
  scale_fill_manual(values = c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
                               "indianred3", "tan3", "royalblue3", "snow4", "tan4"))

ggplot(high.mems, aes(x = Cluster, y = AH_NoxCover, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
                               "indianred3", "tan3", "royalblue3", "snow4", "tan4"))


ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxTreeCover, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkseagreen4", "palevioletred1", "steelblue2", "darkseagreen1",  "lightskyblue1",
                               "indianred3", "tan3", "royalblue3", "snow4", "tan4"))
