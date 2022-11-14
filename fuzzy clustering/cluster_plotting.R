# Plotting fuzzy clusters and Q
library(tidyverse)
library(ggplot2)
library(cowplot)

# Read in files
high.mems <- read.csv("fuzzy clustering/high.mems.csv")

colors <- c("aquamarine4", "aquamarine3", "aquamarine1", "pink", "palevioletred2", "palevioletred4")


# Visualize boxplots
names(high.mems)
ggplot(high.mems, aes(x = Cluster, y = BareSoilCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Bare Soil Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = LargeGaps, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Gaps > 100 cm") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxPerenGrassCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("PG Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")

ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxShrubCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Shrub Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxPerenForbCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Perennial Forb Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxSucculentCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Succulent Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")

ggplot(high.mems, aes(x = Cluster, y = AH_NonNoxTreeCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Tree Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = AH_NoxCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Noxious sp. Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = TotalFoliarCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Total Foliar Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


# Q
ggplot(high.mems, aes(x = Cluster, y = horizontal_flux_total_MD, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster modal concept") + ylab ("Median horizontal flux (Q)") + ggtitle("Median horizontal flux (Q) by cluster") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


# Faceted boxplots
names(high.mems)
high.mems.tall <- tidyr::gather(high.mems, key = "Indicator", value = "Percent", 4:20)
unique(high.mems.tall$Indicator)

bp.table <- dplyr::filter(high.mems.tall, Indicator == "AH_NonNoxPerenGrassCover" |
                            Indicator == "AH_NonNoxShrubCover" |
                            Indicator == "BareSoilCover" |
                            Indicator == "LargeGaps")

bp <- ggplot(bp.table, aes(x = Cluster, y = Percent, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors)
bp + facet_grid(Indicator ~ .)



# Response curves
# Modified from Jeremy's plot script
library(RColorBrewer)
library(cowplot)

# All clusters, entire IQ range
bgplot <- ggplot() +
  geom_point(data = high.mems, aes(x = BareSoilCover,
                                        y = horizontal_flux_total_MD,
                                        color = Cluster, fill = Cluster)) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = BareSoilCover, y = horizontal_flux_total_MD),
              color="darkgray", se = F) +
  theme_bw()

bgplot


# Cluster IQs
str(high.mems.aero)
colors <- c("aquamarine4", "aquamarine3", "aquamarine1", "pink", "palevioletred2", "palevioletred4")
bg_allcluster_plot <- bgplot + geom_vline(data = high.mems %>%
                               dplyr::filter(Cluster == "C1"),
                             aes(xintercept = quantile(BareSoilCover,
                                                       probs = 0.50)),
                             color = "aquamarine4",
                             size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "aquamarine4", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "aquamarine4", size = 1, lty = 2) +
    geom_vline(data = high.mems %>%
                                      dplyr::filter(Cluster == "C2"),
                                       aes(xintercept = quantile(BareSoilCover,
                                                                 probs = 0.50)),
                                       color = "aquamarine3",
                                       size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "aquamarine3", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "aquamarine3", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
                                dplyr::filter(Cluster == "C3"),
                                      aes(xintercept = quantile(BareSoilCover,
                                 probs = 0.50)),
                                        color = "aquamarine1",
                                            size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.50)),
             color = "pink", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "pink", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "pink", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.50)),
             color = "palevioletred2", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "palevioletred2", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "palevioletred2", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.50)),
             color = "palevioletred4", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "palevioletred4", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "palevioletred4", size = 1, lty = 2) +
  scale_y_continuous(name = "Q") +
  scale_x_continuous(name = "Bare Soil (%)")


bg_allcluster_plot

# All cluster gap
lg_allcluster_plot <- bgplot + geom_vline(data = high.mems %>%
                                            dplyr::filter(Cluster == "C1"),
                                          aes(xintercept = quantile(LargeGaps,
                                                                    probs = 0.50)),
                                          color = "aquamarine4",
                                          size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "aquamarine4", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "aquamarine4", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "aquamarine3",
             size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "aquamarine3", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "aquamarine3", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "aquamarine1",
             size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "aquamarine1", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "pink", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "pink", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "pink", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "palevioletred2", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "palevioletred2", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "palevioletred2", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "palevioletred4", size = 1) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "palevioletred4", size = 1, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "palevioletred4", size = 1, lty = 2) +
  scale_y_continuous(name = "Q") +
  scale_x_continuous(name = "Gaps greater than 100 cm (%)")


lg_allcluster_plot




# High mem summary table
library(gtsummary)
library(webshot2)

table(high.mems$Cluster)
names(high.mems)
sumtable <- dplyr::select(high.mems, Cluster, AH_NonNoxPerenForbCover, AH_NonNoxPerenGrassCover,
                          AH_NonNoxShrubCover, AH_NonNoxSucculentCover, AH_NoxCover,
                          BareSoilCover, LargeGaps, TotalFoliarCover, horizontal_flux_total_MD)
sumtable <- dplyr::rename(sumtable, PerennialForbs = AH_NonNoxPerenForbCover, PerennialGrasses = AH_NonNoxPerenGrassCover,
                          Shrubs = AH_NonNoxShrubCover,  Succulents = AH_NonNoxSucculentCover,
                          NoxiousSpecies = AH_NoxCover, Q = horizontal_flux_total_MD)
sumtable <- dplyr::ungroup(sumtable)
sumtable <- sumtable %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 1)))
# IQR
sumtableIQR<- gtsummary::tbl_summary(sumtable, by = Cluster, digits = everything() ~ 1)
sumtableIQR <- gtsummary::modify_header(sumtableIQR, label = "**Indicator**")
sumtableIQR <- gtsummary::modify_spanning_header(sumtableIQR, all_stat_cols() ~ "**Cluster**")

sumtableIQR
gt::gtsave(as_gt(sumtableIQR), path = "fuzzy clustering/Figures", filename = "IQR_table.png")


# Mean, median, mode
sumtableMMM <- gtsummary::tbl_summary(sumtable, by = Cluster,
                                      statistic = list(all_continuous() ~ c("{min}, {mean}, {max}")), digits = everything() ~ 1)
sumtableMMM <- gtsummary::modify_header(sumtableMMM, label = "**Indicator**")
sumtableMMM <- gtsummary::modify_spanning_header(sumtableMMM, all_stat_cols() ~ "**Cluster**")
sumtableMMM
gt::gtsave(as_gt(sumtableMMM), path = "fuzzy clustering/Figures", filename = "MMM_table.png")
