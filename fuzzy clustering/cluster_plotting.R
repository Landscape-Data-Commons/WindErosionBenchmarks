# Plotting fuzzy clusters and Q
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gtsummary)
library(webshot2)
library(gt)


# Read in files
high.mems <- read.csv("fuzzy clustering/high.mems.csv")

colors <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
            "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


# Visualize boxplots
names(high.mems)
ggplot(high.mems, aes(x = Cluster, y = BareSoilCover, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Bare Soil Cover") +
  stat_summary(fun.y=mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = GapLg, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Gaps > 100 cm") +
  stat_summary(fun = mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = Perennial_Graminoid, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("PG Cover") +
  stat_summary(fun =mean, geom="point", shape=20, size= 3, color="red", fill="red")

ggplot(high.mems, aes(x = Cluster, y = Perennial_Shrub, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Shrub Cover") +
  stat_summary(fun =mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = Nox, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Nox Cover")
  stat_summary(fun = mean, geom="point", shape=20, size= 3, color="red", fill="red")


ggplot(high.mems, aes(x = Cluster, y = Inv, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Inv") +
  stat_summary(fun = mean, geom="point", shape=20, size= 3, color="red", fill="red")

ggplot(high.mems, aes(x = Cluster, y = Perennial_Tree, fill = Cluster)) +
  geom_boxplot() + scale_fill_manual(values = colors, name = "Cluster") +
  xlab("Cluster") + ylab("Percent") + ggtitle("Tree Cover") +
  stat_summary(fun = mean, geom="point", shape=20, size= 3, color="red", fill="red")


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
library(grid)
library(gridExtra)
# All clusters, entire IQ range
colors <- c("#88CCEE", "#CC6677",
            "#DDCC77", "#117733",
            "#332288", "#AA4499")
names(high.mems)
bgplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                        y = horizontal_flux_total_MD,
                                        color = Cluster, fill = Cluster),
             size = 0.5) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = ScaledGap, y = horizontal_flux_total_MD),
              color="darkgray", se = F) +
  theme_bw() +
  ylab(bquote("Q (g" ~m^-1 ~d^-1*")")) +
  # scale_y_continuous(trans = "pseudo_log") +
  scale_x_continuous(name = "Scaled Gap (cm)", trans = "pseudo_log")

bgplot


# Cluster IQs
bg_allcluster_plot <- bgplot + geom_vline(data = high.mems %>%
                               dplyr::filter(Cluster == "C1"),
                             aes(xintercept = quantile(ScaledGap,
                                                       probs = 0.50)),
                             color = "#88CCEE",
                             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
    geom_vline(data = high.mems %>%
                                      dplyr::filter(Cluster == "C2"),
                                       aes(xintercept = quantile(ScaledGap,
                                                                 probs = 0.50)),
                                       color = "#CC6677",
                                       size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
                                dplyr::filter(Cluster == "C3"),
                                      aes(xintercept = quantile(ScaledGap,
                                 probs = 0.50)),
                                        color = "#DDCC77",
                                            size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#117733", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#117733", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#117733", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#332288", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#332288", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#332288", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#AA4499", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#AA4499", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#AA4499", size = 0.5, lty = 2) +
  theme(legend.position = "none")

bg_allcluster_plot

ggsave("fuzzy clustering/Figures/sg_allclusters_allIQR.png")



# All clusters, entire IQ range
colors <- c("#88CCEE", "#CC6677",
            "#DDCC77", "#117733",
            "#332288", "#AA4499")
names(high.mems)
bgplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 0.5) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = ScaledGap, y = horizontal_flux_total_MD),
              color="darkgray", se = F) +
  theme_bw() +
  ylab(bquote("Q (g" ~m^-1 ~d^-1*")")) +
  # scale_y_continuous(trans = "pseudo_log") +
  scale_x_continuous(name = "Scaled Gap (cm)", trans = "pseudo_log")

bgplot


# Cluster IQs
bg_allcluster_plot <- bgplot + geom_vline(data = high.mems %>%
                                            dplyr::filter(Cluster == "C1"),
                                          aes(xintercept = quantile(ScaledGap,
                                                                    probs = 0.50)),
                                          color = "#88CCEE",
                                          size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#DDCC77",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#117733", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#117733", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C4"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#117733", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#332288", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#332288", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C5"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#332288", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#AA4499", size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#AA4499", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C6"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#AA4499", size = 0.5, lty = 2) +
  theme(legend.position = "none")

bg_allcluster_plot

ggsave("fuzzy clustering/Figures/sg_allclusters_allIQR.png")


# Legend
legplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1.5) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  theme_bw()
legplot

legend <- cowplot::get_legend(legplot)

grid.newpage()
grid.draw(legend)

# Redo with only lines for C1 and C2
bgplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 0.5) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = ScaledGap, y = horizontal_flux_total_MD),
              color = "darkgray", se = F, size = 0.5) +
  theme_bw() +
  ylab(bquote("Q (g" ~m^-1 ~d^-1*")")) +
  # scale_y_continuous(trans = "pseudo_log") +
  scale_x_continuous(name = "Scaled Gap (cm)", trans = "pseudo_log")

bgplot


# Cluster IQs
rd_allcluster_plot <- bgplot + geom_vline(data = high.mems %>%
                                            dplyr::filter(Cluster == "C1"),
                                          aes(xintercept = quantile(ScaledGap,
                                                                    probs = 0.50)),
                                          color = "#88CCEE",
                                          size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  theme(legend.position = "none")

rd_allcluster_plot

ggsave("fuzzy clustering/Figures/rd_sg_allclusters_allIQR.png")













# High mem summary table
table(high.mems$Cluster)
names(high.mems)
sumtable <- dplyr::select(high.mems, Cluster, AH_NonNoxPerenForbCover, AH_NonNoxPerenGrassCover,
                          AH_NonNoxShrubCover,
                          BareSoilCover, LargeGaps, TotalFoliarCover, ScaledGap, horizontal_flux_total_MD)
sumtable <- dplyr::rename(sumtable, "Perennial Forbs (%)" = AH_NonNoxPerenForbCover, "Perennial Grasses (%)" = AH_NonNoxPerenGrassCover,
                          "Shrubs (%)" = AH_NonNoxShrubCover, "Bare Soil (%)" = BareSoilCover,
                          "Total Foliar Cover (%)" = TotalFoliarCover, "Gaps > 100 cm (%)" = LargeGaps,
                          "Scaled Gap (cm)" = ScaledGap,
                          "Q" = horizontal_flux_total_MD)
sumtable <- dplyr::ungroup(sumtable)
sumtable <- sumtable %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 1)))
# IQR
sumtableIQR<- gtsummary::tbl_summary(sumtable, by = Cluster, digits = everything() ~ 1,
                                     label = Q ~ "Q (g m<sup>-1</sup> d<sup>-1</sup>)")
sumtableIQR <- gtsummary::modify_header(sumtableIQR, label = "**Indicator**")
sumtableIQR <- gtsummary::modify_spanning_header(sumtableIQR, all_stat_cols() ~ "**Cluster**")

sumtableIQR <- sumtableIQR %>%
  gtsummary::as_gt() %>%
  gt::fmt_markdown(columns = vars(label))

sumtableIQR


gt::gtsave(sumtableIQR, path = "fuzzy clustering/Figures", filename = "IQR_table.png")


# Mean, median, mode
sumtableMMM <- gtsummary::tbl_summary(sumtable, by = Cluster,
                                      statistic = list(all_continuous() ~ c("{min}, {mean}, {max}")), digits = everything() ~ 1)
sumtableMMM <- gtsummary::modify_header(sumtableMMM, label = "**Indicator**")
sumtableMMM <- gtsummary::modify_spanning_header(sumtableMMM, all_stat_cols() ~ "**Cluster**")
sumtableMMM
gt::gtsave(as_gt(sumtableMMM), path = "fuzzy clustering/Figures", filename = "MMM_table.png")
