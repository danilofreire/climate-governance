###################################################################
## Replication code for:                                         ##
## "Institutional Design and Elite Support for Climate Policies: ##
## Evidence from Latin American Countries"                       ##
## Danilo Freire, Umberto Mignozzetti, and David Skarbek         ##
## Forthcoming, Journal of Experimental Political Science        ##
## May 2020                                                      ##
###################################################################

# The R code below replicates the analyses included in the main paper.
# The same code is also available in the Supplementary Material, which
# contains additional estimations and the reporting standards
# recommended by the APSA Organized Section on Experimental Research.

# For any questions regarding the replication data or files, please
# contact Danilo Freire (danilofreire@gmail.com) or Umberto Mignozzetti
# (umberto.mig@fgv.br).


## Load packages and data

# Install and load necessary packages
packages <- c("plm", "tidyverse", "clusterSEs",
              "sp", "cregg", "cjoint")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(plm); library(tidyverse)
library(clusterSEs); library(sp)
library(cregg); library(cjoint)

# Load dataset - the object name is cj
load("freire-mignozzetti-skarbek.RData")


## Descriptive statistics

# Select variables
aux <- cj %>%
  select(Response.ID, countryOrigin, groupOrigin,
         LocationLongitude, LocationLatitude) %>%
  unique()

# Country
tab <- data.frame(table(aux$countryOrigin))
names(tab) <- c("Country", "Frequency")
p <- ggplot(data = tab, aes(x = Country, y = Frequency)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black") +
  theme_bw() + annotate(geom = "text", label = tab$Frequency,
                        x = tab$Country, y = tab$Frequency + 2)
p

# Elite type
tab <- data.frame(table(aux$groupOrigin))
names(tab) <- c("Group Origin", "Frequency")
p <- ggplot(data = tab, aes(x = `Group Origin`, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + annotate(geom = "text", label = tab$Frequency,
                        x = tab$`Group Origin`, y = tab$Frequency + 5)
p

# Frequency of features Selected by each attribute
aux <- cj %>%
  select(Response.ID,
         `Who makes the rules?`,
         `How are conflicts resolved?`,
         `What punishments do they use?`,
         `How are repeated violations punished?`,
         `How are costs distributed?`,
         `How often will the agreement be renegotiated?`) %>%
  unique()

# Who makes the rules
tab <- data.frame(table(aux$`Who makes the rules?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("Who makes the rules?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 35) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# How are conflicts resolved?
tab <- data.frame(table(aux$`How are conflicts resolved?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("How are conflicts resolved?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 35) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# What punishments do they use?
tab <- data.frame(table(aux$`What punishments do they use?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("What punishments do they use?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 40) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# How are repeated violations punished?
tab <- data.frame(table(aux$`How are repeated violations punished?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("How are repeated violations punished?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# How are costs distributed?
tab <- data.frame(table(aux$`How are costs distributed?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("How are costs distributed?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# How often will the agreement be renegotiated?
tab <- data.frame(table(aux$`How often will the agreement be renegotiated?`))
names(tab) <- c("Levels", "Frequency")
p <- ggplot(data = tab, aes(x = Levels, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_bw() + xlab("How often will the agreement be renegotiated?") +
  annotate(geom = "text", label = tab$Frequency,
           x = tab$Levels, y = tab$Frequency + 50) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p


## Analyses

# Figure 2 - Relative preferences for institutional
# attributes in climate change agreements in 10
# Latin American countries (pooled data, marginal means)

# Model formula
fm <- selected ~ `Who makes the rules?` +
  `How are conflicts resolved?` +
  `What punishments do they use?` +
  `How are repeated violations punished?` +
  `How are costs distributed?` +
  `How often will the agreement be renegotiated?`

# Plot
mms <- mm(cj, fm, id = ~Response.ID, alpha = .1, h0 = 0.5)

my_faces <- c(rep("plain", 5), "bold",
              rep("plain", 4), "bold",
              rep("plain", 3), "bold",
              rep("plain", 4), "bold",
              rep("plain", 5), "bold",
              rep("plain", 5), "bold")

p <- plot(mms, vline = 0.5, header_fmt = "%s", size = 2) + ggplot2::theme(
    legend.position = "none",
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = element_text(face = my_faces, size = 11)) +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower",
                                                xmax = "upper"),
                            size = 1, height = 0, na.rm = TRUE,
                            position = ggstance::position_dodgev(height = 1))
p

# Figure 3 - Relative preferences for institutional
# attributes in climate change agreements by
# country (marginal means)

# Calculating marginal means

# Argentina
arg <- mm(subset(cj, countryOrigin == "Argentina"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Bolivia
bol <- mm(subset(cj, countryOrigin == "Bolivia"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Brazil
bra <- mm(subset(cj, countryOrigin == "Brazil"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Chile
chi <- mm(subset(cj, countryOrigin == "Chile"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Colombia
col <- mm(subset(cj, countryOrigin == "Colombia"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Costa Rica
cri <- mm(subset(cj, countryOrigin == "Costa Rica"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Ecuador
ecu <- mm(subset(cj, countryOrigin == "Ecuador"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Mexico
mex <- mm(subset(cj, countryOrigin == "Mexico"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Panama
pan <- mm(subset(cj, countryOrigin == "Panama"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Peru
per <- mm(subset(cj, countryOrigin == "Peru"),
        fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Changing country labels
arg$country <- "Argentina"
bol$country <- "Bolivia"
bra$country <- "Brazil"
chi$country <- "Chile"
col$country <- "Colombia"
cri$country <- "Costa Rica"
ecu$country <- "Ecuador"
mex$country <- "Mexico"
pan$country <- "Panama"
per$country <- "Peru"

# Plot by country
p <- plot(rbind(arg, bol, bra, chi, col,
              cri, ecu, mex, pan, per),
        group = "country", vline = 0.5, nr = 10,
        header_fmt = "%s", size = 2) +
  facet_wrap(~ country, ncol = 5) +
  ggplot2::theme(
    axis.text.y = element_text(face = my_faces, size = 11)) +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower",
                                                xmax = "upper"),
                            size = 1, height = 0, na.rm = TRUE,
                            position = ggstance::position_dodgev(height = 1))

p

# Figure 4 - Relative preferences for institutional
# attributes in climate change agreements by
# elite type (marginal means)

# Table for the Executive branch
res1 <- mm(subset(cj, groupOrigin == "Executive"),
           fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Legislative branch
res2 <- mm(subset(cj, groupOrigin == "Legislative"),
           fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Civil society
res3 <- mm(subset(cj, groupOrigin == "Civil Society"),
           fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Academia
res4 <- mm(subset(cj, groupOrigin == "Academia"),
           fm, id = ~Response.ID, alpha = 0.1, h0 = 0.5)

# Changing labels
res1$member_type <- "Executive"
res2$member_type <- "Legislative"
res3$member_type <- "Civil Society"
res4$member_type <- "Academia"

# Plot by elite type
p <- plot(rbind(res1, res2, res3, res4),
     group = "member_type", vline = 0.5, nr = 4,
        header_fmt = "%s", size = 2) +
  facet_wrap(~ member_type, ncol = 4) +
  ggplot2::theme(
    axis.text.y = element_text(face = my_faces, size = 11)) +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower",
                                                xmax = "upper"),
                            size = 1, height = 0, na.rm = TRUE,
                            position = ggstance::position_dodgev(height = 1))
p

## Session information
r$> sessionInfo()
R version 4.0.0 (2020-04-24)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versi
ons/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versi
ons/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_
US.UTF-8/en_US.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices
[5] utils     datasets  methods   base

other attached packages:
 [1] cjoint_2.1.0     survey_4.0
 [3] Matrix_1.2-18    cregg_0.3.0
 [5] sp_1.4-1         clusterSEs_2.6.2
 [7] Formula_1.2-3    AER_1.2-9
 [9] survival_3.1-12  sandwich_2.5-1
[11] lmtest_0.9-37    zoo_1.8-7
[13] car_3.0-7        carData_3.0-3
[15] forcats_0.5.0    stringr_1.4.0
[17] dplyr_0.8.5      purrr_0.3.4
[19] readr_1.3.1      tidyr_1.0.2
[21] tibble_3.0.1     ggplot2_3.3.0
[23] tidyverse_1.3.0  plm_2.2-3
[25] nvimcom_0.9-88

loaded via a namespace (and not attached):
 [1] nlme_3.1-147      fs_1.4.1
 [3] lubridate_1.7.8   httr_1.4.1
 [5] tools_4.0.0       backports_1.1.6
 [7] R6_2.4.1          DBI_1.1.0
 [9] colorspace_1.4-1  withr_2.2.0
[11] tidyselect_1.0.0  curl_4.3
[13] compiler_4.0.0    cli_2.0.2
[15] rvest_0.3.5       mlogit_1.0-3.1
[17] xml2_1.3.2        scales_1.1.0
[19] digest_0.6.25     foreign_0.8-78
[21] rio_0.5.16        pkgconfig_2.0.3
[23] htmltools_0.4.0   bibtex_0.4.2.2
[25] fastmap_1.0.1     dbplyr_1.4.3
[27] rlang_0.4.5       readxl_1.3.1
[29] rstudioapi_0.11   shiny_1.4.0.2
[31] generics_0.0.2    jsonlite_1.6.1
[33] zip_2.0.4         magrittr_1.5
[35] Rcpp_1.0.4.6      munsell_0.5.0
[37] fansi_0.4.1       abind_1.4-5
[39] lifecycle_0.2.0   stringi_1.4.6
[41] gbRd_0.4-11       MASS_7.3-51.5
[43] ggstance_0.3.4    promises_1.1.0
[45] bdsmatrix_1.3-4   crayon_1.3.4
[47] lattice_0.20-41   haven_2.2.0
[49] splines_4.0.0     hms_0.5.3
[51] pillar_1.4.3      reprex_0.3.0
[53] glue_1.4.0        mitools_2.4
[55] data.table_1.12.8 modelr_0.1.6
[57] vctrs_0.2.4       httpuv_1.5.2
[59] Rdpack_0.11-1     miscTools_0.6-26
[61] cellranger_1.1.0  gtable_0.3.0
[63] assertthat_0.2.1  openxlsx_4.1.4
[65] mime_0.9          xtable_1.8-4
[67] broom_0.5.6       later_1.0.0
[69] maxLik_1.3-8      statmod_1.4.34
[71] ellipsis_0.3.0
