# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 5 in Analysis 2
# Code also for: Analysis 2 Robustness Checks in Table S7 and Figures S2 and S3 (in SM D)
# Data files needed:
# "ISSP 2019 SI with additional (total of 34) countries (updated April 17 2024).sav"

# Read in data file/import dataset from SPSS data file
# Data file is: "ISSP 2019 SI with additional (total of 34) countries (updated April 17 2024).sav"

# rm(temp_df_issp_all)
temp_df_issp_all <- ISSP_2019_SI_with_additional_total_of_34_countries_updated_April_17_2024_

# recode variables

# binary variable construction of forced choice question

temp_df_issp_all$respineq <- temp_df_issp_all$v25
table(temp_df_issp_all$respineq)
table(temp_df_issp_all$v25, temp_df_issp_all$respineq)
temp_df_issp_all$respineq <- ifelse( (temp_df_issp_all$respineq>=1 & temp_df_issp_all$respineq<=6), temp_df_issp_all$respineq, NA)
table(temp_df_issp_all$respineq)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_bus <- ifelse( (temp_df_issp_all$respineq>=2 & temp_df_issp_all$respineq<=6), 0, temp_df_issp_all$respineq )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_bus)
summary(temp_df_issp_all$respineq_bus)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_gov <- ifelse( (temp_df_issp_all$respineq %in% c(1,3,4,5,6)), 0, temp_df_issp_all$respineq )
temp_df_issp_all$respineq_gov <- ifelse( (temp_df_issp_all$respineq_gov == 2), 1, temp_df_issp_all$respineq_gov )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_gov)
summary(temp_df_issp_all$respineq_gov)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_none <- ifelse( (temp_df_issp_all$respineq>=1 & temp_df_issp_all$respineq<=5), 0, temp_df_issp_all$respineq )
temp_df_issp_all$respineq_none <- ifelse( (temp_df_issp_all$respineq_none == 6), 1, temp_df_issp_all$respineq_none )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_none)
summary(temp_df_issp_all$respineq_none)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_low <- ifelse( (temp_df_issp_all$respineq %in% c(1,2,3,4,6)), 0, temp_df_issp_all$respineq )
temp_df_issp_all$respineq_low <- ifelse( (temp_df_issp_all$respineq_low == 5), 1, temp_df_issp_all$respineq_low )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_low)
summary(temp_df_issp_all$respineq_low)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_high <- ifelse( (temp_df_issp_all$respineq %in% c(1,2,3,5,6)), 0, temp_df_issp_all$respineq )
temp_df_issp_all$respineq_high <- ifelse( (temp_df_issp_all$respineq_high == 4), 1, temp_df_issp_all$respineq_high )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_high)
summary(temp_df_issp_all$respineq_high)
summary(temp_df_issp_all$respineq)

temp_df_issp_all$respineq_union <- ifelse( (temp_df_issp_all$respineq %in% c(1,2,4,5,6)), 0, temp_df_issp_all$respineq )
temp_df_issp_all$respineq_union <- ifelse( (temp_df_issp_all$respineq_union == 3), 1, temp_df_issp_all$respineq_union )
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_union)
summary(temp_df_issp_all$respineq_union)
summary(temp_df_issp_all$respineq)

# descriptives by country

by(temp_df_issp_all$respineq_gov, temp_df_issp_all$country, summary)

# weight variable

temp_df_issp_all$weight <- temp_df_issp_all$WEIGHT

# country codes and labels in separate data frame
# k = index for number of countries

countrycodes <- attributes(temp_df_issp_all$country)
length(countrycodes$labels)
k <- NULL
k <- length(countrycodes$labels)
k

# function to create data frame of country means
# for each forced choice option

rm(create_country_means)
create_country_means <- function(chart_data,chart_var0, weight_var0) {

  chart_var <- enquo(chart_var0)
  weight_var <- enquo(weight_var0)
  
  # store country means in new data frame
  country_means <<- data.frame(varlabel=character(), wtdmean=double())
  print.data.frame(country_means)

  for (i in 1:k) {
    country_means[i,1] <- countrycodes$labels[i]
  }
  print.data.frame(country_means)

  # regular weighted mean in original scale (including binary vars)
  country_means <- chart_data %>%
    filter(!is.na(!!chart_var)) %>%
    group_by(country) %>%
    summarise(wtdmean = weighted.mean(!!chart_var,!!weight_var))
  print.data.frame(country_means)
  
  return(country_means)
}

# call the function here for each forced choice option

#rm(country_means_bus_bin,country_means_gov_bin,country_means_high_bin,country_means_low_bin,country_means_union_bin,country_means_none_bin)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_bus, weight_var0=weight)
print(country_means, n=35)
country_means_bin_bus <- country_means
print(country_means_bin_bus, n=35)
country_means_old <- country_means 
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_gov, weight_var0=weight)
print(country_means, n=35)
country_means_bin_gov <- country_means
print(country_means_bin_gov, n=35)
country_means_old <- country_means 
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_union, weight_var0=weight)
print(country_means, n=35)
country_means_bin_union <- country_means
print(country_means_bin_union, n=35)
country_means_old <- country_means 
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_high, weight_var0=weight)
print(country_means, n=35)
country_means_bin_high <- country_means
print(country_means_bin_high, n=35)
country_means_old <- country_means 
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_low, weight_var0=weight)
print(country_means, n=35)
country_means_bin_low <- country_means
print(country_means_bin_low, n=35)
country_means_old <- country_means 
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=respineq_none, weight_var0=weight)
print(country_means, n=35)
country_means_bin_none <- country_means
print(country_means_bin_none, n=35)
country_means_old <- country_means 
rm(country_means)

# check that the total across forced choice binary proportions is 1.0
# 2/22/24, this checks out
for (i in 1:k) {
  total_prop <- country_means_bin_bus[i,2] + country_means_bin_gov[i,2] + 
    country_means_bin_union[i,2] + country_means_bin_high[i,2] + 
    country_means_bin_low[i,2] + country_means_bin_none[i,2]
  print(total_prop)
}

# make unique var names for wtdmean
country_means_bin_bus <- rename(country_means_bin_bus, fc_bus = wtdmean)
country_means_bin_gov <- rename(country_means_bin_gov, fc_gov = wtdmean)
country_means_bin_union <- rename(country_means_bin_union, fc_union = wtdmean)
country_means_bin_high <- rename(country_means_bin_high, fc_high = wtdmean)
country_means_bin_low <- rename(country_means_bin_low, fc_low = wtdmean)
country_means_bin_none <- rename(country_means_bin_none, fc_none = wtdmean)

# column bind together
updated_comparative_lineplots <- cbind(country_means_bin_gov,
                                       country_means_bin_bus,
                                       country_means_bin_union,
                                       country_means_bin_high,
                                       country_means_bin_low,
                                       country_means_bin_none)

# check countries align
for (i in 1:k) {
  country_num <- (updated_comparative_lineplots[i,1] + updated_comparative_lineplots[i,3] + 
    updated_comparative_lineplots[i,5] + updated_comparative_lineplots[i,7] + 
    updated_comparative_lineplots[i,9] + updated_comparative_lineplots[i,11])/6
  test_country_num <- ifelse(country_num == updated_comparative_lineplots[i,1], 1, 0)
  print(test_country_num)
}

# remove extra country columns
updated_comparative_lineplots_2 <- updated_comparative_lineplots[,-c(3,5,7,9,11)]
updated_comparative_lineplots_2

# get labels for graphs; must fix Ukraine label
updated_comparative_lineplots_2$varlabel <- get_labels(updated_comparative_lineplots_2$country)
print.data.frame(updated_comparative_lineplots_2)
updated_comparative_lineplots_2$varlabel <- ifelse(updated_comparative_lineplots_2$varlabel=="Ukraine",
                                                   "UA-Ukraine",updated_comparative_lineplots_2$varlabel)
print.data.frame(updated_comparative_lineplots_2)
updated_comparative_lineplots_2$varlabel <- updated_comparative_lineplots_2$varlabel %>% stringr::str_remove(pattern = "-.*")
print.data.frame(updated_comparative_lineplots_2)

# collapse 6 into 3 categories
updated_comparative_lineplots_2$fc_busplus = updated_comparative_lineplots_2$fc_bus +
  updated_comparative_lineplots_2$fc_union + updated_comparative_lineplots_2$fc_high
updated_comparative_lineplots_2$fc_lowplus = updated_comparative_lineplots_2$fc_low +
  updated_comparative_lineplots_2$fc_none 
updated_comparative_lineplots_2

# add new columns for IVs 

gdp_df <- temp_df_issp_all %>%
  group_by(country) %>%
  summarise(n = n(), gdpcapita = mean(gdp, na.rm=T))
print.data.frame(gdp_df)

mf_df <- temp_df_issp_all %>%
  group_by(country) %>%
  summarise(n = n(), index_comp = mean(marketfreedom, na.rm=T))
print.data.frame(mf_df)

gexp_df <- temp_df_issp_all %>%
  group_by(country) %>%
  summarise(n = n(), govexp = mean(govexp, na.rm=T))
print.data.frame(gexp_df)

updated_comparative_lineplots_3 <- cbind(updated_comparative_lineplots_2, gdp_df, mf_df, gexp_df)
updated_comparative_lineplots_3

# check countries align
for (i in 1:k) {
  country_num <- (updated_comparative_lineplots_3[i,11] + updated_comparative_lineplots_3[i,14] + 
                    updated_comparative_lineplots_3[i,17])/3
  test_country_num <- ifelse(country_num == updated_comparative_lineplots_3[i,1], 1, 0)
  print(test_country_num)
}

# remove extra country columns
updated_comparative_lineplots_4 <- updated_comparative_lineplots_3[,-c(11,12,14,15,17,18)]
updated_comparative_lineplots_4

# Article Figure 5
# with vector-based high-res image and other journal specs for publication

# setup
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

rm(articlefigure5)
articlefigure5 <- 
  ggplot(updated_comparative_lineplots_4) + 
  geom_point(aes(y=fc_gov, x=index_comp), color="royalblue4", fill="royalblue4", size=.50) +
  geom_text(aes(y=fc_gov, x=index_comp, label=varlabel), fontface=2, colour = "royalblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_gov, x=index_comp), method='lm_robust', na.rm=TRUE, se=TRUE, color="royalblue4", fill="royalblue4") +
  geom_point(aes(y=fc_busplus, x=index_comp), color="red2", fill="red2", size=.50) +
  geom_text(aes(y=fc_busplus, x=index_comp, label=varlabel), fontface=2, colour = "red2", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_busplus, x=index_comp), method='lm_robust', na.rm=TRUE, se=TRUE, color="red2", fill="red2") +
  geom_point(aes(y=fc_lowplus, x=index_comp), color="lightsteelblue2", fill="lightsteelblue2", size=.50) +
  geom_text(aes(y=fc_lowplus, x=index_comp, label=varlabel), fontface=2, colour = "lightsteelblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_lowplus, x=index_comp), method='lm_robust', na.rm=TRUE, se=TRUE, color="lightsteelblue4", fill="lightsteelblue1") +
  coord_cartesian(xlim=c(-2.5,2.0), ylim=c(0,1.0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 7), 
        axis.title.y = element_text(size = 8), 
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(y="Proportion", x="Marketization Index")

# save to file
# 1 column width, no scale parameter needed 
articlefigure5
ggsave("ArticleFigure5.pdf", plot = articlefigure5, width = 3.55, height = 3.25, units = "in", dpi = 300)

######## Analysis 2 Robustness Checks in SM D

# Figure S2 in SM D
  
# construct graphs for only advanced capitalist/rich countries
updated_comparative_lineplots_4
updated_comparative_lineplots_4_rich <- subset(updated_comparative_lineplots_4, country %in% c(36,554,826,840,208,246,352,578,752,40,250,276,380,756,392))

# note can use method='lm_robust' or method='lmrob'
rm(panelUS, panelnonUS)
panelUS <- ggplot(updated_comparative_lineplots_4_rich) + 
  geom_point(aes(y=fc_gov, x=index_comp), color="royalblue4", fill="royalblue4", size=.50) +
  geom_text(aes(y=fc_gov, x=index_comp, label=varlabel), fontface=2, colour = "royalblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_gov, x=index_comp), method='lmrob', na.rm=TRUE, se=TRUE, color="royalblue4", fill="royalblue4") +
  geom_point(aes(y=fc_busplus, x=index_comp), color="red2", fill="red2", size=.50) +
  geom_text(aes(y=fc_busplus, x=index_comp, label=varlabel), fontface=2, colour = "red2", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_busplus, x=index_comp), method='lmrob', na.rm=TRUE, se=TRUE, color="red2", fill="red2") +
  geom_point(aes(y=fc_lowplus, x=index_comp), color="lightsteelblue2", fill="lightsteelblue2", size=.50) +
  geom_text(aes(y=fc_lowplus, x=index_comp, label=varlabel), fontface=2, colour = "lightsteelblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_lowplus, x=index_comp), method='lmrob', na.rm=TRUE, se=TRUE, color="lightsteelblue4", fill="lightsteelblue2") +
  coord_cartesian(xlim=c(-2.5,2.0), ylim=c(0,1.0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 7), 
        axis.title.y = element_text(size = 8), 
        plot.title = element_text(hjust = 0, size=9,face="bold"),
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(title="A", y="Proportion", x="Marketization index")

panelnonUS <- ggplot(updated_comparative_lineplots_4_rich) + 
  geom_point(aes(y=fc_gov, x=gdpcapita), color="royalblue4", fill="royalblue4", size=.50) +
  geom_text(aes(y=fc_gov, x=gdpcapita, label=varlabel), fontface=2, colour = "royalblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_gov, x=gdpcapita), method='lmrob', na.rm=TRUE, se=TRUE, color="royalblue4", fill="royalblue4") +
  geom_point(aes(y=fc_busplus, x=gdpcapita), color="red2", fill="red2", size=.50) +
  geom_text(aes(y=fc_busplus, x=gdpcapita, label=varlabel), fontface=2, colour = "red2", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_busplus, x=gdpcapita), method='lmrob', na.rm=TRUE, se=TRUE, color="red2", fill="red2") +
  geom_point(aes(y=fc_lowplus, x=gdpcapita), color="lightsteelblue2", fill="lightsteelblue2", size=.50) +
  geom_text(aes(y=fc_lowplus, x=gdpcapita, label=varlabel), fontface=2, colour = "lightsteelblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_lowplus, x=gdpcapita), method='lmrob', na.rm=TRUE, se=TRUE, color="lightsteelblue4", fill="lightsteelblue2") +
  coord_cartesian(xlim=c(0,80000), ylim=c(0,1.0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 7), 
        axis.title.y = element_text(size = 8), 
        plot.title = element_text(hjust = 0, size=9, face="bold"),
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(title="B", y="", x="GDP per capita")

# expanded plot window to get to 1008 x 576, and then expanded width in document
#rm(articlefigureS2)
articlefigureS2 <- panelUS + panelnonUS

# save to file
# 1 column width, no scale parameter needed 
articlefigureS2
ggsave("ArticleFigureS2.pdf", plot = articlefigureS2, width = 5.50, height = 2.75, units = "in", dpi = 300)

# Figure S3 in SM D
# remove Venezuela as an outlier (as it is removed in Fig 5 because it has an NA value for the marketization index)
# this will also affect Table S7 as intended (see line 445)
updated_comparative_lineplots_4[34,11] <- NA
rm(articlefigureS3)
articlefigureS3 <- 
  ggplot(updated_comparative_lineplots_4) + 
  geom_point(aes(y=fc_gov, x=gdpcapita), color="royalblue4", fill="royalblue4", size=.50) +
  geom_text(aes(y=fc_gov, x=gdpcapita, label=varlabel), fontface=2, colour = "royalblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_gov, x=gdpcapita), method='lm_robust', na.rm=TRUE, se=TRUE, color="royalblue4", fill="royalblue4") +
  geom_point(aes(y=fc_busplus, x=gdpcapita), color="red2", fill="red2", size=.50) +
  geom_text(aes(y=fc_busplus, x=gdpcapita, label=varlabel), fontface=2, colour = "red2", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_busplus, x=gdpcapita), method='lm_robust', na.rm=TRUE, se=TRUE, color="red2", fill="red2") +
  geom_point(aes(y=fc_lowplus, x=gdpcapita), color="lightsteelblue2", fill="lightsteelblue2", size=.50) +
  geom_text(aes(y=fc_lowplus, x=gdpcapita, label=varlabel), fontface=2, colour = "lightsteelblue4", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=fc_lowplus, x=gdpcapita), method='lm_robust', na.rm=TRUE, se=TRUE, color="lightsteelblue4", fill="lightsteelblue2") +
  coord_cartesian(xlim=c(0,80000), ylim=c(0,1.0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 7), 
        axis.title.y = element_text(size = 8), 
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(title="", subtitle="", y="Proportion", x="GDP per capita")

# save to file
# 1 column width, no scale parameter needed 
articlefigureS3
ggsave("ArticleFigureS3.pdf", plot = articlefigureS3, width = 3.55, height = 3.25, units = "in", dpi = 300)

#### analyses and robustness checks for Analysis 2

## Table S7 in SM D is constructed from code below (see line 445)

## rich countries only 
# bivariate 

options(scipen=100)
summary(lm_robust(data=updated_comparative_lineplots_4_rich, fc_lowplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich, fc_gov ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich, fc_bus ~ index_comp))

# test outliers
# 250=France; 380=Italy

#rm(updated_comparative_lineplots_4_rich_dropFR,
#   updated_comparative_lineplots_4_rich_dropIT,
#   updated_comparative_lineplots_4_rich_dropFRIT)
updated_comparative_lineplots_4_rich_dropFR <- updated_comparative_lineplots_4_rich %>%
  slice(-5)
updated_comparative_lineplots_4_rich_dropIT <- updated_comparative_lineplots_4_rich %>%
  slice(-8)
updated_comparative_lineplots_4_rich_dropFRIT <- updated_comparative_lineplots_4_rich %>%
  slice(-c(5,8))

#rm(outliers_all,outliers_all_rich,outliers_FR,outliers_IT,outliers_FRIT)
outliers_all_rich <- lm(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp)
outliers_FR <- lm(data=updated_comparative_lineplots_4_rich_dropFR, fc_busplus ~ index_comp)
outliers_IT <- lm(data=updated_comparative_lineplots_4_rich_dropIT, fc_busplus ~ index_comp)
outliers_FRIT <- lm(data=updated_comparative_lineplots_4_rich_dropFRIT, fc_busplus ~ index_comp)

# only removing FR results in no remaining outliers (cook's d > 1)
plot(outliers_all_rich)
plot(outliers_FR)
plot(outliers_IT)
plot(outliers_FRIT)

# no outliers with all countries, rich and non-rich combined
outliers_all <- lm(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp)
plot(outliers_all)

# using robust se; effect driven by private companies option
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_busplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_bus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_union ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_high ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_gov ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_lowplus ~ index_comp))

# use lmrob instead for robust se and outlier identification (using robustbase package)
# also identifies only France as an outlier
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_bus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_gov ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_lowplus ~ index_comp))

# try all data and subsetted data with varying outlier/robust se's for busplus and bus
# all result in statistically significant effect
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_busplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp))

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_bus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4_rich_dropFR, fc_bus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_bus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_bus ~ index_comp))

#######################################################################################
## final results for all and rich countries only
# Table S7 in SM D and the outlier procedures in SM D
# lmrob() is preferred because of outlier correction but consistent with lmrobust() reported below
# Table S7 construction is by hand 

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_gov ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_gov ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_gov ~ gdpcapita))
summary(lmrob(data=updated_comparative_lineplots_4, fc_gov ~ gdpcapita))

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_busplus ~ gdpcapita))
summary(lmrob(data=updated_comparative_lineplots_4, fc_busplus ~ gdpcapita))

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_bus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_bus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_bus ~ gdpcapita))
summary(lmrob(data=updated_comparative_lineplots_4, fc_bus ~ gdpcapita))

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_lowplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4, fc_lowplus ~ index_comp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_lowplus ~ gdpcapita))
summary(lmrob(data=updated_comparative_lineplots_4, fc_lowplus ~ gdpcapita))
#######################################################################################

# note intercorrelations and add controls

cor.test(updated_comparative_lineplots_4_rich_dropFR$index_comp, updated_comparative_lineplots_4_rich_dropFR$gdpcapita)
cor.test(updated_comparative_lineplots_4_rich_dropFR$index_comp, updated_comparative_lineplots_4_rich_dropFR$govexp)
cor.test(updated_comparative_lineplots_4_rich_dropFR$govexp, updated_comparative_lineplots_4_rich_dropFR$gdpcapita)

summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_busplus ~ index_comp + gdpcapita + govexp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_bus ~ index_comp + gdpcapita + govexp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_gov ~ index_comp + gdpcapita + govexp))
summary(lmrob(data=updated_comparative_lineplots_4_rich, fc_lowplus ~ index_comp + gdpcapita + govexp))

## all countries

# bivariate

sd(updated_comparative_lineplots_4$fc_busplus, na.rm=T)
sd(updated_comparative_lineplots_4$fc_gov, na.rm=T)
sd(updated_comparative_lineplots_4$fc_lowplus, na.rm=T)

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ index_comp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ index_comp))

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ gdpcapita))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ gdpcapita))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ gdpcapita))

# combined IVs

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ index_comp + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ index_comp + govexp))

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ gdpcapita + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ gdpcapita + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ gdpcapita + govexp))

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp + gdpcapita))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ index_comp + gdpcapita))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ index_comp + gdpcapita))

summary(lm_robust(data=updated_comparative_lineplots_4, fc_busplus ~ index_comp + gdpcapita + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ index_comp + gdpcapita + govexp))
summary(lm_robust(data=updated_comparative_lineplots_4, fc_lowplus ~ index_comp + gdpcapita + govexp))

# omitted countries: taiwan (no govexp); lithuania (no govexp); venezuela (no index_comp)

pred1 <- lm_robust(data=updated_comparative_lineplots_4, fc_gov ~ index_comp + gdpcapita)
pred2 <- predict(pred1, newdata=updated_comparative_lineplots_4)

# note high correlations between market intervention index and gdp per capita
cor.test(updated_comparative_lineplots_4$index_comp, 
         updated_comparative_lineplots_4$gdpcapita)

cor.test(updated_comparative_lineplots_4$index_comp, 
          updated_comparative_lineplots_4$govexp)

cor.test(updated_comparative_lineplots_4$govexp, 
          updated_comparative_lineplots_4$gdpcapita)

# testing descriptive patterns still hold after controls and are statistically significant
# NOTE: these are NOT the estimates reported in Tables S8 - S11 (see Stata code for those results)
# multilevel models

# code micro level control variables first

# age
temp_df_issp_all$age <- temp_df_issp_all$AGE

# binary gender
temp_df_issp_all$sexR <- temp_df_issp_all$SEX
temp_df_issp_all$sexR <- ifelse(temp_df_issp_all$SEX==1,0,temp_df_issp_all$SEX)
temp_df_issp_all$sexR <- ifelse(temp_df_issp_all$SEX==2,1,temp_df_issp_all$sexR)
table(temp_df_issp_all$SEX, temp_df_issp_all$sexR)
summary(temp_df_issp_all$SEX)
summary(temp_df_issp_all$sexR)
# make factor var
temp_df_issp_all$sexR_num <- temp_df_issp_all$sexR
cro_cpct(temp_df_issp_all$sexR_num,  weight = temp_df_issp_all$weight)
temp_df_issp_all$sexR <- as.factor(temp_df_issp_all$sexR_num)
cro_cpct(temp_df_issp_all$sexR,  weight = temp_df_issp_all$weight)
class(temp_df_issp_all$sexR)
levels(temp_df_issp_all$sexR) <- list(Men = "0", Women = "1")
cro_cpct(temp_df_issp_all$sexR,  weight = temp_df_issp_all$weight)
cro_cpct(temp_df_issp_all$SEX,  weight = temp_df_issp_all$weight)

# education, grouped into 2 cats using degree variable
temp_df_issp_all$degree_2cat <- NA
temp_df_issp_all$degree_2cat <- ifelse((temp_df_issp_all$DEGREE>=0 & temp_df_issp_all$DEGREE<=6), temp_df_issp_all$DEGREE, NA)
table(temp_df_issp_all$degree_2cat)
summary(temp_df_issp_all$degree_2cat)
temp_df_issp_all$degree_2cat <- ifelse(temp_df_issp_all$DEGREE >= 0 & temp_df_issp_all$DEGREE <= 3, 0, temp_df_issp_all$degree_2cat)
temp_df_issp_all$degree_2cat <- ifelse(temp_df_issp_all$DEGREE >= 4 & temp_df_issp_all$DEGREE <= 6, 1, temp_df_issp_all$degree_2cat)
table(temp_df_issp_all$degree_2cat)
table(temp_df_issp_all$DEGREE,temp_df_issp_all$degree_2cat)
cro_cpct(temp_df_issp_all$degree_2cat, weight = temp_df_issp_all$weight)
# make factor var
temp_df_issp_all$degree_2cat_num <- temp_df_issp_all$degree_2cat
cro_cpct(temp_df_issp_all$degree_2cat_num,  weight = temp_df_issp_all$weight)
temp_df_issp_all$degree_2cat <- as.factor(temp_df_issp_all$degree_2cat_num)
cro_cpct(temp_df_issp_all$degree_2cat,  weight = temp_df_issp_all$weight)
class(temp_df_issp_all$degree_2cat)
levels(temp_df_issp_all$degree_2cat) <- list('Less than BA' = "0", 'BA or more' = "1")
cro_cpct(temp_df_issp_all$degree_2cat,  weight = temp_df_issp_all$weight)

# income quartiles
# use family income
temp_df_issp_all$finc_4cat <- NA
temp_df_issp_all$finc_4cat <- ifelse(temp_df_issp_all$Phinc >= 0 & temp_df_issp_all$Phinc <= 25, 0, temp_df_issp_all$finc_4cat)
temp_df_issp_all$finc_4cat <- ifelse(temp_df_issp_all$Phinc >= 26 & temp_df_issp_all$Phinc <= 50, 1, temp_df_issp_all$finc_4cat)
temp_df_issp_all$finc_4cat <- ifelse(temp_df_issp_all$Phinc >= 51 & temp_df_issp_all$Phinc <= 75, 2, temp_df_issp_all$finc_4cat)
temp_df_issp_all$finc_4cat <- ifelse(temp_df_issp_all$Phinc >= 76 & temp_df_issp_all$Phinc <= 100, 3, temp_df_issp_all$finc_4cat)
cro_cpct(temp_df_issp_all$finc_4cat, weight = temp_df_issp_all$weight)
table(temp_df_issp_all$Phinc,temp_df_issp_all$finc_4cat)
# make factor var
temp_df_issp_all$finc_4cat_num <- temp_df_issp_all$finc_4cat
cro_cpct(temp_df_issp_all$finc_4cat_num,  weight = temp_df_issp_all$weight)
temp_df_issp_all$finc_4cat <- as.factor(temp_df_issp_all$finc_4cat_num)
cro_cpct(temp_df_issp_all$finc_4cat,  weight = temp_df_issp_all$weight)
class(temp_df_issp_all$finc_4cat)
levels(temp_df_issp_all$finc_4cat) <- list('Quart 1 Inc' = "0", 'Quart 2 Inc' = "1", 'Quart 3 Inc' = "2", 'Quart 4 Inc' = "3")
cro_cpct(temp_df_issp_all$finc_4cat,  weight = temp_df_issp_all$weight)

# party id, grouped into 3 categories
temp_df_issp_all$partyid_3cat <- NA
temp_df_issp_all$partyid_3cat <- ifelse(temp_df_issp_all$PARTY_LR >= 1 & temp_df_issp_all$PARTY_LR <= 2, 0, temp_df_issp_all$partyid_3cat)
temp_df_issp_all$partyid_3cat <- ifelse(temp_df_issp_all$PARTY_LR %in% c(3,6), 1, temp_df_issp_all$partyid_3cat)
temp_df_issp_all$partyid_3cat <- ifelse(temp_df_issp_all$PARTY_LR >= 4 & temp_df_issp_all$PARTY_LR <= 5, 2, temp_df_issp_all$partyid_3cat)
table(temp_df_issp_all$PARTY_LR,temp_df_issp_all$partyid_3cat)
# make factor var
cro_cpct(temp_df_issp_all$partyid_3cat,  weight = temp_df_issp_all$weight)
temp_df_issp_all$partyid_3cat_num <- temp_df_issp_all$partyid_3cat
cro_cpct(temp_df_issp_all$partyid_3cat_num,  weight = temp_df_issp_all$weight)
temp_df_issp_all$partyid_3cat <- as.factor(temp_df_issp_all$partyid_3cat_num)
cro_cpct(temp_df_issp_all$partyid_3cat,  weight = temp_df_issp_all$weight)
class(temp_df_issp_all$partyid_3cat)
levels(temp_df_issp_all$partyid_3cat) <- list(Left = "0", Center = "1", Right = "2")
cro_cpct(temp_df_issp_all$partyid_3cat,  weight = temp_df_issp_all$weight)

# country
temp_df_issp_all$country_fac <- as.factor(temp_df_issp_all$country)

# forced choice: collapse 6 into 3 categories
temp_df_issp_all$fc_busplus = temp_df_issp_all$respineq_bus +
  temp_df_issp_all$respineq_union + temp_df_issp_all$respineq_high
temp_df_issp_all$fc_lowplus = temp_df_issp_all$respineq_low +
  temp_df_issp_all$respineq_none 
table(temp_df_issp_all$respineq, temp_df_issp_all$fc_busplus)
table(temp_df_issp_all$respineq, temp_df_issp_all$fc_lowplus)
table(temp_df_issp_all$respineq, temp_df_issp_all$respineq_gov)

# multilevel models

lmer(respineq_gov ~ 1 + (1|country), data=temp_df_issp_all) 
lmer(respineq_gov ~ 1 + marketfreedom + (1|country), data=temp_df_issp_all) 
lmer(respineq_gov ~ 1 + marketfreedom + (1|country), data=temp_df_issp_all, weights = weight) 
lmer(respineq_gov ~ 1 + marketfreedom + Zagemc + (1|country), data=temp_df_issp_all, weights = weight) 
lmer(respineq_gov ~ 1 + marketfreedom + zgdp + Zagemc + Zphincmc + (1|country), data=temp_df_issp_all, weights = weight) 

summary(lmer(respineq_gov ~ 1 + marketfreedom + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(respineq_gov ~ 1 + marketfreedom + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(respineq_gov ~ 1 + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 

summary(lmer(fc_busplus ~ 1 + marketfreedom + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + marketfreedom + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + marketfreedom + govexp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + gdp + govexp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 

# final multilevel models

summary(lmer(respineq_gov ~ 1 + marketfreedom + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + marketfreedom + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_lowplus ~ 1 + marketfreedom + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 

summary(lmer(respineq_gov ~ 1 + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_busplus ~ 1 + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 
summary(lmer(fc_lowplus ~ 1 + zgdp + age + sexR + degree_2cat + partyid_3cat + finc_4cat + (1|country), data=temp_df_issp_all, weights = weight)) 

