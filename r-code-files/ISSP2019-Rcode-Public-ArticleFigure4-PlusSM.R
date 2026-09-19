# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 4 and Table S5 (in SM C) in Analysis 1 Robustness Checks
# Data files used:
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

#################################################################
# Main text Figure 4 and SM Table S5 using the parallel items
# convert to 0-1 scale (from 1-5 scale)
# above code needed for robustness checks and correlations below

temp_df_issp_all$goveqinc1 <- NA
temp_df_issp_all$mkt1 <- NA

temp_df_issp_all$goveqinc1 <- temp_df_issp_all$v22
temp_df_issp_all$mkt1 <- temp_df_issp_all$v24

temp_df_issp_all$goveqinc1 <- ifelse( (temp_df_issp_all$goveqinc1>=1 & temp_df_issp_all$goveqinc1<=5), temp_df_issp_all$goveqinc1, NA)
temp_df_issp_all$mkt1 <- ifelse( (temp_df_issp_all$mkt1>=1 & temp_df_issp_all$mkt1<=5), temp_df_issp_all$mkt1, NA)

# set max length of original scale
max <- 5

# convert to 0-1 and test with crosstab
temp_df_issp_all$goveqinc1 <- (max + 1) - temp_df_issp_all$goveqinc1
temp_df_issp_all$goveqinc1 <- (temp_df_issp_all$goveqinc1-1)/(max-1)
table(temp_df_issp_all$v22, temp_df_issp_all$goveqinc1)

temp_df_issp_all$mkt1 <- (max + 1) - temp_df_issp_all$mkt1
temp_df_issp_all$mkt1 <- (temp_df_issp_all$mkt1-1)/(max-1)
table(temp_df_issp_all$v24, temp_df_issp_all$mkt1)

# weighted correlations at the individual level of market vs. govt parallel items (=.31)
wtd.cor(temp_df_issp_all$mkt1, temp_df_issp_all$goveqinc1, weight=temp_df_issp_all$weight)
wtd.cor(temp_df_issp_all$mkt1, temp_df_issp_all$goveqinc1)

wtd.cor(temp_df_issp_all$v22, temp_df_issp_all$v24, weight=temp_df_issp_all$weight)
wtd.cor(temp_df_issp_all$v22, temp_df_issp_all$v24)
cor.test(temp_df_issp_all$v22, temp_df_issp_all$v24)

#rm(rmmissdata_issp)
rmmissdata_issp <- temp_df_issp_all %>%
  filter(!if_any(c(goveqinc1, mkt1, weight), is.na))

wtd.cor(rmmissdata_issp$mkt1, rmmissdata_issp$goveqinc1, weight=rmmissdata_issp$weight)
wtd.cor(rmmissdata_issp$mkt1, rmmissdata_issp$goveqinc1)
cor.test(rmmissdata_issp$mkt1, rmmissdata_issp$goveqinc1)

# to get N (48,775)
sum(complete.cases(temp_df_issp_all$mkt1,
                             temp_df_issp_all$goveqinc1,
                             temp_df_issp_all$weight))
sum(complete.cases(temp_df_issp_all$mkt1,
                             temp_df_issp_all$goveqinc1,
                             temp_df_issp_all$weight) & temp_df_issp_all$weight > 0)

# rm(country_means_bus_parallel_0_1, country_means_gov_parallel_0_1)
# call function to get country means of parallel govt item
country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=goveqinc1, weight_var0=weight)
print(country_means, n=35)

# make labels
# get labels for graphs; must fix Ukraine label
country_means$varlabel <- get_labels(country_means$country)
print.data.frame(country_means)
country_means$varlabel <- ifelse(country_means$varlabel=="Ukraine","-Ukraine",country_means$varlabel)
print.data.frame(country_means)
country_means$varlabel <- country_means$varlabel %>% stringr::str_remove(pattern = ".*-")
print.data.frame(country_means)

# sort by mean value
country_means <- country_means[order(country_means$wtdmean),]
print.data.frame(country_means)

# save to new df
country_means_gov_parallel_0_1 <- country_means
print(country_means_gov_parallel_0_1, n=35)
country_means_old <- country_means 
rm(country_means)

# call function to get country means of parallel market item
country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=mkt1, weight_var0=weight)
print(country_means, n=35)

# make labels
# get labels for graphs; must fix Ukraine label
country_means$varlabel <- get_labels(country_means$country)
print.data.frame(country_means)
country_means$varlabel <- ifelse(country_means$varlabel=="Ukraine","-Ukraine",country_means$varlabel)
print.data.frame(country_means)
country_means$varlabel <- country_means$varlabel %>% stringr::str_remove(pattern = ".*-")
print.data.frame(country_means)

# sort by mean value
country_means <- country_means[order(country_means$wtdmean),]
print.data.frame(country_means)

# save to new df
country_means_bus_parallel_0_1 <- country_means
print(country_means_bus_parallel_0_1, n=35)
country_means_old <- country_means 
rm(country_means)

# correlation of parallel govt and market itmes at country level
country_means_bus_parallel_0_1$wtdmean_b <- country_means_bus_parallel_0_1$wtdmean
country_means_gov_parallel_0_1$wtdmean_g <- country_means_gov_parallel_0_1$wtdmean
country_means_bus_parallel_0_1 <- country_means_bus_parallel_0_1[order(country_means_bus_parallel_0_1$country),]
country_means_gov_parallel_0_1 <- country_means_gov_parallel_0_1[order(country_means_gov_parallel_0_1$country),]
print.data.frame(country_means_bus_parallel_0_1)
print.data.frame(country_means_gov_parallel_0_1)
names(country_means_bus_parallel_0_1)
names(country_means_gov_parallel_0_1)
combine_parallel_items_by_country <- cbind(country_means_bus_parallel_0_1,country_means_gov_parallel_0_1)
print.data.frame(combine_parallel_items_by_country)

# corr = .56
cor.test(combine_parallel_items_by_country$wtdmean_b,
         combine_parallel_items_by_country$wtdmean_g)

# further descriptive analyses
# countries with bus > gov
# Australia, New Zealand, GB, US, Denmark, France, Iceland, Switzerland
print.data.frame(combine_parallel_items_by_country[combine_parallel_items_by_country$wtdmean_b > 
                                                     combine_parallel_items_by_country$wtdmean_g,])

# 2x2 table into mosaic graph with parallel items pooled and across subsets of countries

cro_cpct(temp_df_issp_all$goveqinc1, list(total(), temp_df_issp_all$mkt1), weight = temp_df_issp_all$weight)

# create binary variables for parallel items (1=top 2 cats; 0=bottom 3 cats) 
# to create 2x2 analysis
temp_df_issp_all$goveqinc1_bin <- ifelse(temp_df_issp_all$goveqinc1 %in% c(0,0.25,.50), 0, NA)
temp_df_issp_all$mkt1_bin <- ifelse(temp_df_issp_all$mkt1 %in% c(0,0.25,.50), 0, NA)

temp_df_issp_all$goveqinc1_bin <- ifelse(temp_df_issp_all$goveqinc1 %in% c(0.75,1.0), 1, temp_df_issp_all$goveqinc1_bin)
temp_df_issp_all$mkt1_bin <- ifelse(temp_df_issp_all$mkt1 %in% c(0.75,1.0), 1, temp_df_issp_all$mkt1_bin)

cro_cpct(temp_df_issp_all$goveqinc1, temp_df_issp_all$goveqinc1_bin, weight = temp_df_issp_all$weight)
cro_cpct(temp_df_issp_all$mkt1, temp_df_issp_all$mkt1_bin, weight = temp_df_issp_all$weight)

cro_tpct(temp_df_issp_all$goveqinc1_bin, temp_df_issp_all$mkt1_bin, weight = temp_df_issp_all$weight)

with(subset(temp_df_issp_all, country==840), 
     cro_tpct(goveqinc1_bin, mkt1_bin, weight = weight))

for (i in 1:k) {
  country_crosstab <- with(subset(temp_df_issp_all, country==countrycodes$labels[i]), 
                           cro_tpct(goveqinc1_bin, mkt1_bin, weight = weight))
  print(countrycodes$labels[i])
  print(country_crosstab)
}

#rm(mostmarketcountries, mostgovtcountries)
mostmarketcountries <- subset(temp_df_issp_all, country %in% c(36,208,250,352,554,756,826,840))
mostgovtcountries <- subset(temp_df_issp_all, !(country %in% c(36,208,250,352,554,756,826,840)))

cro_tpct(mostmarketcountries$goveqinc1_bin, mostmarketcountries$mkt1_bin, weight = mostmarketcountries$weight)
cro_tpct(mostgovtcountries$goveqinc1_bin, mostgovtcountries$mkt1_bin, weight = mostgovtcountries$weight)

#rm(country_crosstab)
for (i in 1:k) {
  country_crosstab <- with(subset(mostmarketcountries, country==countrycodes$labels[i]), 
                           cro_tpct(goveqinc1_bin, mkt1_bin, weight = weight))
  print(countrycodes$labels[i])
  print(country_crosstab)
}

mostmarketcountries$goveqinc1_bin_fac <- factor(mostmarketcountries$goveqinc1_bin,
                                                levels = c(0,1),
                                                labels = c("No", "Yes"))
mostmarketcountries$mkt1_bin_fac <- factor(mostmarketcountries$mkt1_bin,
                                           levels = c(0,1),
                                           labels = c("No", "Yes"))

mostgovtcountries$goveqinc1_bin_fac <- factor(mostgovtcountries$goveqinc1_bin,
                                              levels = c(0,1),
                                              labels = c("No", "Yes"))
mostgovtcountries$mkt1_bin_fac <- factor(mostgovtcountries$mkt1_bin,
                                         levels = c(0,1),
                                         labels = c("No", "Yes"))

# unconditional mosaic plots, govt on x axis

#rm(moremkt_mosaic_data, moregovt_mosaic_data)

moremkt_mosaic_data <- mostmarketcountries %>% 
  drop_na(goveqinc1_bin_fac, mkt1_bin_fac, weight) %>% 
  count(goveqinc1_bin_fac, mkt1_bin_fac, wt = weight, name = "freq") %>% 
  mutate(uniqueid = row_number())

moregovt_mosaic_data <- mostgovtcountries %>% 
  drop_na(goveqinc1_bin_fac, mkt1_bin_fac, weight) %>% 
  count(goveqinc1_bin_fac, mkt1_bin_fac, wt = weight, name = "freq") %>% 
  mutate(uniqueid = row_number())

#rm(moremkt_mosaic, moregovt_mosaic)

moremkt_mosaic <- 
  ggplot(moremkt_mosaic_data) +
  geom_marimekko(aes(weight = freq, fill = mkt1_bin_fac),
                 formula = ~ goveqinc1_bin_fac | mkt1_bin_fac, 
                 color = "black", gap = 0.05, linewidth=1) +
  layer(
    stat = StatMarimekkoTiles,
    geom = GeomText,
    mapping = aes(label = paste0(round(after_stat(weight/ sum(weight)) * 100, digits=0), "%")), 
    data = moremkt_mosaic_data,
    position = "identity",
    params = list(colour = "black", alpha = 0.7, size = 2.75)
  ) +
  theme_marimekko() +
  scale_fill_manual(values=c("lightyellow", "lightyellow")) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size = 8), 
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0, size=9, face="bold"),
        #plot.subtitle = element_text(hjust = 0.5, size=9),
        text = element_text(family = "PT Sans")) +
  labs(title="B. Market-only > Government-only (N=8)", subtitle="", y="Support market predistribution", x="Support government redistribution")

moregovt_mosaic <- 
  ggplot(moregovt_mosaic_data) +
  geom_marimekko(aes(weight = freq, fill = mkt1_bin_fac),
                 formula = ~ goveqinc1_bin_fac | mkt1_bin_fac, 
                 color = "black", gap = 0.05, linewidth=1) +
  layer(
    stat = StatMarimekkoTiles,
    geom = GeomText,
    mapping = aes(label = paste0(round(after_stat(weight/ sum(weight)) * 100, digits=0), "%")), 
    data = moregovt_mosaic_data,
    position = "identity",
    params = list(colour = "black", alpha = 0.7, size = 3)
  ) +
  theme_marimekko() +
  scale_fill_manual(values=c("lightyellow", "lightyellow")) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size = 8), 
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0, size=9, face="bold"),
        #plot.subtitle = element_text(hjust = 0.5, size=9),
        text = element_text(family = "PT Sans")) +
  labs(title="A. Government-only > Market-only (N=26)", subtitle="", y="Support market predistribution", x="Support government redistribution")

# roughly 950 x 450 but exported to image and then copied and changed image in word
moregovt_mosaic + moremkt_mosaic

# save to file
# single column, same font, etc., sizing as article figure 2, which is 2 columns, so scale x 2 preserves relative 
# fonts sizes for single column vs. double column figures without changing sizing in ggplot command

rm(articlefigure4)
articlefigure4 <- moregovt_mosaic + moremkt_mosaic
articlefigure4
ggsave("ArticleFigure4.pdf", plot = articlefigure4, width = 3.55, height = 1.5, units = "in", dpi = 300, scale = 2.0)

# now create a table with the 2x2 cell quantities in columns and countries in row
# order from highest to lowest market-only shares

temp_df_issp_all$pi_marketonly <- NA
temp_df_issp_all$pi_govtonly <- NA
temp_df_issp_all$pi_residual <- NA
temp_df_issp_all$pi_both <- NA

temp_df_issp_all$pi_marketonly <- ifelse(temp_df_issp_all$goveqinc1_bin==0 & temp_df_issp_all$mkt1_bin==1, 1, 0)
temp_df_issp_all$pi_govtonly <- ifelse(temp_df_issp_all$goveqinc1_bin==1 & temp_df_issp_all$mkt1_bin==0, 1, 0)
temp_df_issp_all$pi_residual <- ifelse(temp_df_issp_all$goveqinc1_bin==0 & temp_df_issp_all$mkt1_bin==0, 1, 0)
temp_df_issp_all$pi_both <- ifelse(temp_df_issp_all$goveqinc1_bin==1 & temp_df_issp_all$mkt1_bin==1, 1, 0)

temp_df_issp_all$pi_marketonly <- ifelse(temp_df_issp_all$goveqinc1_bin=="NA" | temp_df_issp_all$mkt1_bin=="NA", NA, temp_df_issp_all$pi_marketonly)
temp_df_issp_all$pi_govtonly <- ifelse(temp_df_issp_all$goveqinc1_bin=="NA" | temp_df_issp_all$mkt1_bin=="NA", NA, temp_df_issp_all$pi_govtonly)
temp_df_issp_all$pi_residual <- ifelse(temp_df_issp_all$goveqinc1_bin=="NA" | temp_df_issp_all$mkt1_bin=="NA", NA, temp_df_issp_all$pi_residual)
temp_df_issp_all$pi_both <- ifelse(temp_df_issp_all$goveqinc1_bin=="NA" | temp_df_issp_all$mkt1_bin=="NA", NA, temp_df_issp_all$pi_both)

# double check with all countries pooled and only the US
# 'p' refers to 'parallel items'
cro_tpct(temp_df_issp_all$goveqinc1_bin, temp_df_issp_all$mkt1_bin, weight = temp_df_issp_all$weight)
cro_tpct(temp_df_issp_all$pi_residual, weight = temp_df_issp_all$weight)
cro_tpct(temp_df_issp_all$pi_both, weight = temp_df_issp_all$weight)

with(subset(temp_df_issp_all, country==840), cro_tpct(goveqinc1_bin, mkt1_bin, weight = weight))
with(subset(temp_df_issp_all, country==840), cro_tpct(pi_marketonly, weight = weight))
with(subset(temp_df_issp_all, country==840), cro_tpct(pi_govtonly, weight = weight))
with(subset(temp_df_issp_all, country==840), cro_tpct(pi_residual, weight = weight))
with(subset(temp_df_issp_all, country==840), cro_tpct(pi_both, weight = weight))

# build df with four cells as four different columns

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=pi_marketonly, weight_var0=weight)
country_means_pi_marketonly <- country_means
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=pi_govtonly, weight_var0=weight)
country_means_pi_govtonly <- country_means
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=pi_residual, weight_var0=weight)
country_means_pi_residual <- country_means
rm(country_means)

country_means <- create_country_means(chart_data=temp_df_issp_all, chart_var0=pi_both, weight_var0=weight)
country_means_pi_both <- country_means
rm(country_means)

# check that the total across forced choice binary proportions is 1.0
for (i in 1:k) {
  total_prop <- country_means_pi_marketonly[i,2] + country_means_pi_govtonly[i,2] + 
    country_means_pi_residual[i,2] + country_means_pi_both[i,2]
  print(total_prop)
}

# make unique var names for wtdmean
country_means_pi_marketonly <- rename(country_means_pi_marketonly, pi_market = wtdmean)
country_means_pi_govtonly <- rename(country_means_pi_govtonly, pi_govt = wtdmean)
country_means_pi_residual <- rename(country_means_pi_residual, pi_residual = wtdmean)
country_means_pi_both <- rename(country_means_pi_both, pi_both = wtdmean)

# column bind together
pi_crosstab_govt_market <- cbind(country_means_pi_marketonly,
                                  country_means_pi_govtonly,
                                  country_means_pi_residual,
                                  country_means_pi_both)

# check countries align
for (i in 1:k) {
  country_num <- (pi_crosstab_govt_market[i,1] + pi_crosstab_govt_market[i,3] + 
                    pi_crosstab_govt_market[i,5] + pi_crosstab_govt_market[i,7])/4
  test_country_num <- ifelse(country_num == pi_crosstab_govt_market[i,1], 1, 0)
  print(test_country_num)
}

# remove extra country columns
pi_crosstab_govt_market <- pi_crosstab_govt_market[,-c(3,5,7)]
pi_crosstab_govt_market

# get labels for graphs; must fix Ukraine label
pi_crosstab_govt_market <- pi_crosstab_govt_market[order(pi_crosstab_govt_market$country),]
print.data.frame(pi_crosstab_govt_market)

pi_crosstab_govt_market$varlabel <- get_labels(pi_crosstab_govt_market$country)
print.data.frame(pi_crosstab_govt_market)
pi_crosstab_govt_market$varlabel <- ifelse(pi_crosstab_govt_market$varlabel=="Ukraine",
                                                   "UA-Ukraine",pi_crosstab_govt_market$varlabel)
print.data.frame(pi_crosstab_govt_market)
pi_crosstab_govt_market$varlabel <- pi_crosstab_govt_market$varlabel %>% stringr::str_remove(pattern = "^.*?-")
print.data.frame(pi_crosstab_govt_market)

# sort by market only value, rearrange columns, and print
pi_crosstab_govt_market <- pi_crosstab_govt_market[order(pi_crosstab_govt_market$pi_market, decreasing = TRUE),]
print.data.frame(pi_crosstab_govt_market)

#rm(pi_crosstab_govt_market_print)
pi_crosstab_govt_market_print <- pi_crosstab_govt_market[, c(6,2,3,5,4)]
print.data.frame(pi_crosstab_govt_market_print)

# SM C, Table S5, corresponds to Fig 4 mosaic graph and showing 2x2 values for each country 
tab_df(
  pi_crosstab_govt_market_print,
  CSS = list(css.tdata = 'line-height: 1em;')
  )

# further exploratory analyses of fc market and parallel items market
# correlations reported in the text

pi_crosstab_govt_market_analyses <- pi_crosstab_govt_market[order(pi_crosstab_govt_market$country),]
pi_crosstab_govt_market_analyses

fc_pi_combined_analyses <- cbind(updated_comparative_lineplots_2, pi_crosstab_govt_market_analyses)
fc_pi_combined_analyses

# different measures (pi and fc) of market responses correlated 

# r = 0.83
cor.test(fc_pi_combined_analyses$pi_market,
         fc_pi_combined_analyses$fc_busplus)

# r = -0.25
cor.test(fc_pi_combined_analyses$pi_govt,
         fc_pi_combined_analyses$fc_busplus)

# cor matrix
fc_pi_combined_analyses %>% 
  select(fc_busplus, fc_gov, fc_lowplus, pi_market, pi_govt, pi_residual, pi_both) %>% 
  cor()




