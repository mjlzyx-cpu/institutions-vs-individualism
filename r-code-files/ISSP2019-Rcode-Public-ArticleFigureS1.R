# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure S1 (in SM A)
# Data files used:
# "ISSP 2019 SI with additional (total of 34) countries (updated April 17 2024).sav"

# Read in data file/import dataset from SPSS data file
# Data file is: "ISSP 2019 SI with additional (total of 34) countries (updated April 17 2024).sav"

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

# rm(country_means_bus_bin,country_means_gov_bin,country_means_high_bin,country_means_low_bin,country_means_union_bin,country_means_none_bin)

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

rm(soctype1_df)
soctype1_df <- temp_df_issp_all %>%
  group_by(country) %>%
  #summarise(n = n(), soctypeactual = mean(v48, na.rm=T))
  summarise(soctypeactual = weighted.mean(v48, weight, na.rm=T))
print.data.frame(soctype1_df)

soctype2_df <- temp_df_issp_all %>%
  group_by(country) %>%
  #summarise(n = n(), soctypedesired = mean(v49, na.rm=T))
  summarise(soctypedesired = weighted.mean(v49, weight, na.rm=T))
print.data.frame(soctype2_df)


updated_comparative_lineplots_3 <- cbind(updated_comparative_lineplots_2, soctype1_df, soctype2_df)
updated_comparative_lineplots_3

# check countries align
for (i in 1:k) {
  country_num <- (updated_comparative_lineplots_3[i,11] + updated_comparative_lineplots_3[i,13])/2
  test_country_num <- ifelse(country_num == updated_comparative_lineplots_3[i,1], 1, 0)
  print(test_country_num)
}

# remove extra country columns
updated_comparative_lineplots_4 <- updated_comparative_lineplots_3[,-c(11,13)]
updated_comparative_lineplots_4

# Article Figure S1
# perceived actual vs. desired using weighted means

# vector-based high-res image and other journal specs for publication

# setup
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

rm(articlefigureS1)
articlefigureS1 <- 
  ggplot(updated_comparative_lineplots_4) + 
  geom_point(aes(y=soctypedesired, x=soctypeactual), color="gray40", fill="gray40", size=.50) +
  geom_text(aes(y=soctypedesired, x=soctypeactual, label=varlabel), fontface=2, colour = "gray40", size = 2.0, vjust=-1) +
  geom_smooth(aes(y=soctypedesired, x=soctypeactual), method='lm_robust', na.rm=TRUE, se=TRUE, color="gray40") +
  geom_abline(slope=1, intercept=0, color="steelblue") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12)) +
  scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5), guide = guide_axis(n.dodge = 2), 
                     labels = c("1. Small top, large bottom", "2. Even pyramid", "3. Pyramid, large middle", 
                                "4. Diamomd, large middle", "5. Top heavy")) +
  scale_y_continuous(limits = c(1,5), breaks = c(1,2,3,4,5), oob = scales::squish) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8), 
        axis.text.y = element_text(size = 7), 
        axis.title.y = element_text(size = 8), 
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(title="", subtitle="", y="Desired society type", x="Perceived actual society type")

articlefigureS1
ggsave("ArticleFigureS1.pdf", plot = articlefigureS1, width = 5.00, height = 4.00, units = "in", dpi = 300)
