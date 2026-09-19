
# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 2 in Analysis 1 
# Code also for: Analysis 1 Robustness Checks in Tables S1 (in SM B) and S2 (in SM C)
# Data files used:
# "ISSP 2019 SI with additional (total of 34) countries (updated April 17 2024).sav"
# "TESS3_199_Client-public-test.dta"

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

# rm(create_country_means)
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
  
  # get labels for graphs; must fix Ukraine label
  country_means$varlabel <- get_labels(chart_data$country)
  print.data.frame(country_means)
  country_means$varlabel <- ifelse(country_means$varlabel=="Ukraine","-Ukraine",country_means$varlabel)
  print.data.frame(country_means)
  country_means$varlabel <- country_means$varlabel %>% stringr::str_remove(pattern = ".*-")
  print.data.frame(country_means)
  
  # add region var
  country_means$region <- factor(c(""))
  country_means$region <- ifelse(country_means$country %in% c(36,554,826,840),"Anglo",country_means$region)
  country_means$region <- ifelse(country_means$country %in% c(208,246,352,578,752),"Nordic",country_means$region)
  country_means$region <- ifelse(country_means$country %in% c(100,191,203,233,348,440,643,703,705,804),"E/C European",country_means$region)
  country_means$region <- ifelse(country_means$country %in% c(40,250,276,380,756),"Cont European",country_means$region)
  country_means$region <- ifelse(country_means$country %in% c(158,356,392,608,764),"Asian",country_means$region)
  country_means$region <- ifelse(country_means$country %in% c(152,740,862,710,376),"Latin America/Other",country_means$region)
  print.data.frame(country_means)
  
  # sort by mean value
  country_means <- country_means[order(country_means$wtdmean),]
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

# revert to country order (rather than low to high means order)

country_means_bin_low_countryorder <- country_means_bin_low[order(country_means_bin_low$country),]
country_means_bin_union_countryorder <- country_means_bin_union[order(country_means_bin_union$country),]
country_means_bin_high_countryorder <- country_means_bin_high[order(country_means_bin_high$country),]
country_means_bin_none_countryorder <- country_means_bin_none[order(country_means_bin_none$country),]
country_means_bin_bus_countryorder <- country_means_bin_bus[order(country_means_bin_bus$country),]
country_means_bin_gov_countryorder <- country_means_bin_gov[order(country_means_bin_gov$country),]

# check that the total across forced choice binary proportions is 1.0
# 2/22/24, this checks out

for (i in 1:k) {
  total_prop <- country_means_bin_bus_countryorder[i,2] + country_means_bin_gov_countryorder[i,2] + 
    country_means_bin_union_countryorder[i,2] + country_means_bin_high_countryorder[i,2] + 
    country_means_bin_low_countryorder[i,2] + country_means_bin_none_countryorder[i,2]
  print(total_prop)
}

# now create full tidy data frame for stacked graph in Artile Figure 2

# add new column for FC category to each df

country_means_bin_gov_countryorder$fcoption <- "Government"
country_means_bin_bus_countryorder$fcoption <- "Private companies"
country_means_bin_union_countryorder$fcoption <- "Unions"
country_means_bin_high_countryorder$fcoption <- "High-income people"
country_means_bin_low_countryorder$fcoption <- "Low-income people"
country_means_bin_none_countryorder$fcoption <- "No reduction"

# re-ordering
# sort by mean value of government category

# rm(gov_order)
gov_order <- country_means_bin_gov_countryorder[,2]
colnames(gov_order) <- c('wtdmeangov')

stack_gov <- cbind(country_means_bin_gov_countryorder,gov_order)
stack_bus <- cbind(country_means_bin_bus_countryorder,gov_order)
stack_union <- cbind(country_means_bin_union_countryorder,gov_order)
stack_high <- cbind(country_means_bin_high_countryorder,gov_order)
stack_low <- cbind(country_means_bin_low_countryorder,gov_order)
stack_none <- cbind(country_means_bin_none_countryorder,gov_order)

stack_gov <- stack_gov[order(stack_gov$wtdmeangov),]
stack_bus <- stack_bus[order(stack_bus$wtdmeangov),]
stack_union <- stack_union[order(stack_union$wtdmeangov),]
stack_high <- stack_high[order(stack_high$wtdmeangov),]
stack_low <- stack_low[order(stack_low$wtdmeangov),]
stack_none <- stack_none[order(stack_none$wtdmeangov),]

stack_gov_order <- rbind(stack_none, stack_low, stack_high, stack_bus, stack_union, stack_gov)
stack_gov_order$fcoption <- as.factor(stack_gov_order$fcoption)

# stacked ggplot with all countries and categories visible

# add value labels for government and business

#rm(stack_gov_order_2)
stack_gov_order_2 <- stack_gov_order
stack_gov_order_2$wtdmeanlabel <- "X"
stack_gov_order_2$wtdmeanlabel <- ifelse(stack_gov_order_2$fcoption %in% c("Government", "Private companies"), 
                                        rd(stack_gov_order_2$wtdmean*100, digits=0), "X")

# vector-based high-res image and other journal specs for publication

# setup
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

# plot
#rm(articlefigure2)
articlefigure2 <- stack_gov_order_2 %>%
  ggplot(aes(fill=fct_inorder(fcoption),x=fct_inorder(varlabel), y=wtdmean*100)) + 
  geom_bar(position="stack", stat="identity", color = "white", linewidth = .5) +
  geom_text(aes(label=ifelse(wtdmeanlabel!="X",paste(sprintf("%s", wtdmeanlabel)),"")), 
            color="white", family="PT Sans", fontface=2, size=2.0, hjust = .5, vjust = 1.5, position = "stack") +
  scale_fill_manual(values=c("lightsteelblue3", "lightsteelblue1","plum1","red2", "palevioletred3","royalblue4")) +
  theme_classic() +
  theme(legend.title=element_blank(), 
        legend.text = element_text(size=7), 
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        strip.text = element_text(size = 8),
        axis.text.y = element_text(size=8), 
        axis.title.y = element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(y="Percent", x="") 

# save to file
# 2 column width, no scale parameter needed 
articlefigure2
ggsave("ArticleFigure2.pdf", plot = articlefigure2, width = 7.25, height = 4, units = "in", dpi = 300)

######## SM analyses related to Article Figure 2

# SM B, Table S1
# TESS data using randomized categories
# in original data: 1= govt; 2=low; 3=bus; 4=charity; 5=high; 6=none;
# imported as stata data file, "TESS3_199_Client-public-test.dta"

#rm(tessdata1)
tessdata1 <- TESS3_199_Client_public_test
rm(TESS3_199_Client_public_test)

# weighted values shown in Table S1
cro_cpct(tessdata1$policy_summ, list(total(), tessdata1$treatment), weight = tessdata1$weight)
cro_cpct(tessdata1$policy_summ, list(total(), tessdata1$treatment))

# SM C, Table S2: 
# testing descriptives are statistically significant and hold with controls

# make new df for this section
#rm(temp_df_issp_all_refUS)
temp_df_issp_all_refUS <- temp_df_issp_all

# create new variable with labels only (not numerical values) for countries

# make df with just country number and label
#rm(namesdf)
namesdf <- data.frame(varlabel=character(34), varvalue=double(34))
namesdf$varlabel <- get_labels(temp_df_issp_all$country)
namesdf$varvalue <- get_values(temp_df_issp_all$country)
namesdf

# remove country abbreviations 
namesdf$varlabel <- namesdf$varlabel %>% stringr::str_remove(pattern = ".*-")
namesdf

# map country label onto new variable in main df using match function
temp_df_issp_all_refUS$country_name <- 
  as.character(namesdf[match(temp_df_issp_all_refUS$country,namesdf$varvalue), 'varlabel'])
temp_df_issp_all_refUS$country_name <- as.factor(temp_df_issp_all_refUS$country_name)

# make US the ref category before running models
temp_df_issp_all_refUS <- within(temp_df_issp_all_refUS, country_name <- relevel(country_name, ref = "United States"))

### run analyses and make tables

# bivariate with country dummies and US as ref category

# bivariate for market predistribution outcome
#rm(lmcountryorder_bus)
lmcountryorder_bus <- lm_robust(data=temp_df_issp_all_refUS, respineq_bus ~ country_name, weights=weight)

temp_df_issp_all_refUS$respineq_market <- temp_df_issp_all_refUS$respineq_bus + 
                                            temp_df_issp_all_refUS$respineq_union +
                                            temp_df_issp_all_refUS$respineq_high

#rm(lmcountryorder_market)
lmcountryorder_market <- lm_robust(data=temp_df_issp_all_refUS, respineq_market ~ country_name, weights = weight)

# bivariate for government redistribution outcome
#rm(lmcountryorder_gov)
lmcountryorder_gov <- lm_robust(data=temp_df_issp_all_refUS, respineq_gov ~ country_name, weights = weight)

# bivariate for individual responsibility outcome
temp_df_issp_all_refUS$respineq_individ <- temp_df_issp_all_refUS$respineq_low + 
                                            temp_df_issp_all_refUS$respineq_none
#rm(lmcountryorder_individ)
lmcountryorder_individ <- lm_robust(data=temp_df_issp_all_refUS, respineq_individ ~ country_name, weights = weight)

# create Table S2 with all three outcomes 
# copy and paste into paper and then set font to 6.5
tab_model(
  lmcountryorder_market, lmcountryorder_gov, lmcountryorder_individ,
  auto.label = TRUE,
  show.reflvl = TRUE,
  dv.labels = c("Market Predistribution", "Government Redistribution", "Individual Responsibility"),
  show.se=TRUE,
  show.ci=FALSE
)

# test results for private companies option alone
# reported in notes to Table S2
tab_model(
  lmcountryorder_bus,
  auto.label = TRUE,
  show.reflvl = TRUE,
  dv.labels = c("Market Predistribution", "Government Redistribution", "Individual Responsibility"),
  show.se=TRUE
)

# testing descriptives still hold after controls and are statistically significant
# reported in notes to Table S2  

# code micro level control variables first
# age
temp_df_issp_all_refUS$age <- temp_df_issp_all_refUS$AGE

# binary gender
temp_df_issp_all_refUS$sexR <- temp_df_issp_all_refUS$SEX
temp_df_issp_all_refUS$sexR <- ifelse(temp_df_issp_all_refUS$SEX==1,0,temp_df_issp_all_refUS$SEX)
temp_df_issp_all_refUS$sexR <- ifelse(temp_df_issp_all_refUS$SEX==2,1,temp_df_issp_all_refUS$sexR)
table(temp_df_issp_all_refUS$SEX, temp_df_issp_all_refUS$sexR)
summary(temp_df_issp_all_refUS$SEX)
summary(temp_df_issp_all_refUS$sexR)
# make factor var
temp_df_issp_all_refUS$sexR_num <- temp_df_issp_all_refUS$sexR
cro_cpct(temp_df_issp_all_refUS$sexR_num,  weight = temp_df_issp_all_refUS$weight)
temp_df_issp_all_refUS$sexR <- as.factor(temp_df_issp_all_refUS$sexR_num)
cro_cpct(temp_df_issp_all_refUS$sexR,  weight = temp_df_issp_all_refUS$weight)
class(temp_df_issp_all_refUS$sexR)
levels(temp_df_issp_all_refUS$sexR) <- list(Men = "0", Women = "1")
cro_cpct(temp_df_issp_all_refUS$sexR,  weight = temp_df_issp_all_refUS$weight)
cro_cpct(temp_df_issp_all_refUS$SEX,  weight = temp_df_issp_all_refUS$weight)

# education, grouped into 2 cats using degree variable
temp_df_issp_all_refUS$degree_2cat <- NA
temp_df_issp_all_refUS$degree_2cat <- ifelse((temp_df_issp_all_refUS$DEGREE>=0 & temp_df_issp_all_refUS$DEGREE<=6), temp_df_issp_all_refUS$DEGREE, NA)
table(temp_df_issp_all_refUS$degree_2cat)
summary(temp_df_issp_all_refUS$degree_2cat)
temp_df_issp_all_refUS$degree_2cat <- ifelse(temp_df_issp_all_refUS$DEGREE >= 0 & temp_df_issp_all_refUS$DEGREE <= 3, 0, temp_df_issp_all_refUS$degree_2cat)
temp_df_issp_all_refUS$degree_2cat <- ifelse(temp_df_issp_all_refUS$DEGREE >= 4 & temp_df_issp_all_refUS$DEGREE <= 6, 1, temp_df_issp_all_refUS$degree_2cat)
table(temp_df_issp_all_refUS$degree_2cat)
table(temp_df_issp_all_refUS$DEGREE,temp_df_issp_all_refUS$degree_2cat)
cro_cpct(temp_df_issp_all_refUS$degree_2cat, weight = temp_df_issp_all_refUS$weight)
# make factor var
temp_df_issp_all_refUS$degree_2cat_num <- temp_df_issp_all_refUS$degree_2cat
cro_cpct(temp_df_issp_all_refUS$degree_2cat_num,  weight = temp_df_issp_all_refUS$weight)
temp_df_issp_all_refUS$degree_2cat <- as.factor(temp_df_issp_all_refUS$degree_2cat_num)
cro_cpct(temp_df_issp_all_refUS$degree_2cat,  weight = temp_df_issp_all_refUS$weight)
class(temp_df_issp_all_refUS$degree_2cat)
levels(temp_df_issp_all_refUS$degree_2cat) <- list('Less than BA' = "0", 'BA or more' = "1")
cro_cpct(temp_df_issp_all_refUS$degree_2cat,  weight = temp_df_issp_all_refUS$weight)

# income quartiles
# use family income
temp_df_issp_all_refUS$finc_4cat <- NA
temp_df_issp_all_refUS$finc_4cat <- ifelse(temp_df_issp_all_refUS$Phinc >= 0 & temp_df_issp_all_refUS$Phinc <= 25, 0, temp_df_issp_all_refUS$finc_4cat)
temp_df_issp_all_refUS$finc_4cat <- ifelse(temp_df_issp_all_refUS$Phinc >= 26 & temp_df_issp_all_refUS$Phinc <= 50, 1, temp_df_issp_all_refUS$finc_4cat)
temp_df_issp_all_refUS$finc_4cat <- ifelse(temp_df_issp_all_refUS$Phinc >= 51 & temp_df_issp_all_refUS$Phinc <= 75, 2, temp_df_issp_all_refUS$finc_4cat)
temp_df_issp_all_refUS$finc_4cat <- ifelse(temp_df_issp_all_refUS$Phinc >= 76 & temp_df_issp_all_refUS$Phinc <= 100, 3, temp_df_issp_all_refUS$finc_4cat)
cro_cpct(temp_df_issp_all_refUS$finc_4cat, weight = temp_df_issp_all_refUS$weight)
table(temp_df_issp_all_refUS$Phinc,temp_df_issp_all_refUS$finc_4cat)
# make factor var
temp_df_issp_all_refUS$finc_4cat_num <- temp_df_issp_all_refUS$finc_4cat
cro_cpct(temp_df_issp_all_refUS$finc_4cat_num,  weight = temp_df_issp_all_refUS$weight)
temp_df_issp_all_refUS$finc_4cat <- as.factor(temp_df_issp_all_refUS$finc_4cat_num)
cro_cpct(temp_df_issp_all_refUS$finc_4cat,  weight = temp_df_issp_all_refUS$weight)
class(temp_df_issp_all_refUS$finc_4cat)
levels(temp_df_issp_all_refUS$finc_4cat) <- list('Quart 1 Inc' = "0", 'Quart 2 Inc' = "1", 'Quart 3 Inc' = "2", 'Quart 4 Inc' = "3")
cro_cpct(temp_df_issp_all_refUS$finc_4cat,  weight = temp_df_issp_all_refUS$weight)

# party id, grouped into 3 categories
temp_df_issp_all_refUS$partyid_3cat <- NA
temp_df_issp_all_refUS$partyid_3cat <- ifelse(temp_df_issp_all_refUS$PARTY_LR >= 1 & temp_df_issp_all_refUS$PARTY_LR <= 2, 0, temp_df_issp_all_refUS$partyid_3cat)
temp_df_issp_all_refUS$partyid_3cat <- ifelse(temp_df_issp_all_refUS$PARTY_LR %in% c(3,6), 1, temp_df_issp_all_refUS$partyid_3cat)
temp_df_issp_all_refUS$partyid_3cat <- ifelse(temp_df_issp_all_refUS$PARTY_LR >= 4 & temp_df_issp_all_refUS$PARTY_LR <= 5, 2, temp_df_issp_all_refUS$partyid_3cat)
table(temp_df_issp_all_refUS$PARTY_LR,temp_df_issp_all_refUS$partyid_3cat)
# make factor var
cro_cpct(temp_df_issp_all_refUS$partyid_3cat,  weight = temp_df_issp_all_refUS$weight)
temp_df_issp_all_refUS$partyid_3cat_num <- temp_df_issp_all_refUS$partyid_3cat
cro_cpct(temp_df_issp_all_refUS$partyid_3cat_num,  weight = temp_df_issp_all_refUS$weight)
temp_df_issp_all_refUS$partyid_3cat <- as.factor(temp_df_issp_all_refUS$partyid_3cat_num)
cro_cpct(temp_df_issp_all_refUS$partyid_3cat,  weight = temp_df_issp_all_refUS$weight)
class(temp_df_issp_all_refUS$partyid_3cat)
levels(temp_df_issp_all_refUS$partyid_3cat) <- list(Left = "0", Center = "1", Right = "2")
cro_cpct(temp_df_issp_all_refUS$partyid_3cat,  weight = temp_df_issp_all_refUS$weight)

# run analyses with controls
options(scipen = 500)
rm(lmcountryorder_market_controls)
lmcountryorder_market_controls <- lm(respineq_market ~ age + sexR + degree_2cat + partyid_3cat + finc_4cat + country_name, 
                                            data=temp_df_issp_all_refUS) 
lmcountryorder_market_controls <- lm_robust(respineq_market ~ country_name + age + sexR + degree_2cat + finc_4cat + partyid_3cat, 
                                           weights=weight, data=temp_df_issp_all_refUS) 
summary(lmcountryorder_market_controls)

# table not used
#tab_model(
#  lmcountryorder_market_controls, 
#  #auto.label = TRUE,
#  show.reflvl = TRUE,
#  dv.labels = c("Market Predistribution"),
#  show.se=TRUE,
#  show.ci=FALSE
#)

# run analyses in Stata (see Stata code file)
write_dta(temp_df_issp_all_refUS,"/Users/lesliemccall/Documents/McCallLocalFiles/issppaper/issp2019_allcountries_allcodedvars_may2024.dta")
