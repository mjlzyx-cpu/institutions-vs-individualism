# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 3 in Analysis 1
# Code also for: Analysis 1 Robustness Checks in Tables S3 and S4 (in SM C)
# Data files used:
# "GSS2021.dta"

# Read in data file/import dataset from Stata data file
# Data file is: "GSS2021.dta"

# rm(gss2021)
gss2021 <- GSS2021

# recode variables

# binary variable construction of forced choice question

gss2021$respineq_bus <- ifelse( (gss2021$respineq>=2 & gss2021$respineq<=6), 0, gss2021$respineq )
table(gss2021$respineq, gss2021$respineq_bus)
summary(gss2021$respineq_bus)
summary(gss2021$respineq)

gss2021$respineq_gov <- ifelse( (gss2021$respineq %in% c(1,3,4,5,6)), 0, gss2021$respineq )
gss2021$respineq_gov <- ifelse( (gss2021$respineq_gov == 2), 1, gss2021$respineq_gov )
table(gss2021$respineq, gss2021$respineq_gov)
summary(gss2021$respineq_gov)
summary(gss2021$respineq)

gss2021$respineq_none <- ifelse( (gss2021$respineq>=1 & gss2021$respineq<=5), 0, gss2021$respineq )
gss2021$respineq_none <- ifelse( (gss2021$respineq_none == 6), 1, gss2021$respineq_none )
table(gss2021$respineq, gss2021$respineq_none)
summary(gss2021$respineq_none)
summary(gss2021$respineq)

gss2021$respineq_low <- ifelse( (gss2021$respineq %in% c(1,2,3,4,6)), 0, gss2021$respineq )
gss2021$respineq_low <- ifelse( (gss2021$respineq_low == 5), 1, gss2021$respineq_low )
table(gss2021$respineq, gss2021$respineq_low)
summary(gss2021$respineq_low)
summary(gss2021$respineq)

gss2021$respineq_high <- ifelse( (gss2021$respineq %in% c(1,2,3,5,6)), 0, gss2021$respineq )
gss2021$respineq_high <- ifelse( (gss2021$respineq_high == 4), 1, gss2021$respineq_high )
table(gss2021$respineq, gss2021$respineq_high)
summary(gss2021$respineq_high)
summary(gss2021$respineq)

gss2021$respineq_union <- ifelse( (gss2021$respineq %in% c(1,2,4,5,6)), 0, gss2021$respineq )
gss2021$respineq_union <- ifelse( (gss2021$respineq_union == 3), 1, gss2021$respineq_union )
table(gss2021$respineq, gss2021$respineq_union)
summary(gss2021$respineq_union)
summary(gss2021$respineq)

by(gss2021$respineq_gov, gss2021$degree, summary)

# weight variable

gss2021$weight <- gss2021$wtssnrps

# covariates

# education, grouped into 2 cats using degree variable
gss2021$degree_2cat <- NA
gss2021$degree_2cat <- ifelse(gss2021$degree %in% c(0,1,2),0,gss2021$degree_2cat)
gss2021$degree_2cat <- ifelse(gss2021$degree %in% c(3,4),1,gss2021$degree_2cat)
table(gss2021$degree_2cat,gss2021$degree)
# make factor var
cro_cpct(gss2021$degree_2cat,  weight = gss2021$wtssnrps)
gss2021$degree_2cat_num <- gss2021$degree_2cat
cro_cpct(gss2021$degree_2cat_num,  weight = gss2021$wtssnrps)
gss2021$degree_2cat <- as.factor(gss2021$degree_2cat_num)
cro_cpct(gss2021$degree_2cat,  weight = gss2021$wtssnrps)
class(gss2021$degree_2cat)
levels(gss2021$degree_2cat) <- list('Less than BA' = "0", 'BA or more' = "1")
cro_cpct(gss2021$degree_2cat,  weight = gss2021$wtssnrps)

# income quartiles
# use family income
gss2021$finc_num <- NA
gss2021$finc_num <- ifelse(gss2021$income16==27, NA, gss2021$income16)
prop.table(table(gss2021$finc_num))
prop.table(table(gss2021$income16))
# use sur package
library(sur)
test_inc <- subset(gss2021, gss2021$income16!=27)
cumulative.table(test_inc$income16)
# rough quartile groupings are: 1-15; 16-19; 20-22; 23-26
gss2021$finc_4cat <- NA
gss2021$finc_4cat <- ifelse(gss2021$finc_num >= 1 & gss2021$finc_num <= 15, 0, gss2021$finc_4cat)
gss2021$finc_4cat <- ifelse(gss2021$finc_num >= 16 & gss2021$finc_num <= 19, 1, gss2021$finc_4cat)
gss2021$finc_4cat <- ifelse(gss2021$finc_num >= 20 & gss2021$finc_num <= 22, 2, gss2021$finc_4cat)
gss2021$finc_4cat <- ifelse(gss2021$finc_num >= 23 & gss2021$finc_num <= 26, 3, gss2021$finc_4cat)
cro_cpct(gss2021$finc_4cat, weight = gss2021$wtssnrps)
table(gss2021$income16,gss2021$finc_4cat)
# make factor var
gss2021$finc_4cat_num <- gss2021$finc_4cat
cro_cpct(gss2021$finc_4cat_num,  weight = gss2021$wtssnrps)
gss2021$finc_4cat <- as.factor(gss2021$finc_4cat_num)
cro_cpct(gss2021$finc_4cat,  weight = gss2021$wtssnrps)
class(gss2021$finc_4cat)
levels(gss2021$finc_4cat) <- list('Quartile 1' = "0", 'Quartile 2' = "1", 'Quartile 3' = "2", 'Quartile 4' = "3")
cro_cpct(gss2021$finc_4cat,  weight = gss2021$wtssnrps)

# party id, grouped into 3 categories
gss2021$partyid_3cat <- NA
gss2021$partyid_3cat <- ifelse(gss2021$partyid >= 0 & gss2021$partyid <= 2, 0, gss2021$partyid_3cat)
gss2021$partyid_3cat <- ifelse(gss2021$partyid == 3, 1, gss2021$partyid_3cat)
gss2021$partyid_3cat <- ifelse(gss2021$partyid >= 4 & gss2021$partyid <= 6, 2, gss2021$partyid_3cat)
gss2021$partyid_3cat <- ifelse(gss2021$partyid == 7, 1, gss2021$partyid_3cat)
table(gss2021$partyid,gss2021$partyid_3cat)
# make factor var
cro_cpct(gss2021$partyid_3cat,  weight = gss2021$wtssnrps)
gss2021$partyid_3cat_num <- gss2021$partyid_3cat
cro_cpct(gss2021$partyid_3cat_num,  weight = gss2021$wtssnrps)
gss2021$partyid_3cat <- as.factor(gss2021$partyid_3cat_num)
cro_cpct(gss2021$partyid_3cat,  weight = gss2021$wtssnrps)
class(gss2021$partyid_3cat)
levels(gss2021$partyid_3cat) <- list(Democrat = "0", Independent = "1", Republican = "2")
cro_cpct(gss2021$partyid_3cat,  weight = gss2021$wtssnrps)

# race, grouped into 6 cats
# 1=white; 4-14=Asian; 2=black; 16=latinx; 3=Native Am.; 15=other
# recode to : white=0, black=1, latinx=2, asian=3, native american=4, other=5
gss2021$race_6cat <- NA
gss2021$race_6cat <- ifelse(gss2021$raceacs1 == 1, 0, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs4 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs5 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs6 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs7 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs8 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs9 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs10 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs11 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs12 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs13 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs14 == 1, 3, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs3 == 1, 4, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs15 == 1, 5, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs2 == 1, 1, gss2021$race_6cat)
gss2021$race_6cat <- ifelse(gss2021$raceacs16 == 1, 2, gss2021$race_6cat)
table(gss2021$race,gss2021$race_6cat)
prop.table(table(gss2021$race_6cat))
cro_cpct(gss2021$race_6cat, weight = gss2021$wtssnrps)
# make factor var
gss2021$race_6cat_num <- gss2021$race_6cat
cro_cpct(gss2021$race_6cat_num,  weight = gss2021$wtssnrps)
gss2021$race_6cat <- as.factor(gss2021$race_6cat_num)
cro_cpct(gss2021$race_6cat,  weight = gss2021$wtssnrps)
class(gss2021$race_6cat)
levels(gss2021$race_6cat) <- list(White = "0", Black = "1", Latinx = "2", Asian = "3", 'Native Am.' = "4", Other = "5")
cro_cpct(gss2021$race_6cat,  weight = gss2021$wtssnrps)

# binary gender
gss2021$sexR <- gss2021$sex
gss2021$sexR <- ifelse(gss2021$sex==1,0,gss2021$sex)
gss2021$sexR <- ifelse(gss2021$sex==2,1,gss2021$sexR)
table(gss2021$sex, gss2021$sexR)
summary(gss2021$sex)
summary(gss2021$sexR)
# make factor var
gss2021$sexR_num <- gss2021$sexR
cro_cpct(gss2021$sexR_num,  weight = gss2021$wtssnrps)
gss2021$sexR <- as.factor(gss2021$sexR_num)
cro_cpct(gss2021$sexR,  weight = gss2021$wtssnrps)
class(gss2021$sexR)
levels(gss2021$sexR) <- list(Men = "0", Women = "1")
cro_cpct(gss2021$sexR,  weight = gss2021$wtssnrps)

# begin building df for graph of forced choice by US subgroup

# factor variable labels in separate data frame
# k = index for number of categories

# rm(create_group_means)
create_group_means <- function(chart_data,chart_var0, factor_var0, weight_var0) {

  chart_var <- enquo(chart_var0)
  factor_var <- enquo(factor_var0)
  weight_var <- enquo(weight_var0)
  
  group_means <- data.frame(varlabel=character(), wtdmean=double())
  group_means
  
  # loop through groups

  # regular mean in original scale (including binary vars)
  for (i in 1:k) {
    group_means[i,1] <- facvarcodes$levels[i]
  }
  group_means
  
  # regular weighted mean in original scale (including binary vars)
  group_means <- chart_data %>%
    filter(!is.na(!!chart_var) & !is.na(!!factor_var)) %>%
    group_by(!!factor_var) %>%
    summarise(wtdmean = weighted.mean(!!chart_var,!!weight_var))
  group_means
  
  return(group_means)
}

create_stack_gov_order <- function() { 
  
  # label data and put in correct order for stacked chart
  group_means_gss_gov[,3] <- "Government"
  group_means_gss_bus[,3] <- "Private companies"
  group_means_gss_union[,3] <- "Unions"
  group_means_gss_high[,3] <- "High-income people"
  group_means_gss_low[,3] <- "Low-income people"
  group_means_gss_none[,3] <- "No reduction"
  
  rm(gov_order)
  gov_order <- group_means_gss_gov[,2]
  gov_order
  colnames(gov_order) <- c('wtdmeangov')
  gov_order
  
  # correct order by government 
  # rm(stack_gov,stack_bus,stack_union,stack_high,stack_low,stack_none)
  stack_gov <- cbind(group_means_gss_gov,gov_order)
  stack_bus <- cbind(group_means_gss_bus,gov_order)
  stack_union <- cbind(group_means_gss_union,gov_order)
  stack_high <- cbind(group_means_gss_high,gov_order)
  stack_low <- cbind(group_means_gss_low,gov_order)
  stack_none <- cbind(group_means_gss_none,gov_order)
  
  stack_gov <- stack_gov[order(stack_gov$wtdmeangov),]
  stack_bus <- stack_bus[order(stack_bus$wtdmeangov),]
  stack_union <- stack_union[order(stack_union$wtdmeangov),]
  stack_high <- stack_high[order(stack_high$wtdmeangov),]
  stack_low <- stack_low[order(stack_low$wtdmeangov),]
  stack_none <- stack_none[order(stack_none$wtdmeangov),]
  
  # rm(stack_gov_order)
  stack_gov_order <- rbind(stack_none, stack_low, stack_high, stack_bus, stack_union, stack_gov)
  stack_gov_order
  colnames(stack_gov_order) <- c('varlabel', 'wtdmean', 'fcoption', 'wtdmeangov')
  stack_gov_order$fcoption <- as.factor(stack_gov_order$fcoption)
  
  # set this to the k value above
  for (i in 1:k) {
    total_prop <- stack_gov_order[i,2] + stack_gov_order[i+k,2] + 
      stack_gov_order[i+(k*2),2] + stack_gov_order[i+(k*3),2] + 
      stack_gov_order[i+(k*4),2] + stack_gov_order[i+(k*5),2]
    print(total_prop)
  }
  
  group_means_gss_gov <- NULL
  group_means_gss_bus <- NULL
  group_means_gss_union <- NULL
  group_means_gss_high <- NULL
  group_means_gss_low <- NULL
  group_means_gss_none <- NULL
  
  return(stack_gov_order)
  
}

# call the functions here for each forced choice option
# store group means in new data frame
# then combine and sort by gov mean

# party id

facvarcodes <- attributes(gss2021$partyid_3cat)
k <- NULL
k <- length(facvarcodes$levels)
k
group_means_gss_bus <- create_group_means(chart_data=gss2021, chart_var0=respineq_bus, factor_var=partyid_3cat, weight_var0=weight)
group_means_gss_gov <- create_group_means(chart_data=gss2021, chart_var0=respineq_gov, factor_var=partyid_3cat, weight_var0=weight)
group_means_gss_union <- create_group_means(chart_data=gss2021, chart_var0=respineq_union, factor_var=partyid_3cat, weight_var0=weight)
group_means_gss_high <- create_group_means(chart_data=gss2021, chart_var0=respineq_high, factor_var=partyid_3cat, weight_var0=weight)
group_means_gss_low <- create_group_means(chart_data=gss2021, chart_var0=respineq_low, factor_var=partyid_3cat, weight_var0=weight)
group_means_gss_none <- create_group_means(chart_data=gss2021, chart_var0=respineq_none, factor_var=partyid_3cat, weight_var0=weight)

stack_gov_order_partyid <- create_stack_gov_order()
stack_gov_order_partyid

# education

facvarcodes <- attributes(gss2021$degree_2cat)
k <- NULL
k <- length(facvarcodes$levels)
k
group_means_gss_bus <- create_group_means(chart_data=gss2021, chart_var0=respineq_bus, factor_var=degree_2cat, weight_var0=weight)
group_means_gss_gov <- create_group_means(chart_data=gss2021, chart_var0=respineq_gov, factor_var=degree_2cat, weight_var0=weight)
group_means_gss_union <- create_group_means(chart_data=gss2021, chart_var0=respineq_union, factor_var=degree_2cat, weight_var0=weight)
group_means_gss_high <- create_group_means(chart_data=gss2021, chart_var0=respineq_high, factor_var=degree_2cat, weight_var0=weight)
group_means_gss_low <- create_group_means(chart_data=gss2021, chart_var0=respineq_low, factor_var=degree_2cat, weight_var0=weight)
group_means_gss_none <- create_group_means(chart_data=gss2021, chart_var0=respineq_none, factor_var=degree_2cat, weight_var0=weight)

stack_gov_order_degree <- create_stack_gov_order()
stack_gov_order_degree

# race

facvarcodes <- attributes(gss2021$race_6cat)
k <- NULL
k <- length(facvarcodes$levels)
k
group_means_gss_bus <- create_group_means(chart_data=gss2021, chart_var0=respineq_bus, factor_var=race_6cat, weight_var0=weight)
group_means_gss_gov <- create_group_means(chart_data=gss2021, chart_var0=respineq_gov, factor_var=race_6cat, weight_var0=weight)
group_means_gss_union <- create_group_means(chart_data=gss2021, chart_var0=respineq_union, factor_var=race_6cat, weight_var0=weight)
group_means_gss_high <- create_group_means(chart_data=gss2021, chart_var0=respineq_high, factor_var=race_6cat, weight_var0=weight)
group_means_gss_low <- create_group_means(chart_data=gss2021, chart_var0=respineq_low, factor_var=race_6cat, weight_var0=weight)
group_means_gss_none <- create_group_means(chart_data=gss2021, chart_var0=respineq_none, factor_var=race_6cat, weight_var0=weight)

stack_gov_order_race <- create_stack_gov_order()
stack_gov_order_race

# family income

facvarcodes <- attributes(gss2021$finc_4cat)
k <- NULL
k <- length(facvarcodes$levels)
k
group_means_gss_bus <- create_group_means(chart_data=gss2021, chart_var0=respineq_bus, factor_var=finc_4cat, weight_var0=weight)
group_means_gss_gov <- create_group_means(chart_data=gss2021, chart_var0=respineq_gov, factor_var=finc_4cat, weight_var0=weight)
group_means_gss_union <- create_group_means(chart_data=gss2021, chart_var0=respineq_union, factor_var=finc_4cat, weight_var0=weight)
group_means_gss_high <- create_group_means(chart_data=gss2021, chart_var0=respineq_high, factor_var=finc_4cat, weight_var0=weight)
group_means_gss_low <- create_group_means(chart_data=gss2021, chart_var0=respineq_low, factor_var=finc_4cat, weight_var0=weight)
group_means_gss_none <- create_group_means(chart_data=gss2021, chart_var0=respineq_none, factor_var=finc_4cat, weight_var0=weight)

stack_gov_order_finc <- create_stack_gov_order()
stack_gov_order_finc

# binary gender

facvarcodes <- attributes(gss2021$sexR)
k <- NULL
k <- length(facvarcodes$levels)
k
group_means_gss_bus <- create_group_means(chart_data=gss2021, chart_var0=respineq_bus, factor_var=sexR, weight_var0=weight)
group_means_gss_gov <- create_group_means(chart_data=gss2021, chart_var0=respineq_gov, factor_var=sexR, weight_var0=weight)
group_means_gss_union <- create_group_means(chart_data=gss2021, chart_var0=respineq_union, factor_var=sexR, weight_var0=weight)
group_means_gss_high <- create_group_means(chart_data=gss2021, chart_var0=respineq_high, factor_var=sexR, weight_var0=weight)
group_means_gss_low <- create_group_means(chart_data=gss2021, chart_var0=respineq_low, factor_var=sexR, weight_var0=weight)
group_means_gss_none <- create_group_means(chart_data=gss2021, chart_var0=respineq_none, factor_var=sexR, weight_var0=weight)

stack_gov_order_gender <- create_stack_gov_order()
stack_gov_order_gender

# now put them together into one graph using facet wrap
stack_gov_order_race_2 <- cbind(stack_gov_order_race,"race")
stack_gov_order_race_2
stack_gov_order_partyid_2 <- cbind(stack_gov_order_partyid,"party")
stack_gov_order_partyid_2
stack_gov_order_degree_2 <- cbind(stack_gov_order_degree,"degree")
stack_gov_order_degree_2
stack_gov_order_finc_2 <- cbind(stack_gov_order_finc,"finc")
stack_gov_order_finc_2
stack_gov_order_gender_2 <- cbind(stack_gov_order_gender,"gender")
stack_gov_order_gender_2

# need to make same variable name before rbind
stack_gov_order_race_2 <- rename(stack_gov_order_race_2, V5 = `"race"`)
stack_gov_order_partyid_2 <- rename(stack_gov_order_partyid_2, V5 = `"party"`)
stack_gov_order_degree_2 <- rename(stack_gov_order_degree_2, V5 = `"degree"`)
stack_gov_order_finc_2 <- rename(stack_gov_order_finc_2, V5 = `"finc"`)
stack_gov_order_gender_2 <- rename(stack_gov_order_gender_2, V5 = `"gender"`)

stack_gov_order_race_gender_degree_finc_partyid_2 <- rbind(stack_gov_order_race_2, stack_gov_order_gender_2, stack_gov_order_degree_2, stack_gov_order_finc_2, stack_gov_order_partyid_2)
stack_gov_order_race_gender_degree_finc_partyid_2

stack_gov_order_race_gender_degree_finc_partyid_2$V5 <- as.factor(stack_gov_order_race_gender_degree_finc_partyid_2$V5)

levels(stack_gov_order_race_gender_degree_finc_partyid_2$V5) = list('A. Race' = "race", 'B. Gender' = "gender", 'C. Degree' = "degree", 'D. Family income' = "finc", 'E. Party' = "party")

# comment out geom_text() line to remove value labels

# version without other race category because small N
stack_gov_order_race_gender_degree_finc_partyid_3 <- subset(stack_gov_order_race_gender_degree_finc_partyid_2, !varlabel=="Other")

# selected value labels only
# for government and business

#rm(stack_gov_order_race_gender_degree_finc_partyid_4)
stack_gov_order_race_gender_degree_finc_partyid_4 <- stack_gov_order_race_gender_degree_finc_partyid_3
stack_gov_order_race_gender_degree_finc_partyid_4$wtdmeanlabel <- "X"
stack_gov_order_race_gender_degree_finc_partyid_4$wtdmeanlabel <- ifelse(stack_gov_order_race_gender_degree_finc_partyid_4$fcoption %in% c("Government", "Private companies"), 
                                                                         rd(stack_gov_order_race_gender_degree_finc_partyid_4$wtdmean*100, digits=0),
                                                                         "X")

# make figure with vector-based high-res image and other journal specs for publication

# setup
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

# plot
#rm(articlefigure3)
articlefigure3 <- ggplot(data=stack_gov_order_race_gender_degree_finc_partyid_4, aes(fill=fct_inorder(fcoption),x=fct_inorder(varlabel), y=wtdmean*100)) + 
  geom_bar(position="stack", stat="identity", color = "white", linewidth = .5) +
  facet_grid(.~fct_inorder(V5), scale="free_x", space="free") + 
  geom_text(aes(label=ifelse(wtdmeanlabel!="X",paste(sprintf("%s", wtdmeanlabel)),"")), 
            color="white", family="PT Sans", fontface=2, size=2.0, hjust = .5, vjust = 1.5, position = "stack") +
  scale_fill_manual(values=c("lightsteelblue3", "lightsteelblue1","plum1","red2", "palevioletred3","royalblue4")) +
  theme_classic() +
  theme(legend.position = "right", 
        legend.title=element_blank(), 
        legend.text = element_text(size=7),
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(0.2, "cm"),
        strip.background = element_blank(), 
        strip.text = element_text(hjust=0, face="bold", size=9),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        text = element_text(family = "PT Sans")) +
  labs(y="Percent", x="") 

# save to file
# single column, same font, etc., sizing as article figure 2, which is 2 columns, so scale x 2 preserves relative 
# fonts sizes for single column vs. double column figures without changing sizing in ggplot command
articlefigure3
ggsave("ArticleFigure3.pdf", plot = articlefigure3, width = 3.55, height = 1.5, units = "in", dpi = 300, scale = 2.0)

# end of stacked bar chart for US data by social/political/economic groups

# SM C, Tables S3 and S4
# begin supplementary information analyses connected to Article Figure 3

# replace "Other" race with NA for analyses

prop.table(table(gss2021$race_6cat_num))
gss2021$race_6cat <- gss2021$race_6cat_num
gss2021$race_6cat <- ifelse(gss2021$race_6cat==5, NA, gss2021$race_6cat)
gss2021$race_6cat <- as.factor(gss2021$race_6cat)
class(gss2021$race_6cat)
levels(gss2021$race_6cat) <- list(White = "0", Black = "1", Latinx = "2", Asian = "3", 'Native Am.' = "4")
cro_cpct(gss2021$race_6cat,  weight = gss2021$wtssnrps)

# analyze variance among subgroups in responses to force choice question

# start with private companies only
# rm(lmbus1,lmgov1)
# rm(logitbus1, logitgov1)

logitbus1 <- glm(respineq_bus ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, family=binomial(link="logit"), data=gss2021)
logitgov1 <- glm(respineq_gov ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, family=binomial(link="logit"), data=gss2021)

lmbus1 <- summary(lm(respineq_bus ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, weights = gss2021$wtssnrps, data=gss2021 ))
lmgov1 <- summary(lm(respineq_gov ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, weights = gss2021$wtssnrps, data=gss2021 ))

summary(logitbus1)
summary(logitgov1)
lmbus1
lmgov1

# now repeat business analyses with all 3 options under market predistribution

# do a three category respineq variable used later for multinomial logit

gss2021$respineq_3cat_multilog <- NA
gss2021$respineq_3cat_multilog <- ifelse(gss2021$respineq==2, 2, gss2021$respineq_3cat_multilog)
gss2021$respineq_3cat_multilog <- ifelse(gss2021$respineq %in% c(1,3,4), 1, gss2021$respineq_3cat_multilog)
gss2021$respineq_3cat_multilog <- ifelse(gss2021$respineq %in% c(5,6), 0, gss2021$respineq_3cat_multilog)
gss2021$respineq_3cat_fac_multilog <- as.factor(gss2021$respineq_3cat_multilog)
table(gss2021$respineq,gss2021$respineq_3cat_multilog)

# now convert to binary with all market predistribution options coded as 1

gss2021$respineq_allmarketresp <- gss2021$respineq_3cat_multilog
gss2021$respineq_allmarketresp <- ifelse(gss2021$respineq_3cat_multilog==2, 0, gss2021$respineq_3cat_multilog)
table(gss2021$respineq_3cat_multilog,gss2021$respineq_allmarketresp)

# rm(logitallmkt1,lmallmkt1)
logitallmkt1 <- glm(respineq_allmarketresp ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, family=binomial(link="logit"), data=gss2021)
lmallmkt1 <- summary(lm(respineq_allmarketresp ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, weights = gss2021$wtssnrps, data=gss2021 ))

summary(logitallmkt1)
lmallmkt1

# now repeat with robust standard errors
# need estimatr package for lm_robust

#rm(lmgov2, lmallmkt2)
lmgov2 <- lm_robust(respineq_gov ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, weights = gss2021$wtssnrps, data=gss2021 )
lmallmkt2 <- lm_robust(respineq_allmarketresp ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, weights = gss2021$wtssnrps, data=gss2021 )

summary(lmgov2)
summary(lmallmkt2)

# table makers
# library(sjPlot); library(sjlabelled); library(sjmisc)
# select all in viewer, and then select copy
# past into word, select the table, and set font=8, edit if necessary

# Table S3: LPM
# robust se's
tab_model(
  lmallmkt2, lmgov2, 
  pred.labels = c("Intercept", "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", 
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]"),
  dv.labels = c("Market Predistribution", "Government Redistribution"),
  show.se=TRUE
)

# FYI
# regular se's
tab_model(
  lmallmkt1, lmgov1, 
  pred.labels = c("Intercept", "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", 
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]"),
  dv.labels = c("Market Predistribution", "Government Redistribution"),
  show.se=TRUE
)

# FYI
# logits
tab_model(
  logitallmkt1, logitgov1, 
  pred.labels = c("Intercept", "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", 
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]"),
  dv.labels = c("Market Predistribution", "Government Redistribution"),
  show.se=TRUE
)

# Table S4: multinomial logit
# library(nnet)

mlogit1 <- multinom(respineq_3cat_fac_multilog ~ partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021)

# report log odds
# this table is hand edited in Word for presentation in the paper
tab_model(
  mlogit1, 
  pred.labels = c("Intercept", "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", 
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]",
                  "Intercept", "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", 
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]"),
  dv.labels = c("Individual Responsiblity vs. Market (1)/Govt (2)"),
  transform = NULL,
  show.se=TRUE
)

