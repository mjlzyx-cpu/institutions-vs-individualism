# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 6 in Analysis 3
# Code also for: Analysis 3 Robustness Checks in Tables S12, S14, S15, Figures S4, S5 (in SM E)
# Data files used:
# "GSS2021.dta"
# "Merged dataset with countries fielding supplementals (reduced variable set with South Africa included, with updated ZA-adjusted regular weight and alt_weight).sav"

#####################################
# main 4-item intervention and 
# market ideology analyses for the US
# 4-item intervention index is in 
# Article Figure 6, Panel A
# market ideology index is in 
# Figure S4, Panel A (SM E)
#####################################

# Read in data file/import dataset from Stata data file
# Data file is: "GSS2021.dta"

gss2021 <- GSS2021

### recode variables

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
levels(gss2021$finc_4cat) <- list('Quart 1 Inc' = "0", 'Quart 2 Inc' = "1", 'Quart 3 Inc' = "2", 'Quart 4 Inc' = "3")
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

# forced choice question

# for graphs, make the first group be government, followed by other institutions and individualism
gss2021$respineq2 <- ifelse(gss2021$respineq==1, 2, NA)
gss2021$respineq2 <- ifelse(gss2021$respineq==2, 1, gss2021$respineq2)
gss2021$respineq2 <- ifelse(gss2021$respineq %in% c(3,4,5,6), gss2021$respineq, gss2021$respineq2)
table(gss2021$respineq, gss2021$respineq2)

str(gss2021$respineq2)
levels(gss2021$respineq2)
gss2021$respineq2_fac <- as.factor(gss2021$respineq2)
levels(gss2021$respineq2_fac)

# market ideology and intervention indices

gss2021$inequal6R <- NA
str(gss2021$inequal6R)
gss2021$inequal6R <- ifelse( (gss2021$inequal6>=1 & gss2021$inequal6<=5), gss2021$inequal6, NA)
table(gss2021$inequal6R)
table(gss2021$inequal6R,gss2021$inequal6)
by(gss2021$inequal6R,gss2021$respineq, summary)
cro_cpct(gss2021$inequal6R, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

gss2021$inequal5R <- NA
str(gss2021$inequal5R)
gss2021$inequal5R <- ifelse( (gss2021$inequal5>=1 & gss2021$inequal5<=5), gss2021$inequal5, NA)
table(gss2021$inequal5R)
table(gss2021$inequal5R,gss2021$inequal5)
by(gss2021$inequal5R,gss2021$respineq, summary)
cro_cpct(gss2021$inequal5R, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

gss2021$boardrepR <- NA
str(gss2021$boardrepR)
gss2021$boardrepR <- ifelse( (gss2021$boardrep>=1 & gss2021$boardrep<=5), gss2021$boardrep, NA)
table(gss2021$boardrepR)
table(gss2021$boardrepR,gss2021$boardrep)
by(gss2021$boardrepR,gss2021$respineq, summary)
cro_cpct(gss2021$boardrepR, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

gss2021$upwagesR <- NA
str(gss2021$upwagesR)
gss2021$upwagesR <- ifelse( (gss2021$upwages>=1 & gss2021$upwages<=5), gss2021$upwages, NA)
table(gss2021$upwagesR)
table(gss2021$upwagesR,gss2021$upwages)
by(gss2021$upwagesR,gss2021$respineq, summary)
cro_cpct(gss2021$upwagesR, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

gss2021$limitpayR <- NA
str(gss2021$limitpayR)
gss2021$limitpayR <- ifelse( (gss2021$limitpay>=1 & gss2021$limitpay<=5), gss2021$limitpay, NA)
table(gss2021$limitpayR)
table(gss2021$limitpayR,gss2021$limitpay)
by(gss2021$limitpayR,gss2021$respineq, summary)
cro_cpct(gss2021$limitpayR, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

gss2021$trdunio1R <- NA
str(gss2021$trdunio1R)
gss2021$trdunio1R <- ifelse( (gss2021$trdunio1>=1 & gss2021$trdunio1<=5), gss2021$trdunio1, NA)
table(gss2021$trdunio1R)
table(gss2021$trdunio1R,gss2021$trdunio1)
by(gss2021$trdunio1R,gss2021$respineq, summary)
cro_cpct(gss2021$trdunio1R, list(total(), gss2021$respineq2_fac), weight = gss2021$wtssnrps)

### make two indices

# 0-1 scale for indices
gss2021$marketideol <- NA
gss2021$interventioin4 <- NA
gss2021$marketideol <- ((((gss2021$inequal6R+gss2021$inequal5R-2)/8)*(-1))+1)
gss2021$intervention4 <- ((((gss2021$trdunio1R+gss2021$boardrepR+gss2021$upwagesR+gss2021$limitpayR-4)/16)*(-1))+1)

### run some analyses in R with controls
### main analyses done in Stata with margins command (see line 220 and separate Stata command file)

# rm(ideologyindex)
# rm(interventionindex)

ideologyindex <- lm(marketideol ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )
interventionindex <- lm(intervention4 ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

# robust standard errors
interventionindex_robust <- lm_robust(intervention4 ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )
ideologyindex_robust <- lm_robust(marketideol ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

# use margins command in Stata for predicted values in Article Figure 6
# see "ISSP2019-Rcode-Public-ArticleFigure6-PlusSM-postStatagraphsonly.R" for predicted values input to Figure 6
# write_dta(gss2021,"/Users/lesliemccall/Documents/McCallLocalFiles/issppaper/gss2021_coded_march2024.dta")
write_dta(gss2021,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/gss2021_coded_aug2026_fig6afigS4a.dta")

#####################################
# replicate with 6 other countries
# (main 4 item intervention and 
# market ideology analyses) 
# 4-item intervention index is in 
# Article Figure 6, Panel D
# market ideology index is in 
# Figure S4, Panel B (SM E)
#####################################

# select only countries with supplemental data

issp2019_supplement <- Merged_dataset_with_countries_fielding_supplementals_reduced_variable_set_with_South_Africa_included_with_updated_ZA_adjusted_regular_weight_and_alt_weight_
# this is just a check
issp2019_supplement <- subset(issp2019_supplement, country %in% c(152,352,554,710,752,756,840))

### recode variables

# weights
# weights are disproportionately large in SA, but fixed in latest dataset

issp2019_supplement$weight <- issp2019_supplement$WEIGHT
by(issp2019_supplement$weight, issp2019_supplement$country, summary)

# country2 is only supplemental countries

issp2019_supplement$country2 <- NA
issp2019_supplement$country2 <- ifelse(issp2019_supplement$country %in% c(152,352,554,710,752,756,840), issp2019_supplement$country, NA)

issp2019_supplement$country2 <- as.factor(issp2019_supplement$country2)
cro_cpct(issp2019_supplement$country2)
class(issp2019_supplement$country2)
levels(issp2019_supplement$country2) <- list('Chile' = "152", 'Iceland' = "352", 'New Zealand' = "554", 
                                             'South Africa' = "710", 'Sweden' = "752", 'Switzerland' = "756", 
                                             'United States' = "840")

cro_cpct(issp2019_supplement$country2)
cro_cpct(issp2019_supplement$country2, weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$country2, weight = issp2019_supplement$altweight)

# covariates

# age
issp2019_supplement$age <- issp2019_supplement$AGE

# binary gender
issp2019_supplement$sexR <- issp2019_supplement$SEX
issp2019_supplement$sexR <- ifelse(issp2019_supplement$SEX==1,0,issp2019_supplement$SEX)
issp2019_supplement$sexR <- ifelse(issp2019_supplement$SEX==2,1,issp2019_supplement$sexR)
table(issp2019_supplement$SEX, issp2019_supplement$sexR)
summary(issp2019_supplement$SEX)
summary(issp2019_supplement$sexR)
# make factor var
issp2019_supplement$sexR_num <- issp2019_supplement$sexR
cro_cpct(issp2019_supplement$sexR_num,  weight = issp2019_supplement$weight)
issp2019_supplement$sexR <- as.factor(issp2019_supplement$sexR_num)
cro_cpct(issp2019_supplement$sexR,  weight = issp2019_supplement$weight)
class(issp2019_supplement$sexR)
levels(issp2019_supplement$sexR) <- list(Men = "0", Women = "1")
cro_cpct(issp2019_supplement$sexR,  weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$SEX,  weight = issp2019_supplement$weight)

# education, grouped into 2 cats using degree variable
issp2019_supplement$degree_2cat <- NA
issp2019_supplement$degree_2cat <- ifelse((issp2019_supplement$DEGREE>=0 & issp2019_supplement$DEGREE<=6), issp2019_supplement$DEGREE, NA)
table(issp2019_supplement$degree_2cat)
summary(issp2019_supplement$degree_2cat)
issp2019_supplement$degree_2cat <- ifelse(issp2019_supplement$DEGREE >= 0 & issp2019_supplement$DEGREE <= 3, 0, issp2019_supplement$degree_2cat)
issp2019_supplement$degree_2cat <- ifelse(issp2019_supplement$DEGREE >= 4 & issp2019_supplement$DEGREE <= 6, 1, issp2019_supplement$degree_2cat)
table(issp2019_supplement$degree_2cat)
table(issp2019_supplement$DEGREE,issp2019_supplement$degree_2cat)
cro_cpct(issp2019_supplement$degree_2cat, weight = issp2019_supplement$weight)
# make factor var
issp2019_supplement$degree_2cat_num <- issp2019_supplement$degree_2cat
cro_cpct(issp2019_supplement$degree_2cat_num,  weight = issp2019_supplement$weight)
issp2019_supplement$degree_2cat <- as.factor(issp2019_supplement$degree_2cat_num)
cro_cpct(issp2019_supplement$degree_2cat,  weight = issp2019_supplement$weight)
class(issp2019_supplement$degree_2cat)
levels(issp2019_supplement$degree_2cat) <- list('Less than BA' = "0", 'BA or more' = "1")
cro_cpct(issp2019_supplement$degree_2cat,  weight = issp2019_supplement$weight)

# income quartiles
# use family income
issp2019_supplement$finc_4cat <- NA
issp2019_supplement$finc_4cat <- ifelse(issp2019_supplement$Phinc >= 0 & issp2019_supplement$Phinc <= 25, 0, issp2019_supplement$finc_4cat)
issp2019_supplement$finc_4cat <- ifelse(issp2019_supplement$Phinc >= 26 & issp2019_supplement$Phinc <= 50, 1, issp2019_supplement$finc_4cat)
issp2019_supplement$finc_4cat <- ifelse(issp2019_supplement$Phinc >= 51 & issp2019_supplement$Phinc <= 75, 2, issp2019_supplement$finc_4cat)
issp2019_supplement$finc_4cat <- ifelse(issp2019_supplement$Phinc >= 76 & issp2019_supplement$Phinc <= 100, 3, issp2019_supplement$finc_4cat)
cro_cpct(issp2019_supplement$finc_4cat, weight = issp2019_supplement$weight)
table(issp2019_supplement$Phinc,issp2019_supplement$finc_4cat)
# make factor var
issp2019_supplement$finc_4cat_num <- issp2019_supplement$finc_4cat
cro_cpct(issp2019_supplement$finc_4cat_num,  weight = issp2019_supplement$weight)
issp2019_supplement$finc_4cat <- as.factor(issp2019_supplement$finc_4cat_num)
cro_cpct(issp2019_supplement$finc_4cat,  weight = issp2019_supplement$weight)
class(issp2019_supplement$finc_4cat)
levels(issp2019_supplement$finc_4cat) <- list('Quart 1 Inc' = "0", 'Quart 2 Inc' = "1", 'Quart 3 Inc' = "2", 'Quart 4 Inc' = "3")
cro_cpct(issp2019_supplement$finc_4cat,  weight = issp2019_supplement$weight)

# party id, grouped into 3 categories
issp2019_supplement$partyid_3cat <- NA
issp2019_supplement$partyid_3cat <- ifelse(issp2019_supplement$PARTY_LR >= 1 & issp2019_supplement$PARTY_LR <= 2, 0, issp2019_supplement$partyid_3cat)
issp2019_supplement$partyid_3cat <- ifelse(issp2019_supplement$PARTY_LR %in% c(3,6), 1, issp2019_supplement$partyid_3cat)
issp2019_supplement$partyid_3cat <- ifelse(issp2019_supplement$PARTY_LR >= 4 & issp2019_supplement$PARTY_LR <= 5, 2, issp2019_supplement$partyid_3cat)
table(issp2019_supplement$PARTY_LR,issp2019_supplement$partyid_3cat)
# make factor var
cro_cpct(issp2019_supplement$partyid_3cat,  weight = issp2019_supplement$weight)
issp2019_supplement$partyid_3cat_num <- issp2019_supplement$partyid_3cat
cro_cpct(issp2019_supplement$partyid_3cat_num,  weight = issp2019_supplement$weight)
issp2019_supplement$partyid_3cat <- as.factor(issp2019_supplement$partyid_3cat_num)
cro_cpct(issp2019_supplement$partyid_3cat,  weight = issp2019_supplement$weight)
class(issp2019_supplement$partyid_3cat)
levels(issp2019_supplement$partyid_3cat) <- list(Left = "0", Center = "1", Right = "2")
cro_cpct(issp2019_supplement$partyid_3cat,  weight = issp2019_supplement$weight)

# forced choice question

# for some graphs, make the first group be government, followed by other institutions and individualism
# make it a factor variable

cro_cpct(issp2019_supplement$v25, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
issp2019_supplement$respineq <- issp2019_supplement$v25
issp2019_supplement$respineq2 <- ifelse(issp2019_supplement$respineq==1, 2, NA)
issp2019_supplement$respineq2 <- ifelse(issp2019_supplement$respineq==2, 1, issp2019_supplement$respineq2)
issp2019_supplement$respineq2 <- ifelse(issp2019_supplement$respineq %in% c(3,4,5,6), issp2019_supplement$respineq, issp2019_supplement$respineq2)
table(issp2019_supplement$respineq, issp2019_supplement$respineq2)
cro_cpct(issp2019_supplement$respineq2, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

str(issp2019_supplement$respineq2)
levels(issp2019_supplement$respineq2)
issp2019_supplement$respineq2_fac <- as.factor(issp2019_supplement$respineq2)
levels(issp2019_supplement$respineq2_fac)

# supplemental policy questions

# view the data
cro_cpct(issp2019_supplement$supmar1, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar2, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar3, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar4, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar5, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar6, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar7, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar8, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar9, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

# recoding
issp2019_supplement$inequal6 <- issp2019_supplement$supmar5
issp2019_supplement$inequal6R <- NA
str(issp2019_supplement$inequal6R)
issp2019_supplement$inequal6R <- ifelse( (issp2019_supplement$inequal6>=1 & issp2019_supplement$inequal6<=5), issp2019_supplement$inequal6, NA)
table(issp2019_supplement$inequal6R)
table(issp2019_supplement$inequal6R,issp2019_supplement$inequal6)
by(issp2019_supplement$inequal6R,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$inequal6R, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar5, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

issp2019_supplement$inequal5 <- issp2019_supplement$supmar9
issp2019_supplement$inequal5R <- NA
str(issp2019_supplement$inequal5R)
issp2019_supplement$inequal5R <- ifelse( (issp2019_supplement$inequal5>=1 & issp2019_supplement$inequal5<=5), issp2019_supplement$inequal5, NA)
table(issp2019_supplement$inequal5R)
table(issp2019_supplement$inequal5R,issp2019_supplement$inequal5)
by(issp2019_supplement$inequal5R,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$inequal5R, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar9, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
# CL, NZ, SA missing on inequal5

issp2019_supplement$boardrep <- issp2019_supplement$supmar2
issp2019_supplement$boardrepR <- NA
str(issp2019_supplement$boardrepR)
issp2019_supplement$boardrepR <- ifelse( (issp2019_supplement$boardrep>=1 & issp2019_supplement$boardrep<=5), issp2019_supplement$boardrep, NA)
table(issp2019_supplement$boardrepR)
table(issp2019_supplement$boardrepR,issp2019_supplement$boardrep)
by(issp2019_supplement$boardrepR,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$boardrepR, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar2, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

issp2019_supplement$upwages <- issp2019_supplement$supmar3
issp2019_supplement$upwagesR <- NA
str(issp2019_supplement$upwagesR)
issp2019_supplement$upwagesR <- ifelse( (issp2019_supplement$upwages>=1 & issp2019_supplement$upwages<=5), issp2019_supplement$upwages, NA)
table(issp2019_supplement$upwagesR)
table(issp2019_supplement$upwagesR,issp2019_supplement$upwages)
by(issp2019_supplement$upwagesR,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$upwagesR, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar3, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

issp2019_supplement$limitpay <- issp2019_supplement$supmar4
issp2019_supplement$limitpayR <- NA
str(issp2019_supplement$limitpayR)
issp2019_supplement$limitpayR <- ifelse( (issp2019_supplement$limitpay>=1 & issp2019_supplement$limitpay<=5), issp2019_supplement$limitpay, NA)
table(issp2019_supplement$limitpayR)
table(issp2019_supplement$limitpayR,issp2019_supplement$limitpay)
by(issp2019_supplement$limitpayR,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$limitpayR, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar4, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
# Switzerland missing on limitpay

issp2019_supplement$trdunio1 <- issp2019_supplement$supmar1
issp2019_supplement$trdunio1R <- NA
str(issp2019_supplement$trdunio1R)
issp2019_supplement$trdunio1R <- ifelse( (issp2019_supplement$trdunio1>=1 & issp2019_supplement$trdunio1<=5), issp2019_supplement$trdunio1, NA)
table(issp2019_supplement$trdunio1R)
table(issp2019_supplement$trdunio1R,issp2019_supplement$trdunio1)
by(issp2019_supplement$trdunio1R,issp2019_supplement$respineq, summary)
cro_cpct(issp2019_supplement$trdunio1R, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$supmar1, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

# 0-1 scale
# adjust indices for missing country data
issp2019_supplement$interventioin4 <- NA
issp2019_supplement$intervention4 <- ifelse(issp2019_supplement$country2=="Switzerland",
                                            ((((issp2019_supplement$trdunio1R+
                                                  issp2019_supplement$boardrepR+
                                                  issp2019_supplement$upwagesR-3)/12)*(-1))+1),
                                            ((((issp2019_supplement$trdunio1R+
                                           issp2019_supplement$boardrepR+
                                           issp2019_supplement$upwagesR+
                                           issp2019_supplement$limitpayR-4)/16)*(-1))+1) )

issp2019_supplement$marketideol <- NA
issp2019_supplement$marketideol <- ifelse(issp2019_supplement$country %in% c(152,554,710),
                                          ((((issp2019_supplement$inequal6R-1)/4)*(-1))+1),
                                          ((((issp2019_supplement$inequal6R+issp2019_supplement$inequal5R-2)/8)*(-1))+1))

### some analyses in R, with all countries and country dummies 
# main analyses done in Stata with margins command (line 505)

ideologyindex_issp <- lm(marketideol ~ respineq2_fac + as.factor(country2), data=issp2019_supplement, weights = weight )
interventionindex_issp <- lm(intervention4 ~ respineq2_fac + as.factor(country2), data=issp2019_supplement, weights = weight )

### analyses, non-US only

# create new dataset with only 6 non-US countries
#rm(issp2019_supplement_nonUS)
issp2019_supplement_nonUS <- subset(issp2019_supplement, (country2!="United States"))
table(issp2019_supplement_nonUS$country2)

## analyses without country dummies
#rm(ideologyindex_issp, interventionindex_issp)
ideologyindex_issp <- lm(marketideol ~ respineq2_fac, data=issp2019_supplement_nonUS, weights = weight )
interventionindex_issp <- lm(intervention4 ~ respineq2_fac, data=issp2019_supplement_nonUS, weights = weight )

## analyses with country dummies (ZA adjusted weights)
rm(ideologyindex_issp, interventionindex_issp)

interventionindex_issp <- lm(intervention4 ~ respineq2_fac + country2, data=issp2019_supplement_nonUS, weights = weight )
summary(interventionindex_issp)

ideologyindex_issp <- lm(marketideol ~ respineq2_fac + country2, data=issp2019_supplement_nonUS, weights = weight )
summary(ideologyindex_issp)

## analyses with full controls
# education, income, age, binary gender, rural/urban, partisanship
rm(ideologyindex_issp_robust, interventionindex_issp_robust)

# robust standard errors
interventionindex_issp_robust <- lm(intervention4 ~ respineq2_fac + 
                               country2 + sexR + age + degree_2cat + 
                               finc_4cat + partyid_3cat + country2, 
                             data=issp2019_supplement_nonUS, weights = weight )

ideologyindex_issp_robust <- lm(marketideol ~ respineq2_fac + 
                           country2 + sexR + age + degree_2cat + 
                           finc_4cat + partyid_3cat + country2, 
                         data=issp2019_supplement_nonUS, weights = weight )


# use margins command in Stata for predicted values in Article Figure 6
# see "ISSP2019-Rcode-Public-ArticleFigure6-PlusSM-postStatagraphsonly.R" for predicted values input to Figure 6

# there is a illegal character in variable name filter_$
names(issp2019_supplement_nonUS)
view_df(issp2019_supplement_nonUS)
issp2019_supplement_nonUS <- issp2019_supplement_nonUS[,-c(41)]
write_dta(issp2019_supplement_nonUS,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/issp2019_nonUS_coded_aug2026_fig6dfigS4b.dta")

#####################################
# SM Tables S12, S14, S15 (SM E)
# descriptive distributions of 
# policy variables (Table S12) 
# multiple regression results 
# for 4-item intervention index
# and market ideology index 
# in Table S14 (US) and
# Table S15 (non-US)
#####################################

### SM Table S12  percentage distribution of policy questions by country

#rm(percentdistrib_table, issp2019_supplement_table)
issp2019_supplement_table <- drop_unused_labels(issp2019_supplement)

expss_output_viewer()
percentdistrib_table = issp2019_supplement_table %>% 
  cross_cpct(
    cell_vars = list(supmar5,supmar9,supmar1,supmar2,supmar3,supmar4),
    col_vars = list(total(), country2),
    weight = weight
  ) %>% 
  set_caption("")
percentdistrib_table

### SM Table S14 and S15 with full model results that graphs are based on 

# Table S14: US
tab_model(
  interventionindex_robust, ideologyindex_robust, 
  pred.labels = c("Intercept", 
                  "Priv. Comp. [vs. Gov.]", "Unions [vs. Gov.]", "High-inc. Ind. [vs. Gov.]",
                  "Low-inc. Ind. [vs. Gov.]","No reduce [vs. Gov.]",
                  "Independent [vs. Dem.]", "Republican [vs. Dem.]", "Black [vs. White]",
                  "Latinx [vs. White]", "Asian [vs. White]", "Native American [vs. White]", "Other [vs. White]",
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                  "BA or more [vs. Less]", "Age", "Female [vs. Male]"),
  dv.labels = c("Market Intervention Index", "Market Ideology Index"),
  show.se=TRUE
)

# Table S15: nonUS
tab_model(
  interventionindex_issp_robust, ideologyindex_issp_robust, 
  pred.labels = c("Intercept", "Priv. Comp. [vs. Gov.]", "Unions [vs. Gov.]", "High-inc. Ind. [vs. Gov.]",
                  "Low-inc. Ind. [vs. Gov.]","No reduce [vs. Gov.]",
                  "Iceland [vs. Chile]","New Zealand [vs. Chile]","South Africa [vs. Chile]",
                  "Sweden [vs. Chile]","Switzerland [vs. Chile]",
                  "Female [vs. Male]", "Age", "BA or more [vs. Less]",
                  "Quart 2 Income [vs. Q1]", "Quart 3 Income [vs. Q1]", "Quart 4 Income [vs. Q1]",
                   "Center [vs. Left]", "Right [vs. Left]"),
  dv.labels = c("Market Intervention Index", "Market Ideology Index"),
  show.se=TRUE
)

#####################################
# sub 2-item indices for government 
# and market items separately in
# Article Figure 6, US, Panels B & C
#####################################

# 2/11/25 all code below modified to include 2 new indices
# three intervention indices total by splitting current intervention into 2 indices
gss2021$intervention2g <- NA
gss2021$intervention2m <- NA
gss2021$intervention2g <- ((((gss2021$upwagesR+gss2021$limitpayR-2)/8)*(-1))+1)
gss2021$intervention2m <- ((((gss2021$trdunio1R+gss2021$boardrepR-2)/8)*(-1))+1)

### run analyses with controls

rm(interventionindex2g_robust, interventionindex2m_robust)

# add analyses with intervention split into 2 indices
interventionindex2g_robust <- lm_robust(intervention2g ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )
interventionindex2m_robust <- lm_robust(intervention2m ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

### replicate with Stata in margins
write_dta(gss2021,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/gss2021_coded_aug2026_fig6bc.dta")

#####################################
# sub 2-item indices for government 
# and market items separately in
# Article Figure 6, non-US, 
# Panels E & F
#####################################

# three intervention indices by splitting current intervention into 2 indices
issp2019_supplement_nonUS$intervention2g <- NA
issp2019_supplement_nonUS$intervention2m <- NA

issp2019_supplement_nonUS$intervention2g <- ifelse(issp2019_supplement_nonUS$country2=="Switzerland",
                                             ((((issp2019_supplement_nonUS$upwagesR-1)/4)*(-1))+1),
                                             ((((issp2019_supplement_nonUS$upwagesR+
                                                   issp2019_supplement_nonUS$limitpayR-2)/8)*(-1))+1))

issp2019_supplement_nonUS$intervention2m <- ifelse(issp2019_supplement_nonUS$country2=="Switzerland",
                                             ((((issp2019_supplement_nonUS$trdunio1R+
                                                   issp2019_supplement_nonUS$boardrepR-2)/8)*(-1))+1),
                                             ((((issp2019_supplement_nonUS$trdunio1R+
                                                   issp2019_supplement_nonUS$boardrepR+-2)/8)*(-1))+1) )

### final analyses with countries and full controls
# intervention sub-indices included 
# education, income, age, binary gender, partisanship, country

rm(interventionindex2g_issp_robust, interventionindex2m_issp_robust)

# robust standard errors
interventionindex2g_issp_robust <- lm_robust(intervention2g ~ respineq2_fac + 
                                      country2 + sexR + age + degree_2cat + 
                                      finc_4cat + partyid_3cat + country2, 
                                    data=issp2019_supplement_nonUS, weights = weight )

interventionindex2m_issp_robust <- lm_robust(intervention2m ~ respineq2_fac + 
                                  country2 + sexR + age + degree_2cat + 
                                  finc_4cat + partyid_3cat + country2, 
                                data=issp2019_supplement_nonUS, weights = weight )


# replicate with Stata in margins
#write_dta(issp2019_supplement_nonUS,"/Users/lesliemccall/Documents/McCallLocalFiles/issppaper/issp2019_nonUS_coded_march2024.dta")
write_dta(issp2019_supplement_nonUS,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/issp2019_nonUS_coded_aug2026_fig6ef.dta")

#####################################
# Article Figure 6 Robustness Checks
# in Figure S5 (SM E)
# support for government unemployment  
# benefits, US
#####################################

# reverse code govunemp
gss2021$govunempRev <- dplyr::recode(gss2021$govunemp, `1`=1, `2`=.75, `3`=.50, `4`=.25, `5`=0)
table(gss2021$govunemp, gss2021$govunempRev)

rm(govunemp_all_robust)
govunemp_all_robust <- lm_robust(govunempRev ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

# no controls, checking missing values are handled properly
rm(govunemp_robust)
govunemp_robust <- lm_robust(govunempRev ~ respineq2_fac, data=gss2021, weights = wtssnrps )

gss2021 %>%
  group_by(respineq2_fac) %>%
  dplyr::summarize(govunempmeans = weighted.mean(govunempRev, w=wtssnrps, na.rm=T))

rm(rmmissdata_gss)
rmmissdata_gss <- gss2021 %>%
  filter(!if_any(c(govunempRev, respineq), is.na))

rmmissdata_gss %>%
  group_by(respineq2_fac) %>%
  dplyr::summarize(govunempmeans = weighted.mean(govunempRev, w=wtssnrps, na.rm=T))

# compare to intervention index 4
gss2021 %>%
  group_by(respineq2_fac) %>%
  dplyr::summarize(weighted.mean(intervention4, w=wtssnrps, na.rm=T))

### replicate with Stata in margins
write_dta(gss2021,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/gss2021_coded_aug2026_figS5.dta")

#####################################
# Article Figure 6 Robustness Checks
# in Figure S5 (SM E)
# support for government unemployment   
# benefits, non-US
#####################################

# need to first remove filter_$ variable, which is column 41
# also include US to test diffs in GSS vs. ISSP for US

issp2019_supplement$govunemp <- issp2019_supplement$v23
issp2019_supplement_clean <- issp2019_supplement[, -41]
# write_dta(issp2019_supplement_clean,"~/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/issp2019_supppolicies_coded_nov2025.dta")
write_dta(issp2019_supplement_clean,"/Users/lmccall/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/issp2019_supppolicies_coded_aug2026_figS5.dta")

# also do the analysis here in R

issp2019_supplement$govunempRev <- NA
str(issp2019_supplement$govunempRev)
issp2019_supplement$govunempRev <- ifelse( (issp2019_supplement$govunemp>=1 & issp2019_supplement$govunemp<=5), issp2019_supplement$govunemp, NA)
table(issp2019_supplement$govunempRev)
table(issp2019_supplement$govunempRev,issp2019_supplement$govunemp)
by(issp2019_supplement$govunempRev,issp2019_supplement$respineq2, summary)
cro_cpct(issp2019_supplement$govunempRev, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)
cro_cpct(issp2019_supplement$govunemp, list(total(), issp2019_supplement$country2), weight = issp2019_supplement$weight)

# redo with reversed coding
issp2019_supplement$govunempRev <- NA
issp2019_supplement$govunempRev <- dplyr::recode(issp2019_supplement$v23, `1`=1, `2`=.75, `3`=.50, `4`=.25, `5`=0)
table(issp2019_supplement$v23, issp2019_supplement$govunempRev)

# countries most similar to US in FC question distribution
rm(issp2019_supplement_nonUS, govunempRev_issp_robust)

# issp2019_supplement_nonUS <- subset(issp2019_supplement, (country2!="United States"))
issp2019_supplement_nonUS <- subset(issp2019_supplement, (country2=="New Zealand" | country2=="Switzerland"))
table(issp2019_supplement_nonUS$country2)

# include country dummies if more than one country
govunempRev_issp_robust <- lm_robust(govunempRev ~ respineq2_fac + 
                            country2 + sexR + age + degree_2cat + 
                            finc_4cat + partyid_3cat, 
                            data=issp2019_supplement_nonUS, weights = weight )
summary(govunempRev_issp_robust)

# check missing data; results are consistent with partyid and income missing and with no covariates
# pattern found originally in bivariate analysis

rm(govunempRev_issp_robust)
govunempRev_issp_robust <- lm_robust(govunempRev ~ respineq2_fac + 
                            country2 + sexR + age + degree_2cat, 
                            data=issp2019_supplement_nonUS, weights = weight )
summary(govunempRev_issp_robust)

# no country dummy
rm(govunempRev_issp_robust)
govunempRev_issp_robust <- lm_robust(govunempRev ~ respineq2_fac + 
                            sexR + age + degree_2cat + 
                            finc_4cat + partyid_3cat, 
                            data=issp2019_supplement_nonUS, weights = weight )

summary(govunempRev_issp_robust)

# Figure S5, Panels C and D, FC question distribution in x-axis labels

rm(NZ_CH_wtd_dist)
NZ_CH_wtd_dist <- issp2019_supplement %>%
  filter(country2 %in% c("New Zealand", "Switzerland")) %>%
  filter(!is.na(respineq2)) %>%
  dplyr::reframe(wpct(respineq2_fac, weight))
print.data.frame(NZ_CH_wtd_dist) 

# also check sample sizes at baseline

with(subset(issp2019_supplement, country==554), table(respineq2_fac))
with(subset(issp2019_supplement, country==756), table(respineq2_fac))
with(subset(issp2019_supplement, country %in% c(554,756)), table(respineq2_fac))


