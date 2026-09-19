# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure S6 (in SM F)
# Data files used:
# "GSS2021-correctmissing.dta"

# Read in data file/import dataset from Stata data file
# Data file is: "GSS2021-correctmissing.dta"

gss2021 <- GSS2021_correctmissing

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
# then 7=can't choose options
gss2021$respineq2 <- NA
gss2021$respineq2 <- ifelse(gss2021$respineqm==1, 2, gss2021$respineq2)
gss2021$respineq2 <- ifelse(gss2021$respineqm==2, 1, gss2021$respineq2)
gss2021$respineq2 <- ifelse(gss2021$respineqm %in% c(3,4,5,6), gss2021$respineqm, gss2021$respineq2)
gss2021$respineq2 <- ifelse(gss2021$respineqm==10, 7, gss2021$respineq2)
table(gss2021$respineqm, gss2021$respineq2)

str(gss2021$respineq2)
levels(gss2021$respineq2)
gss2021$respineq2_fac <- as.factor(gss2021$respineq2)
levels(gss2021$respineq2_fac)

# get missing values

prop.table(table(gss2021$respineqm))
prop.table(table(gss2021$eqwlthm))
prop.table(table(gss2021$buseqincm))

prop.table(table(gss2021$inequal6m))
prop.table(table(gss2021$inequal5m))
prop.table(table(gss2021$trdunio1m))
prop.table(table(gss2021$upwagesm))
prop.table(table(gss2021$limitpaym))
prop.table(table(gss2021$boardrepm))

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

### make indices

# two indices
# 0-1 scale for indices
gss2021$marketideol <- NA
gss2021$intervention4 <- NA
gss2021$marketideol <- ((((gss2021$inequal6R+gss2021$inequal5R-2)/8)*(-1))+1)
gss2021$intervention4 <- ((((gss2021$trdunio1R+gss2021$boardrepR+gss2021$upwagesR+gss2021$limitpayR-4)/16)*(-1))+1)

### run analyses with controls

# rm(ideologyindex, interventionindex, ideologyindex_robust, interventionindex_robust)

ideologyindex <- lm(marketideol ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )
interventionindex4 <- lm(intervention4 ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

# robust standard errors (ggeffects does not work with robust se)
interventionindex_robust <- lm_robust(intervention4 ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )
ideologyindex_robust <- lm_robust(marketideol ~ respineq2_fac + partyid_3cat + race_6cat + finc_4cat + degree_2cat + age + sexR, data=gss2021, weights = wtssnrps )

interventionindex_robust
ideologyindex_robust

### replicate with Stata in margins
#write_dta(gss2021,"~/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/gss2021_coded_nov2025.dta")
#write_dta(gss2021,"~/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/gss2021_coded_oct2024.dta")
#write_dta(gss2021,"~/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/gss2021_coded_feb2025.dta")
#write_dta(gss2021,"~/Library/Mobile Documents/com~apple~CloudDocs/marketredis/newissp/publicfiles/finalversions/gss2021_coded_aug2026.dta")

###########################
# Article Figure S6 in SM F
# these graphs take data from margins command in Stata 
# they include the can't choose category of the forced-choice question

# here is the distribution for x-axis labels
cro_cpct(gss2021$respineq2_fac, weight = gss2021$wtssnrps)

# setup for journal printing
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

# n = 1435, weighted OLS with robust se and  r-squared adjusted = .16
rm(cimarketideolbyfc)
cimarketideolbyfc <- data.frame(matrix(nrow=7, ncol=5))
cimarketideolbyfc$lb <- c(.4203,.4192,.4188,.3928,.5041,.5040,.4445)
cimarketideolbyfc$ub <- c(.4686,.4622,.5147,.5286,.5752,.5768,.5004)
cimarketideolbyfc$marketideol <- c(.4444,.4407,.4668,.4607,.5397,.5404,.4724)
cimarketideolbyfc$wtssnrps <- c(1,1,1,1,1,1,1)
cimarketideolbyfc$respineq2 <- c(1,2,3,4,5,6,7)
cimarketideolbyfc

rm(marketideol_cantchoose)
marketideol_cantchoose <- ggplot(data=cimarketideolbyfc, aes(x = factor(respineq2), y = marketideol, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", marketideol))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.25, position = "stack") +
  geom_errorbar(data=cimarketideolbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +    
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3","goldenrod2")) +
  scale_x_discrete(labels = c("Government (32%)", "Private Companies (25%)", "Unions (5%)", "High-Income People (6%)", "Low-Income People (6%)", "No Reduction (10%)", "Cannot Choose (17%)")) +
  theme_classic() +
  theme(legend.position="none", 
        strip.background = element_blank(), 
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(size=8),
        #axis.title.y = element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0, size=9, face="bold"),
        text = element_text(family = "PT Sans")) +  
  labs(title="B. Market Ideology Index, U.S.", y="", x="")

# n = 1414, weighted OLS with robust se, r-squared adjusted = .35
rm(ciinterventionbyfc)
ciintervenbyfc <- data.frame(matrix(nrow=7, ncol=5))
ciintervenbyfc$lb <- c(.7053,.6305,.6484,.5810,.4328,.4655,.5845)
ciintervenbyfc$ub <- c(.7416,.6713,.7435,.6731,.5304,.5400,.6301)
ciintervenbyfc$intervention4 <- c(.7235,.6509,.6960,.6270,.4816,.5027,.6073)
ciintervenbyfc$wtssnrps <- c(1,1,1,1,1,1,1)
ciintervenbyfc$respineq2 <- c(1,2,3,4,5,6,7)
ciintervenbyfc

rm(intervention_cantchoose)
intervention_cantchoose <- ggplot(data=ciintervenbyfc, aes(x = factor(respineq2), y = intervention4, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", intervention4))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.25, position = "stack") +
  geom_errorbar(data=ciintervenbyfc, aes(ymin=lb, ymax=ub), width=0.1) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3","goldenrod2")) +
  scale_x_discrete(labels = c("Government (32%)", "Private Companies (25%)", "Unions (5%)", "High-Income People (6%)", "Low-Income People (6%)", "No Reduction (10%)", "Cannot Choose (17%)")) +  
  theme_classic() +
  theme(legend.position="none", 
        strip.background = element_blank(),
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0, size=9, face="bold"),
        text = element_text(family = "PT Sans")) +  
  labs(title="A. 4-Item Mkt. Interven. Index, U.S.", y="Average marginal predicted value (0-1 scale)", x="")

marketideol_cantchoose
intervention_cantchoose

rm(articlefigureS6)
articlefigureS6 <- intervention_cantchoose + marketideol_cantchoose
articlefigureS6 
ggsave("ArticleFigureS6.pdf", plot = articlefigureS6, width = 5.0, height = 3.5, units = "in", dpi = 300)


