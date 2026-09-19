# R script file for public repository
#
# Article: "Support for Government and Market Actors to Reduce Economic Inequality Outweighs 
# Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, R. González, 
# B. Milne, S. Ólafsdóttir, B. J. Roberts, M. Sapin, and J. C. Castillo. 
# 
# Code for: Figure 6 in Analysis 3 
# Code also for: Analysis 3 Robustness Checks in Figures S4 and S5 (in SM E)
# Figures are composed by hand from Stata output from the margins command

# Article Figure 6
# US
# 4-item index

#rm(ciintervenbyfcA, ciinterven2gbyfcB, ciinterven2mbyfcC, ciintervenbyfcD, ciinterven2gbyfcE, ciinterven2mbyfcF)

# n=1235
ciintervenbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciintervenbyfc$lb <- c(.7060,.6316,.6490,.5840,.4349,.4659)
ciintervenbyfc$ub <- c(.7427,.6727,.7438,.6760,.5332,.5418)
ciintervenbyfc$intervention <- c(.7244,.6521,.6964,.6300,.4841,.5039)
ciintervenbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciintervenbyfc$respineq2 <- c(1,2,3,4,5,6)
ciintervenbyfc$X1 <- "A. 4-Item Index, U.S."
ciintervenbyfc$X2 <- 0
ciintervenbyfc
ciintervenbyfcA <- ciintervenbyfc
ciintervenbyfcA
rm(ciintervenbyfc)

# US sub index with government only items
# n = 1266, weighted OLS with robust se
#rm(ciinterven2gbyfc)
ciinterven2gbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciinterven2gbyfc$lb <- c(.7148,.5981,.6304,.5732,.3775,.4210)
ciinterven2gbyfc$ub <- c(.7599,.6498,.7595,.6743,.4922,.5156)
ciinterven2gbyfc$intervention <- c(.7374,.6240,.6950,.6238,.4349,.4683)
ciinterven2gbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciinterven2gbyfc$respineq2 <- c(1,2,3,4,5,6)
ciinterven2gbyfc
ciinterven2gbyfc$X1 <- "B. 2-Item Gov. Actor Index, U.S."
ciinterven2gbyfc$X2 <- 0
ciinterven2gbyfc
ciinterven2gbyfcB <- ciinterven2gbyfc
ciinterven2gbyfcB
rm(ciinterven2gbyfc)

# US sub index with market only items
# n = 1257, weighted OLS with robust se
#rm(ciinterven2mbyfc)
ciinterven2mbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciinterven2mbyfc$lb <- c(.6830,.6557,.6678,.5636,.4793,.5070)
ciinterven2mbyfc$ub <- c(.7293,.6999,.7841,.7000,.5826,.5915)
ciinterven2mbyfc$intervention <- c(.7061,.6778,.7259,.6318,.5310,.5493)
ciinterven2mbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciinterven2mbyfc$respineq2 <- c(1,2,3,4,5,6)
ciinterven2mbyfc
ciinterven2mbyfc$X1 <- "C. 2-Item Market Actor Index, U.S."
ciinterven2mbyfc$X2 <- 0
ciinterven2mbyfc
ciinterven2mbyfcC <- ciinterven2mbyfc
ciinterven2mbyfcC
rm(ciinterven2mbyfc)

# non-US countries
# 4-item index
#rm(ciintervenbyfc)
#rm(ciintervenbyfcD)
ciintervenbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciintervenbyfc$lb <- c(.7477,.6984,.6945,.6478,.5238,.5193)
ciintervenbyfc$ub <- c(.7609,.7228,.7307,.7028,.5944,.5683)
ciintervenbyfc$intervention <- c(.7543,.7106,.7126,.6753,.5591,.5438)
ciintervenbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciintervenbyfc$respineq2 <- c(1,2,3,4,5,6)
ciintervenbyfc
ciintervenbyfc$X1 <- "D. 4-Item Index, Non-U.S."
ciintervenbyfc$X2 <- 1
ciintervenbyfc
ciintervenbyfcD <- ciintervenbyfc
ciintervenbyfcD
rm(ciintervenbyfc)

# graphs for two intervention sub-indices
# 512 x 512 from clipboard
# n = 5194, weighted OLS with robust se
#rm(ciinterven2gbyfc)
ciinterven2gbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciinterven2gbyfc$lb <- c(.7525,.6847,.6657,.6323,.4873,.4658)
ciinterven2gbyfc$ub <- c(.7687,.7152,.7122,.6986,.5745,.5293)
ciinterven2gbyfc$intervention <- c(.7606,.6999,.6890,.6654,.5309,.4976)
ciinterven2gbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciinterven2gbyfc$respineq2 <- c(1,2,3,4,5,6)
ciinterven2gbyfc
ciinterven2gbyfc$X1 <- "E. 2-Item Gov. Actor Index, Non-U.S."
ciinterven2gbyfc$X2 <- 1
ciinterven2gbyfc
ciinterven2gbyfcE <- ciinterven2gbyfc
ciinterven2gbyfcE
rm(ciinterven2gbyfc)

# n = 5166, weighted OLS with robust se
#rm(ciinterven2mbyfc)
ciinterven2mbyfc <- data.frame(matrix(nrow=6, ncol=5))
ciinterven2mbyfc$lb <- c(.7431,.7102,.7227,.6606,.5548,.5625)
ciinterven2mbyfc$ub <- c(.7579,.7367,.7599,.7196,.6294,.6172)
ciinterven2mbyfc$intervention <- c(.7505,.7234,.7413,.6901,.5921,.5898)
ciinterven2mbyfc$wtssnrps <- c(1,1,1,1,1,1)
ciinterven2mbyfc$respineq2 <- c(1,2,3,4,5,6)
ciinterven2mbyfc
ciinterven2mbyfc$X1 <- "F. 2-Item Market Actor Index, Non-U.S."
ciinterven2mbyfc$X2 <- 1
ciinterven2mbyfc
ciinterven2mbyfcF <- ciinterven2mbyfc
ciinterven2mbyfcF
rm(ciinterven2mbyfc)

# combine panels together
#rm(ciintervenbyfcA, ciinterven2gbyfcB, ciinterven2mbyfcC, ciintervenbyfcD, ciinterven2gbyfcE, ciinterven2mbyfcF)
allpanels <- rbind(ciintervenbyfcA, ciinterven2gbyfcB, ciinterven2mbyfcC, ciintervenbyfcD, ciinterven2gbyfcE, ciinterven2mbyfcF)
allpanels

# break up into two calls, one for each column

# make with vector-based high-res image and other journal specs for publication

# setup
options(device = "png")
font_add_google("PT Sans")
showtext_auto()

rm(columnUS, columnnonUS)

columnUS <- ggplot(data=allpanels[1:18,], aes(x = factor(respineq2), y = intervention, fill = factor(respineq2))) + 
  geom_col() +
  facet_wrap(.~fct_inorder(X1), ncol=1, dir="v") + 
  geom_text(aes(label=paste(sprintf("%0.2f", intervention))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 2.2, position = "stack") +
  geom_errorbar(data=allpanels[1:18,], aes(ymin=lb, ymax=ub), width=0.125, linewidth = 0.25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (38%)", "Private companies (29%)", "Unions (6%)", "High-income people (7%)", "Low-income people (7%)", "No reduction (11%)")) +  
  theme_classic() +
  theme(legend.position="none", 
        strip.background = element_blank(), 
        strip.text = element_text(hjust=0, face="bold", size=9),
        axis.line = element_line(color = "gray30", linewidth = .25),
        axis.ticks = element_line(color = "gray30", linewidth = .25),
        axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0.5, size=9, face="bold"),
        text = element_text(family = "PT Sans")) +
  labs(title="", y="Average marginal predicted value (0-1 scale)", x="")

columnnonUS <- ggplot(data=allpanels[19:36,], aes(x = factor(respineq2), y = intervention, fill = factor(respineq2))) + 
  geom_col() +
  facet_wrap(.~fct_inorder(X1), ncol=1, dir="v") + 
  geom_text(aes(label=paste(sprintf("%0.2f", intervention))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 2.2, position = "stack") +
  geom_errorbar(data=allpanels[19:36,], aes(ymin=lb, ymax=ub), width=0.125, linewidth=0.25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (60%)", "Private companies (20%)", "Unions (10%)", "High-income people (4%)", "Low-income people (2%)", "No reduction (5%)")) +  
  theme_classic() +
  theme(legend.position="none", 
        strip.background = element_blank(), 
        strip.text = element_text(hjust=0, face="bold", size=9),
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(size=8),
        #axis.title.y = element_text(size=8),
        axis.line.x = element_line(color = "gray30", linewidth = .25),
        axis.text.x = element_text(angle = 45, hjust = 1, size=8),
        axis.ticks.x = element_line(color = "gray30", linewidth = .25),
        axis.title.x = element_text(size=8), 
        plot.title = element_text(hjust = 0.5, size=9, face="bold"),
        text = element_text(family = "PT Sans")) +
  labs(title="", y="", x="")

rm(articlefigure6)
articlefigure6 <- columnUS + columnnonUS

# save to file
# 1 column width, no scale parameter needed 

articlefigure6
ggsave("ArticleFigure6.pdf", plot = articlefigure6, width = 5.60, height = 5.6, units = "in", dpi = 300)

######## SM analyses related to Article Figure 6

# SM E, Figure S4
# market ideology graphs

rm(cimarketideolbyfc)
cimarketideolbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cimarketideolbyfc$lb <- c(.4184,.4171,.4175,.3899,.5003,.5008)
cimarketideolbyfc$ub <- c(.4668,.4611,.5116,.5235,.5726,.5748)
cimarketideolbyfc$marketideol <- c(.4426,.4391,.4646,.4567,.5365,.5378)
cimarketideolbyfc$wtssnrps <- c(1,1,1,1,1,1)
cimarketideolbyfc$respineq2 <- c(1,2,3,4,5,6)
cimarketideolbyfc

rm(panelUS, panelnonUS)

panelUS <- ggplot(data=cimarketideolbyfc, aes(x = factor(respineq2), y = marketideol, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", marketideol))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.25, position = "stack") +
  geom_errorbar(data=cimarketideolbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +    
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (38%)", "Private companies (29%)", "Unions (6%)", "High-income people (7%)", "Low-income people (7%)", "No reduction (11%)")) +  
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
  labs(title="A. U.S.", y="Average marginal predicted value (0-1 scale)", x="")

rm(cimarketideolbyfc)
cimarketideolbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cimarketideolbyfc$lb <- c(.5130,.5224,.5196,.4520,.5788,.6274)
cimarketideolbyfc$ub <- c(.5314,.5570,.5643,.5294,.6585,.6799)
cimarketideolbyfc$marketideol <- c(.5222,.5397,.5420,.4907,.6187,.6536)
cimarketideolbyfc$weight <- c(1,1,1,1,1,1)
cimarketideolbyfc$respineq2 <- c(1,2,3,4,5,6)
cimarketideolbyfc

panelnonUS <- ggplot(data=cimarketideolbyfc, aes(x = factor(respineq2), y = marketideol, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", marketideol))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.25, position = "stack") +
  geom_errorbar(data=cimarketideolbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +    
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (60%)", "Private companies (20%)", "Unions (10%)", "High-income people (4%)", "Low-income people (2%)", "No reduction (5%)")) +  
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
  labs(title="B. Non-U.S.", y="", x="")

rm(articlefigureS4)
articlefigureS4 <- panelUS + panelnonUS

articlefigureS4 
ggsave("ArticleFigureS4.pdf", plot = articlefigureS4, width = 5.0, height = 3.5, units = "in", dpi = 300)

######## SM analyses related to Article Figure 6

# SM E, Figure S5
# unemployment benefits graphs

rm(govunemp_4GN, govunemp_6nonUS, govunemp_NZ_CH, govunemp_US, intervention4_NZ_CH)

rm(cigovunempbyfc)
cigovunempbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cigovunempbyfc$lb <- c(.6787,.5159,.5119,.5290,.3099,.3605)
cigovunempbyfc$ub <- c(.7293,.5776,.7025,.6574,.4290,.4671)
cigovunempbyfc$govunemp <- c(.7040,.5467,.6072,.5932,.3694,.4138)
cigovunempbyfc$wtssnrps <- c(1,1,1,1,1,1)
cigovunempbyfc$respineq2 <- c(1,2,3,4,5,6)
cigovunempbyfc

govunemp_US <- ggplot(data=cigovunempbyfc, aes(x = factor(respineq2), y = govunemp, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", govunemp))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.5, position = "stack") +
  geom_errorbar(data=cigovunempbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (38%)", "Private companies (29%)", "Unions (6%)", "High-income people (7%)", "Low-income people (7%)", "No reduction (11%)")) +  
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
  labs(title="A. Gov. Unemp. Benefits, U.S.", y="Average marginal predicted value (0-1 scale)", x="")

rm(cigovunempbyfc)
cigovunempbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cigovunempbyfc$lb <- c(.7326,.6653,.6334,.5805,.4784,.5606)
cigovunempbyfc$ub <- c(.7510,.6993,.6768,.6581,.5754,.6319)
cigovunempbyfc$govunemp <- c(.7418,.6823,.6551,.6193,.5269,.5962)
cigovunempbyfc$weight <- c(1,1,1,1,1,1)
cigovunempbyfc$respineq2 <- c(1,2,3,4,5,6)
cigovunempbyfc

govunemp_6nonUS <- ggplot(data=cigovunempbyfc, aes(x = factor(respineq2), y = govunemp, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", govunemp))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.5, position = "stack") +
  geom_errorbar(data=cigovunempbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (60%)", "Private companies (20%)", "Unions (10%)", "High-income people (4%)", "Low-income people (2%)", "No reduction (5%)")) +
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
  labs(title="B. Gov. Unemp. Benefits, Non-U.S.", y="", x="")

govunemp_US + govunemp_6nonUS

# repeat with NJ and CH only

rm(cigovunempbyfc)
cigovunempbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cigovunempbyfc$lb <- c(.6924,.6119,.5779,.5579,.3747,.4486)
cigovunempbyfc$ub <- c(.7288,.6516,.6916,.6444,.4963,.5704)
cigovunempbyfc$govunemp <- c(.7106,.6318,.6348,.6011,.4355,.5095)
cigovunempbyfc$weight <- c(1,1,1,1,1,1)
cigovunempbyfc$respineq2 <- c(1,2,3,4,5,6)
cigovunempbyfc

govunemp_NZ_CH <- ggplot(data=cigovunempbyfc, aes(x = factor(respineq2), y = govunemp, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", govunemp))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.5, position = "stack") +
  geom_errorbar(data=cigovunempbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (44%)", "Private companies (33%)", "Unions (7%)", "High-income people (6%)", "Low-income people (4%)", "No reduction (6%)")) +
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
  labs(title="C. Gov. Unemp. Benefits, NZ & CH", y="Average marginal predicted value (0-1 scale)", x="") 

# repeat with NJ and CH for intervention4 for comparison

rm(cigovunempbyfc)
cigovunempbyfc <- data.frame(matrix(NA, nrow=6, ncol=2))
cigovunempbyfc$lb <- c(.7490,.7057,.7378,.6722,.5336,.4962)
cigovunempbyfc$ub <- c(.7717,.7333,.8005,.7274,.6305,.5737)
cigovunempbyfc$govunemp <- c(.7603,.7195,.7692,.6998,.5820,.5350)
cigovunempbyfc$weight <- c(1,1,1,1,1,1)
cigovunempbyfc$respineq2 <- c(1,2,3,4,5,6)
cigovunempbyfc

intervention4_NZ_CH <- ggplot(data=cigovunempbyfc, aes(x = factor(respineq2), y = govunemp, fill = factor(respineq2))) + 
  geom_col() +
  geom_text(aes(label=paste(sprintf("%0.2f", govunemp))), 
            color="white", family="PT Sans", fontface=2, size=2.5, hjust = .5, vjust = 3.5, position = "stack") +
  geom_errorbar(data=cigovunempbyfc, aes(ymin=lb, ymax=ub), width=0.125, linewidth = .25) +
  scale_y_continuous(limits = c(0,1), breaks = c(0.0,0.25,.50,.75,1.0), oob = scales::squish) +
  scale_fill_manual(values=c("royalblue4", "red2", "palevioletred3","plum1", "lightsteelblue1","lightsteelblue3")) +
  scale_x_discrete(labels = c("Government (44%)", "Private companies (33%)", "Unions (7%)", "High-income people (6%)", "Low-income people (4%)", "No reduction (6%)")) +
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
  labs(title="D. 4-Item Mkt. Interven. Index, NZ & CH", y="", x="") 

govunemp_US + govunemp_6nonUS
govunemp_NZ_CH + intervention4_NZ_CH

rm(articlefigureS5top, articlefigureS5bot)
articlefigureS5top <- govunemp_US + govunemp_6nonUS
articlefigureS5bot <- govunemp_NZ_CH + intervention4_NZ_CH

rm(articlefigureS5)
articlefigureS5 <- articlefigureS5top/articlefigureS5bot
articlefigureS5
ggsave("ArticleFigureS5.pdf", plot = articlefigureS5, width = 5.25, height = 7.0, units = "in", dpi = 300)

