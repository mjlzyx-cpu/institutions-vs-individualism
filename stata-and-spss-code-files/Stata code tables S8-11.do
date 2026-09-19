***Code for Supplemental Analyses, Tables S8 to 11 in "Support for Government and Market Actors to Reduce Economic Inequality Outweighs Norms of Individual Responsibility Worldwide" by A. Lindh, L. McCall, J. Edlund, B. J. Roberts, R. González, B. Milne, S. Ólafsdóttir,  M. Sapin, and J. C. Castillo. 

**Multilevel, multinomial regression with country random effects, using gsem command
**Note that DV (forced choice) is recoded so that Government option is coded 1 (treated as base outcome)

**Table S8
* Three aggregated forced-choice outcome groups (government base outcome) using marketization index as the main contextual explanatory variable

*Model 1 (without other country-level controls) 


gsem (fc3 <- c.marketfreedom fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial))


*Model 2 (with other country-level controls) 

gsem (fc3 <- c.marketfreedom gini govexp ud fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial) )

**Table S9
* Three aggregated forced-choice outcome groups (government base outcome) using GPD per capita as the main contextual explanatory variable

*Model 3 (without other country-level controls) 

gsem (fc3 <- c.zgdp fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial))


*Model 4 (with other country-level controls) 

gsem (fc3 <- c.zgdp gini govexp ud fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial))

**Table S10
* Six disaggregated forced-choice outcome groups (governemnt base outcome) using marketization as the main contextual explanatory variable

*Model 5 (without other country-level controls) 

gsem (v25_2 <- c.marketfreedom fincQ2dum fincQ3dum fincQ4dum  univd age2 gender  C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial)) 

*Model 6 (with other country-level controls) 

gsem (v25_2 <- c.marketfreedom gini govexp ud fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial))

**Table S11
* Six disaggregated forced-choice outcome groups (governemnt base outcome) using GDP per capita as the main contextual explanatory variable

*Model 7 (without other country-level controls) 

gsem (v25_2 <- c.zgdp fincQ2dum fincQ3dum fincQ4dum  univd age2 gender  C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial)) 

*Model 8 (with other country-level controls) 

gsem (v25_2 <- c.zgdp gini govexp ud fincQ2dum fincQ3dum fincQ4dum  univd age2 gender C[country] if country != 862 [pweight=WEIGHT], link(logit) family(multinomial))
