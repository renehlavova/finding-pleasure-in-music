### Libraries ----
library("foreign")
library("psych")
library("factoextra")
library("dplyr")
library("sjstats")
library("lavaan")

setwd("~/Dropbox/STUDY/MGR/THESIS")

### Importing data ----
music_ia = read.spss("music_cleaned_item_analysis.sav", to.data.frame=TRUE)
names(music_ia) <- tolower(names(music_ia)) # columns

### Item analysis ----
citation("psych")
citation("factoextra")
citation("foreign")
citation("factoextra")
citation("dplyr")
citation("sjstats")
head(music_ia)

dich = function(variable, categories) {
  midpoint = categories / 2
  even = as.logical(ifelse((midpoint-trunc(midpoint))==0, "TRUE", "FALSE"))
  midpoint = ifelse(even==TRUE, midpoint, midpoint+.5) 
  if(even==FALSE) variable[variable==midpoint] = NA
  variable = ifelse(variable<midpoint, 0, 1)
}

### Social support ----
supp = music_ia[,5:16]

str(supp) # structure of unscored items
lapply(supp, FUN=table, exclude=NULL) # fre tables

# item difficulty
means.supp = round(colMeans(supp, na.rm=TRUE), 2) # checking mean scores
means.supp

supp.dich = data.frame(dich(supp,7)) # convert polytomous to dichotomous
difficulty.supp = round(colMeans(supp.dich, na.rm=TRUE), 2) # check item difficulty
subset(difficulty.supp, (difficulty.supp < .2 | difficulty.supp > .8)) # show only those outside the interval

difficulty(supp.dich) # double check item difficulty from sjstats

# scoring
which(colnames(supp)==c("supp1", "supp2"))
supp.keys.list = list(supp_family = c(3, 4, 8, 11),
                      supp_friends = c(6, 7, 9, 12),
                      supp_sign_other = c(1, 2, 5, 10),
                      supp_total = c(1,2,3,4,5,6,7,8,9,10,11,12)) # items in the subscales
supp.keys = make.keys(supp, supp.keys.list, item.labels = colnames(supp)) # create the scoring key itself
supp.scored = scoreItems(supp.keys, supp, impute="none", min=1, max=7) # calculate the scores
head(supp.scored$scores)

music_ia$supp_family = supp.scored$scores[,"supp_family"] # saving the scores into the df
music_ia$supp_friends = supp.scored$scores[,"supp_friends"]
music_ia$supp_sign_other = supp.scored$scores[,"supp_sign_other"]
music_ia$supp_total = supp.scored$scores[,"supp_total"]

# item discrimination (item-total correlation later on reliability)
iic.supp = round(cor(supp, use="pairwise.complete.obs"), 2) # inter-item correlation
iic.supp

# reliability - cronbach alpha
output.alpha.family = psych::alpha(supp[,abs(supp.keys.list$supp_family)], check.keys=TRUE) # calculate alpha
output.alpha.family
output.alpha.friends = psych::alpha(supp[,abs(supp.keys.list$supp_friends)], check.keys=TRUE) # calculate alpha
output.alpha.friends
output.alpha.sign_other = psych::alpha(supp[,abs(supp.keys.list$supp_sign_other)], check.keys=TRUE) # calculate alpha
output.alpha.sign_other
output.alpha.supp_total = psych::alpha(supp[,abs(supp.keys.list$supp_total)], check.keys=TRUE) # calculate alpha
output.alpha.supp_total

scale.names = c("Support - family", "Support - friends", "Support - significant other", "Support - total score") # format the output into a readable form
supp.alphas = as.numeric(c(output.alpha.family$total[2],
                           output.alpha.friends$total[2],
                           output.alpha.sign_other$total[2],
                           output.alpha.supp_total$total[2]))
supp.alpha.table = data.frame(Scale = scale.names, Std.Alpha = supp.alphas)
supp.alpha.table

# reliability - omega
# >>>> doesn't assume unidimensionality, so we can check overall scale reliability
output.omega.overall = omega(supp)
output.omega.overall

# cfa hierarchical
supp.model = 'supp_family =~ supp3 + supp4 + supp8 + supp11
                supp_friends =~ supp6 + supp7 + supp9 + supp12
                supp_sign_other =~ supp1 + supp2 + supp5 + supp10
                supp_total =~ supp_family + supp_friends + supp_sign_other'
fit.supp.cfa = lavaan::cfa(supp.model, data = supp, estimator="MLR", missing="fiml")
lavaan::summary(fit.supp.cfa, fit.measures=T)

# cfa double check 1 factor only
supp.model1 = 'supp_total =~ supp1 + supp2 + supp3 + supp4 + supp5 + supp6 + supp7 + supp8 + supp9 + supp10 + supp11 + supp12'
fit.supp.cfa1 = lavaan::cfa(supp.model1, data = supp, estimator="MLR", missing="fiml")
lavaan::summary(fit.supp.cfa1, fit.measures=T)

# cfa double check 3 factors only
supp.model2 = 'supp_family =~ supp3 + supp4 + supp8 + supp11
                supp_friends =~ supp6 + supp7 + supp9 + supp12
                supp_sign_other =~ supp1 + supp2 + supp5 + supp10'
fit.supp.cfa2 = lavaan::cfa(supp.model2, data = supp, estimator="MLR", missing="fiml")
lavaan::summary(fit.supp.cfa2, fit.measures=T)

# country differences
library(forcats)
music_ia$countries = fct_collapse(music_ia$countries, Ireland_UK = c("Ireland", "UK"))

summary(aov(supp_family ~ countries, data=music_ia))
summary(aov(supp_friends ~ countries, data=music_ia))
summary(aov(supp_sign_other ~ countries, data=music_ia))
summary(aov(supp_total ~ countries, data=music_ia))

summary(aov(supp1 ~ countries, data=music_ia))
summary(aov(supp2 ~ countries, data=music_ia))
summary(aov(supp3 ~ countries, data=music_ia))
summary(aov(supp4 ~ countries, data=music_ia))
summary(aov(supp5 ~ countries, data=music_ia))
summary(aov(supp6 ~ countries, data=music_ia))
summary(aov(supp7 ~ countries, data=music_ia))
summary(aov(supp8 ~ countries, data=music_ia))
summary(aov(supp9 ~ countries, data=music_ia))
summary(aov(supp10 ~ countries, data=music_ia))
summary(aov(supp11 ~ countries, data=music_ia))
summary(aov(supp12 ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=supp_family, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp_friends, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp_sign_other, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp_total, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp1, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp2, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp3, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp4, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp5, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp6, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp7, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp8, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp9, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp10, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp11, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp12, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

##### AGAIN WITHOUT ITEM 10
# scoring
which(colnames(supp)==c("supp1", "supp2"))
supp.keys.list = list(supp_family = c(3, 4, 8, 11),
                      supp_friends = c(6, 7, 9, 12),
                      supp_sign_other = c(1, 2, 5),
                      supp_total = c(1,2,3,4,5,6,7,8,9,11,12)) # items in the subscales
supp.keys = make.keys(supp, supp.keys.list, item.labels = colnames(supp)) # create the scoring key itself
supp.scored = scoreItems(supp.keys, supp, impute="none", min=1, max=7) # calculate the scores
head(supp.scored$scores)

music_ia$supp_family = supp.scored$scores[,"supp_family"] # saving the scores into the df
music_ia$supp_friends = supp.scored$scores[,"supp_friends"]
music_ia$supp_sign_other = supp.scored$scores[,"supp_sign_other"]
music_ia$supp_total = supp.scored$scores[,"supp_total"]

# cronbach
output.alpha.sign_other = psych::alpha(supp[,abs(supp.keys.list$supp_sign_other)], check.keys=TRUE) # calculate alpha
output.alpha.sign_other
output.alpha.supp_total = psych::alpha(supp[,abs(supp.keys.list$supp_total)], check.keys=TRUE) # calculate alpha
output.alpha.supp_total

# omega
supp %>% select(-supp10) %>% omega(nfactors=3)

# cfa 
supp.model2 = 'supp_family =~ supp3 + supp4 + supp8 + supp11
                supp_friends =~ supp6 + supp7 + supp9 + supp12
                supp_sign_other =~ supp1 + supp2 + supp5
                supp_total =~ supp_family + supp_friends + supp_sign_other'
fit.supp.cfa2 = lavaan::cfa(supp.model2, data = supp, estimator="MLR", missing="fiml")
lavaan::summary(fit.supp.cfa2, fit.measures=T)

# country diff
summary(aov(supp_sign_other ~ countries, data=music_ia))
summary(aov(supp_total ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=supp_sign_other, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=supp_total, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

### BMRQ ----
bmrq = music_ia[,17:36]

str(bmrq) # structure of unscored items
lapply(bmrq, FUN=table, exclude=NULL) # fre tables

# item difficulty
means.bmrq = round(colMeans(bmrq, na.rm=TRUE), 2) # checking mean scores
means.bmrq

bmrq.dich = data.frame(dich(bmrq,5))
difficulty.bmrq = round(colMeans(bmrq.dich, na.rm=TRUE), 2)
subset(difficulty.bmrq, (difficulty.bmrq < .2 | difficulty.bmrq > .8))

difficulty(bmrq.dich)

# scoring
which(colnames(bmrq)==c("barc1_sharing"))
bmrq.keys.list = list(barcelona_ms = c(-2, 7, 11, 17),
                      barcelona_ee = c(3, 8, 12, 18),
                      barcelona_mr = c(4, 9, 14, 19),
                      barcelona_sm = c(-5, 10, 15, 20),
                      barcelona_sr = c(1, 6, 13, 16),
                      barcelona_total = c(1, -2, 3, 4, -5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_ms = bmrq.scored$scores[,"barcelona_ms"] # saving the scores into the df
music_ia$barcelona_ee = bmrq.scored$scores[,"barcelona_ee"]
music_ia$barcelona_mr = bmrq.scored$scores[,"barcelona_mr"]
music_ia$barcelona_sm = bmrq.scored$scores[,"barcelona_sm"]
music_ia$barcelona_sr = bmrq.scored$scores[,"barcelona_sr"]
music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# item discrimination (item-total correlation later on reliability)
iic.bmrq = round(cor(bmrq, use="pairwise.complete.obs"), 2) # inter-item correlation
iic.bmrq

# reliability - cronbach
output.alpha.barcelona_ms = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_ms)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_ms
output.alpha.barcelona_ee = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_ee)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_ee
output.alpha.barcelona_mr = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_mr)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_mr
output.alpha.barcelona_sm = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_sm)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_sm
output.alpha.barcelona_sr = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_sr)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_sr
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

scale.names = c("Musical Seeking", "Emotion Evocation", "Mood Regulation", "Sensory Motor", "Social Reward", "Total") # format the output into a readable form
barcelona.alphas = as.numeric(c(output.alpha.barcelona_ms$total[2],
                           output.alpha.barcelona_ee$total[2],
                           output.alpha.barcelona_mr$total[2],
                           output.alpha.barcelona_sm$total[2],
                           output.alpha.barcelona_sr$total[2],
                           output.alpha.barcelona_total$total[2]))
barcelona.alpha.table = data.frame(Scale = scale.names, Std.Alpha = barcelona.alphas)
barcelona.alpha.table

# reliability - omega
# >>>> doesn't assume unidimensionality, so we can check overall scale reliability
output.omega.overall.bmrq = omega(bmrq, nfactors = 5)
output.omega.overall.bmrq

# cfa 5 factors
bmrq.model = 'barcelona_ms =~ barc2_hardly_listen + barc7_inform_self + barc11_newmusic + barc17_spending
              barcelona_ee =~ barc3_contain_emot + barc8_get_emotional + barc12_tearful + barc18_chills
              barcelona_mr =~ barc4_gives_company + barc9_calms + barc14_chill_out + barc19_comforts
              barcelona_sm =~ barc5_no_dance + barc10_dance + barc15_humming + barc20_tapping
              barcelona_sr =~ barc1_sharing + barc6_bonding + barc13_sing_with_others + barc16_concert_connect'

fit.bmrq.cfa = cfa(bmrq.model, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa, fit.measures=T)

modindices(fit.bmrq.cfa)

# cfa 1 factor 
bmrq.model2 = 'barcelona =~ barc2_hardly_listen + barc7_inform_self + barc11_newmusic + barc17_spending + barc3_contain_emot + barc8_get_emotional + barc12_tearful + barc18_chills + barc4_gives_company + barc9_calms + barc14_chill_out + barc19_comforts + barc5_no_dance + barc10_dance + barc15_humming + barc20_tapping + barc1_sharing + barc6_bonding + barc13_sing_with_others + barc16_concert_connect'

fit.bmrq.cfa2 = cfa(bmrq.model2, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa2, fit.measures=T)

modindices(fit.bmrq.cfa2)

# country differences
# library(forcats)
# music_ia$countries = fct_collapse(music_ia$countries, Ireland_UK = c("Ireland", "UK"))

summary(aov(barcelona_ee ~ countries, data=music_ia))
summary(aov(barcelona_mr ~ countries, data=music_ia))
summary(aov(barcelona_sr ~ countries, data=music_ia))
summary(aov(barcelona_sm ~ countries, data=music_ia))
summary(aov(barcelona_ms ~ countries, data=music_ia))
summary(aov(barcelona_total ~ countries, data=music_ia))

summary(aov(barc1_sharing ~ countries, data=music_ia))
summary(aov(barc2_hardly_listen ~ countries, data=music_ia))
summary(aov(barc3_contain_emot ~ countries, data=music_ia))
summary(aov(barc4_gives_company ~ countries, data=music_ia))
summary(aov(barc5_no_dance ~ countries, data=music_ia))
summary(aov(barc6_bonding ~ countries, data=music_ia))
summary(aov(barc7_inform_self ~ countries, data=music_ia))
summary(aov(barc8_get_emotional ~ countries, data=music_ia))
summary(aov(barc9_calms ~ countries, data=music_ia))
summary(aov(barc10_dance ~ countries, data=music_ia))
summary(aov(barc11_newmusic ~ countries, data=music_ia))
summary(aov(barc12_tearful ~ countries, data=music_ia))
summary(aov(barc13_sing_with_others ~ countries, data=music_ia))
summary(aov(barc14_chill_out ~ countries, data=music_ia))
summary(aov(barc15_humming ~ countries, data=music_ia))
summary(aov(barc16_concert_connect ~ countries, data=music_ia))
summary(aov(barc17_spending ~ countries, data=music_ia))
summary(aov(barc18_chills ~ countries, data=music_ia))
summary(aov(barc19_comforts ~ countries, data=music_ia))
summary(aov(barc20_tapping ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=barcelona_ee, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barcelona_mr, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barcelona_sr, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barcelona_sm, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barcelona_ms, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barcelona_total, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc1_sharing, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc2_hardly_listen, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc3_contain_emot, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc4_gives_company, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc5_no_dance, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc6_bonding, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc7_inform_self, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc8_get_emotional, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc9_calms, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc10_dance, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc11_newmusic, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc12_tearful, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc13_sing_with_others, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc14_chill_out, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc15_humming, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc16_concert_connect, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc17_spending, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc18_chills, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc19_comforts, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=barc20_tapping, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

##### AGAIN WITHOUT ITEM 20
# scoring
bmrq.keys.list = list(barcelona_total = c(1, -2, 3, 4, -5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping) %>% omega(nfactors=5)

# cfa 
bmrq.model2 = 'bmrq.uni =~ barc1_sharing + barc2_hardly_listen + barc3_contain_emot + barc4_gives_company + barc5_no_dance + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc9_calms + barc10_dance + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc15_humming + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa2 = cfa(bmrq.model2, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa2, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5
# scoring
bmrq.keys.list = list(barcelona_total = c(1, -2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance) %>% omega(nfactors=5)

# cfa 
bmrq.model3 = 'bmrq.uni =~ barc1_sharing + barc2_hardly_listen + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc9_calms + barc10_dance + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc15_humming + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa3 = cfa(bmrq.model3, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa3, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance, -barc2_hardly_listen) %>% omega(nfactors=5)

# cfa 
bmrq.model4 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc9_calms + barc10_dance + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc15_humming + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa4 = cfa(bmrq.model4, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa4, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance) %>% omega(nfactors=5)

# cfa 
bmrq.model5 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc9_calms + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc15_humming + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa5 = cfa(bmrq.model5, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa5, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10 + 15
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance, -barc15_humming) %>% omega(nfactors=5)

# cfa 
bmrq.model6 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc9_calms + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa6 = cfa(bmrq.model6, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa6, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10 + 15 + 9
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 11, 12, 13, 14, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance, -barc15_humming, -barc9_calms) %>% omega(nfactors=5)

# cfa 
bmrq.model7 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc14_chill_out + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa7 = cfa(bmrq.model7, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa7, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10 + 15 + 9 + 14
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 11, 12, 13, 16, 17, 18, 19)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance, -barc15_humming, -barc9_calms, -barc14_chill_out) %>% omega(nfactors=5)

# cfa 
bmrq.model8 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc16_concert_connect + barc17_spending + barc18_chills + barc19_comforts'
fit.bmrq.cfa8 = cfa(bmrq.model8, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa8, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10 + 15 + 9 + 14 + 19
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 4, 6, 7, 8, 11, 12, 13, 16, 17, 18)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc19_comforts, -barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance, -barc15_humming, -barc9_calms, -barc14_chill_out) %>% omega(nfactors=5)

# cfa 
bmrq.model9 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc4_gives_company + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc16_concert_connect + barc17_spending + barc18_chills'
fit.bmrq.cfa9 = cfa(bmrq.model9, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa9, fit.measures=T)

##### AGAIN WITHOUT ITEMS 20 + 5 + 2 + 10 + 15 + 9 + 14 + 19 + 4
# scoring
bmrq.keys.list = list(barcelona_total = c(1, 3, 6, 7, 8, 11, 12, 13, 16, 17, 18)) # items in the subscales
bmrq.keys = make.keys(bmrq, bmrq.keys.list, item.labels = colnames(bmrq)) # create the scoring key itself
bmrq.scored = scoreItems(bmrq.keys, bmrq, impute="none", min=1, max=5) # calculate the scores
head(bmrq.scored$scores)

music_ia$barcelona_total = bmrq.scored$scores[,"barcelona_total"]

# cronbach
output.alpha.barcelona_total = psych::alpha(bmrq[,abs(bmrq.keys.list$barcelona_total)], check.keys=TRUE) # calculate alpha
output.alpha.barcelona_total

# omega
bmrq %>% select(-barc4_gives_company, -barc19_comforts, -barc20_tapping, -barc5_no_dance, -barc2_hardly_listen, -barc10_dance, -barc15_humming, -barc9_calms, -barc14_chill_out) %>% omega(nfactors=5)

# cfa 
bmrq.model10 = 'bmrq.uni =~ barc1_sharing + barc3_contain_emot + barc6_bonding + barc7_inform_self + barc8_get_emotional + barc11_newmusic + barc12_tearful + barc13_sing_with_others + barc16_concert_connect + barc17_spending + barc18_chills'
fit.bmrq.cfa10 = cfa(bmrq.model10, data = bmrq, estimator="MLR", missing="fiml")
summary(fit.bmrq.cfa10, fit.measures=T)

### PANAS ----
panas = music_ia[,37:56]

str(panas) # structure of unscored items
lapply(panas, FUN=table, exclude=NULL) # fre tables

# item difficulty
means.panas = round(colMeans(panas, na.rm=TRUE), 2) # checking mean scores
means.panas

panas.dich = data.frame(dich(panas,5))
difficulty.panas = round(colMeans(panas.dich, na.rm=TRUE), 2)
subset(difficulty.panas, (difficulty.panas < .2 | difficulty.panas > .8))

difficulty(panas.dich)

# scoring
which(colnames(panas)==c("panas1_interested"))
panas.keys.list = list(panas_p = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19),
                      panas_n = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) # items in the subscales
panas.keys = make.keys(panas, panas.keys.list, item.labels = colnames(panas)) # create the scoring key itself
panas.scored = scoreItems(panas.keys, panas, impute="none", min=1, max=5, totals=TRUE) # calculate the scores
head(panas.scored$scores)

music_ia$panas_p = panas.scored$scores[,"panas_p"] # saving the scores into the df
music_ia$panas_n = panas.scored$scores[,"panas_n"]

# item discrimination (item-total correlation later on reliability)
iic.panas = round(cor(panas, use="pairwise.complete.obs"), 2) # inter-item correlation
iic.panas

# reliability - cronbach
output.alpha.panas_p = psych::alpha(panas[,abs(panas.keys.list$panas_p)], check.keys=TRUE) # calculate alpha
output.alpha.panas_p
output.alpha.panas_n = psych::alpha(panas[,abs(panas.keys.list$panas_n)], check.keys=TRUE) # calculate alpha
output.alpha.panas_n

scale.names = c("Positive Affect", "Negative Affect") # format the output into a readable form
panas.alphas = as.numeric(c(output.alpha.panas_p$total[2],
                            output.alpha.panas_n$total[2]))
panas.alpha.table = data.frame(Scale = scale.names, Std.Alpha = panas.alphas)
panas.alpha.table

# reliability - omega
output.omega.panas = omega(panas, nfactors=2)
output.omega.panas

# cfa
panas.model = 'panas_p =~ panas1_interested + panas3_excited + panas5_strong + panas9_enthusiastic + panas10_proud + panas12_alert + panas14_inspired + panas16_determined + panas17_attentive + panas19_active
               panas_n =~ panas2_distressed + panas4_upset + panas6_guilty + panas7_scared + panas8_hostile + panas11_irritable + panas13_ashamed + panas15_nervous + panas18_jittery + panas20_afraid'
fit.panas.cfa = lavaan::cfa(panas.model, data = panas, estimator="MLR", missing="fiml")
lavaan::summary(fit.panas.cfa, fit.measures=T)

# country differences
# library(forcats)
# music_ia$countries = fct_collapse(music_ia$countries, Ireland_UK = c("Ireland", "UK"))

summary(aov(panas_p ~ countries, data=music_ia))
summary(aov(panas_n ~ countries, data=music_ia))

summary(aov(panas1_interested ~ countries, data=music_ia))
summary(aov(panas2_distressed ~ countries, data=music_ia))
summary(aov(panas3_excited ~ countries, data=music_ia))
summary(aov(panas4_upset ~ countries, data=music_ia))
summary(aov(panas5_strong ~ countries, data=music_ia))
summary(aov(panas6_guilty ~ countries, data=music_ia))
summary(aov(panas7_scared ~ countries, data=music_ia))
summary(aov(panas8_hostile ~ countries, data=music_ia))
summary(aov(panas9_enthusiastic ~ countries, data=music_ia))
summary(aov(panas10_proud ~ countries, data=music_ia))
summary(aov(panas11_irritable ~ countries, data=music_ia))
summary(aov(panas12_alert ~ countries, data=music_ia))
summary(aov(panas13_ashamed ~ countries, data=music_ia))
summary(aov(panas14_inspired ~ countries, data=music_ia))
summary(aov(panas15_nervous ~ countries, data=music_ia))
summary(aov(panas16_determined ~ countries, data=music_ia))
summary(aov(panas17_attentive ~ countries, data=music_ia))
summary(aov(panas18_jittery ~ countries, data=music_ia))
summary(aov(panas19_active ~ countries, data=music_ia))
summary(aov(panas20_afraid ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=panas_p, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas_n, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas1_interested, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas2_distressed, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas3_excited, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas4_upset, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas5_strong, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas6_guilty, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas7_scared, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas8_hostile, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas9_enthusiastic, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas10_proud, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas11_irritable, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas12_alert, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas13_ashamed, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas14_inspired, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas15_nervous, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas16_determined, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas17_attentive, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas18_jittery, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas19_active, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(music_ia, aes(x=countries, y=panas20_afraid, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

##### AGAIN WITHOUT ITEM EXCITED 
# scoring
which(colnames(panas)==c("panas1_interested"))
panas.keys.list = list(panas_p = c(1, 5, 9, 10, 12, 14, 16, 17, 19),
                       panas_n = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) # items in the subscales
panas.keys = make.keys(panas, panas.keys.list, item.labels = colnames(panas)) # create the scoring key itself
panas.scored = scoreItems(panas.keys, panas, impute="none", min=1, max=5, totals=TRUE) # calculate the scores
head(panas.scored$scores)

music_ia$panas_p = panas.scored$scores[,"panas_p"] # saving the scores into the df
music_ia$panas_n = panas.scored$scores[,"panas_n"]

# reliability - cronbach
output.alpha.panas_p = psych::alpha(panas[,abs(panas.keys.list$panas_p)], check.keys=TRUE) # calculate alpha
output.alpha.panas_p

# reliability - omega
panas %>% select(-panas3_excited) %>% omega(nfactors=2)

# cfa bez položky excited
panas.model2 = 'panas_p =~ panas1_interested + panas5_strong + panas9_enthusiastic + panas10_proud + panas12_alert + panas14_inspired + panas16_determined + panas17_attentive + panas19_active
               panas_n =~ panas2_distressed + panas4_upset + panas6_guilty + panas7_scared + panas8_hostile + panas11_irritable + panas13_ashamed + panas15_nervous + panas18_jittery + panas20_afraid'
fit.panas.cfa2 = lavaan::cfa(panas.model2, data = panas, estimator="MLR", missing="fiml")
lavaan::summary(fit.panas.cfa2, fit.measures=T)

modindices(fit.panas.cfa2)

# country differences
# library(forcats)
# music_ia$countries = fct_collapse(music_ia$countries, Ireland_UK = c("Ireland", "UK"))

summary(aov(panas_p ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=panas_p, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

##### AGAIN WITHOUT ITEM GUILTY (6) 
# scoring
which(colnames(panas)==c("panas1_interested"))
panas.keys.list = list(panas_p = c(1, 5, 9, 10, 12, 14, 16, 17, 19),
                       panas_n = c(2, 4, 7, 8, 11, 13, 15, 18, 20)) # items in the subscales
panas.keys = make.keys(panas, panas.keys.list, item.labels = colnames(panas)) # create the scoring key itself
panas.scored = scoreItems(panas.keys, panas, impute="none", min=1, max=5, totals=TRUE) # calculate the scores
head(panas.scored$scores)

music_ia$panas_p = panas.scored$scores[,"panas_p"] # saving the scores into the df
music_ia$panas_n = panas.scored$scores[,"panas_n"]

# reliability - cronbach
output.alpha.panas_n = psych::alpha(panas[,abs(panas.keys.list$panas_n)], check.keys=TRUE) # calculate alpha
output.alpha.panas_n

# reliability - omega
panas %>% select(-panas3_excited, -panas6_guilty) %>% omega(nfactors=2)

# cfa bez položky excited + guilty
panas.model3 = 'panas_p =~ panas1_interested + panas5_strong + panas9_enthusiastic + panas10_proud + panas12_alert + panas14_inspired + panas16_determined + panas17_attentive + panas19_active
               panas_n =~ panas2_distressed + panas4_upset + panas7_scared + panas8_hostile + panas11_irritable + panas13_ashamed + panas15_nervous + panas18_jittery + panas20_afraid'
fit.panas.cfa3 = lavaan::cfa(panas.model3, data = panas, estimator="MLR", missing="fiml")
lavaan::summary(fit.panas.cfa3, fit.measures=T)

modindices(fit.panas.cfa3)

# country differences
# library(forcats)
# music_ia$countries = fct_collapse(music_ia$countries, Ireland_UK = c("Ireland", "UK"))

summary(aov(panas_n ~ countries, data=music_ia))

ggplot(music_ia, aes(x=countries, y=panas_n, fill=countries)) + 
  geom_boxplot() + 
  theme_classic()

