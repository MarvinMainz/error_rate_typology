########################################################
############### 1. WALS (versioning) ###################
########################################################

# import packages
library(tidyverse)

# import data
wals_values_2008 <- read_csv("wals2008_values.csv")
wals_languages_2008 <- read_csv("wals2008_languages.csv")

wals_values_2020.3 <- read_csv("wals_2020v3_values.csv")
wals_languages_2020.3 <- read_csv("wals_2020v3_languages_corr.csv")


# data transformation

## change old ID format (2008) into new format for comparability
wals_values_2008 <-
  wals_values_2008 %>%
  mutate(Parameter_ID = str_c(Parameter_ID, "A"))

## combining 2008 and 2020v3 (using 'full_join', keeping all rows with unmatched IDs!)
values_both <- full_join(
  select(wals_values_2008, Language_ID, Parameter_ID, Value) %>%
    rename(Value_2008 = Value)
  , select(wals_values_2020.3, Language_ID, Parameter_ID, Value) %>%
    rename(Value_2020.3 = Value)) %>%
  mutate(
    Value_2020.3 = replace_na(Value_2020.3, -1) # replacing NA with negative numbers for better computability
    , Value_2008 = replace_na(Value_2008, -2))


# data analysis

## version 1: including all NAs

### 1. all discrepancies

sum(values_both$Value_2008 != values_both$Value_2020.3)
# 19842
round(sum(values_both$Value_2008 != values_both$Value_2020.3) / nrow(values_both) * 100, 2)
# 25.77%

### 2. discrepancies per lang

langs_with_errors1 <-
  filter(values_both, Value_2008 != Value_2020.3) %>%
  pull(Language_ID) %>%
  unique()

errors_per_lang1 <- filter(values_both, Language_ID %in% langs_with_errors1) %>%
  group_by(Language_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_lang1)

### 3. discrepancies per feature

feat_with_errors1 <-
  filter(values_both, Value_2008 != Value_2020.3) %>%
  pull(Parameter_ID) %>%
  unique()

errors_per_feat1 <- filter(values_both, Parameter_ID %in% feat_with_errors1) %>%
  group_by(Parameter_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_feat1)

### 4. additionally: checking all NAs
nrow(filter(values_both, Value_2020.3 == -1))
# 522 NAs in 2020v3 ('removed datapoints')
nrow(filter(values_both, Value_2008 == -2))
# 18932 NAs in 2008 ('added datapoints')
nrow(values_both) - nrow(filter(values_both, Value_2020.3 == -1)) - nrow(filter(values_both, Value_2008 == -2))
# 57543 (non-NA values)


## version 2: omitting all NAs

values_both2 <- inner_join(
  select(wals_values_2008, Language_ID, Parameter_ID, Value) %>%
    rename(Value_2008 = Value)
  , select(wals_values_2020.3, Language_ID, Parameter_ID, Value) %>%
    rename(Value_2020.3 = Value))

### 1. all discrepancies

sum(values_both2$Value_2008 != values_both2$Value_2020.3)
# 388
round(sum(values_both2$Value_2008 != values_both2$Value_2020.3) / nrow(values_both2) * 100, 2)
# 0.67%

### 2. discrepancies per lang
langs_with_errors2 <-
  filter(values_both2, Value_2008 != Value_2020.3) %>%
  pull(Language_ID) %>%
  unique()

errors_per_lang2 <- filter(values_both2, Language_ID %in% langs_with_errors2) %>%
  group_by(Language_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_lang2)


## 3. discrepancies per feature

feat_with_errors2 <-
  filter(values_both2, Value_2008 != Value_2020.3) %>%
  pull(Parameter_ID) %>%
  unique()

errors_per_feat2 <- filter(values_both2, Parameter_ID %in% feat_with_errors2) %>%
  group_by(Parameter_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_feat2)


## version 3: omitting only 2008's NAs (considering *removed* but not added datapoints)

values_both3 <- filter(values_both, Value_2008 != -2)

### 1. discrepancies ('errors')

nrow (values_both3)
# 58065
sum(values_both3$Value_2008 != values_both3$Value_2020.3)
# 910
round(sum(values_both3$Value_2008 != values_both3$Value_2020.3) / nrow (values_both3) * 100, 2)
# 1.57%

### 2. discrepancies per lang

langs_with_errors3 <-
  filter(values_both3, Value_2008 != Value_2020.3) %>%
  pull(Language_ID) %>%
  unique()

errors_per_lang3 <- filter(values_both3, Language_ID %in% langs_with_errors3) %>%
  group_by(Language_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_lang3)

### 3. discrepancies per feature

feat_with_errors3 <-
  filter(values_both3, Value_2008 != Value_2020.3) %>%
  pull(Parameter_ID) %>%
  unique()

errors_per_feat3 <- filter(values_both3, Parameter_ID %in% feat_with_errors3) %>%
  group_by(Parameter_ID) %>%
  summarise(
    n_errors = sum(Value_2008!=Value_2020.3)
    , n_total  = n()) %>%
  mutate(proportion = n_errors / n_total) %>% 
  as.data.frame() %>% 
  arrange(desc(proportion))

head(errors_per_feat3)


########################################################
################ 2. Grambank: Siwi #####################
########################################################

# load packages
library(tidyverse)

# import data
Siwi_orig <- readr::read_tsv("NB_siwi1239.tsv") # original Grambank 1.0 version
Siwi_corr <- readr::read_tsv("NB_siwi1239_corrections.tsv") # version with all *accepted* corrections

# merge original and corrected
Siwi_comp <- dplyr::left_join(Siwi_corr, Siwi_orig, by = "Feature_ID")
Siwi_comp <- Siwi_comp %>% 
  dplyr::transmute(
    Feature_ID,
    value_orig = Value.y,
    value_corr = Value.x
  )

# note: there are 220 instead of 195 rows
length(unique(Siwi_comp$Feature_ID))
# 25 features which are no longer used in Grambank are still present

# all 220 features
Siwi_comp_220 <- Siwi_comp

# only retained features
Siwi_comp_195 <- filter(Siwi_comp, Feature_ID %in% GB$Feature_ID)

# error rate
GB_Siwi_error <- irr::agree(Siwi_comp_195[2:3])
round(100- GB_Siwi_error$value, 2)
# 16.67% error rate
  # this is the best estimate for comparability with other GB error rates

# alternatively use original 220 variable set
GB_Siwi_error2 <- irr::agree(Siwi_comp_220[2:3])
round(100- GB_Siwi_error2$value, 2)
# 14.57% error rate

# taking NA vs non-NA as mistake by omission
Siwi_comp_220[2][is.na(Siwi_comp_220[2])] <- "?"
Siwi_comp_195[2][is.na(Siwi_comp_195[2])] <- "?"

GB_Siwi_error3 <- irr::agree(Siwi_comp_195[2:3])
round(100 - GB_Siwi_error3$value, 2)
# 25.64% error rate

GB_Siwi_error4 <- irr::agree(Siwi_comp_220[2:3])
round(100- GB_Siwi_error4$value, 2)
# 22.73% error rate


########################################################
################ 3. Grambank: Hebrew ###################
########################################################

# https://github.com/grambank/grambank/issues/34
# 7 mistakes were criticised, 7 were accepted
round(7/195 * 100, 2)
 # 3.59% error rate


########################################################
################ 4. Grambank: Karok ####################
########################################################

# https://github.com/grambank/grambank/issues/43
# 9 comments relating to feature values, one of them relating to 3 features at once, thus:

round(11/195 * 100, 2)
 # 5.64% error rate


########################################################
######### 5. Jazyki Mira (Nikuliceva) ##################
########################################################

# from Polyakov et al. 2009 we get the estimates
# Danish: 2.2%, Norwegian: 0.9% and Swedish: 1.3%
# all we want to know is the number of data points per language

jazyki_mira <- read_csv("jazyki_mira_values.csv")
jazyki_mira_langs <- read_csv("jazyki_mira_languages.csv")

by <- join_by("Language_ID" == "ID")
jazyki_mira <- left_join(jazyki_mira, jazyki_mira_langs, by = by)

## 1. Danish
nrow(filter(jazyki_mira, Glottocode == "dani1285; juti1236"))
 # 110 (110 * 0.022 ~ 2.4 ??)

## 2. Norwegian
nrow(filter(jazyki_mira, Glottocode == "norw1258"))
 # 111 (111 * 0.009 = 1)

## 3. Swedish
nrow(filter(jazyki_mira, Glottocode == "swed1254"))
 # 121 (121 * 0.013 = 1.573 ??)


########################################################
################ 6. WALS: German #######################
########################################################

erroneous <- 10
doubtful <- 25

by <- join_by("Language_ID" == "ID")
wals_2008 <- left_join(wals_values_2008, wals_languages_2008, by = by)

nrow(filter(wals_2008, Name == "German"))
# 129 

round(erroneous / 129 * 100, 2)
 # 7.75% error rate

 round((erroneous + doubtful) / 129 * 100, 2)
 # 27.13% error rate


########################################################
################# 7. WALS: Latvian #####################
########################################################

## Version 1 (following Hammarström 2016)

# double-check n of data points
nrow(filter(wals_2008, Name == "Latvian"))
 # 112 
  
# all inaccuracies
round(10/112 * 100, 2)
 # 8.93% error rate

# strict annotation error
 round(7/112 * 100, 2)
 # 6.25% error rate

## Version 2 (following Cysouw 2011)

 # Cysouw mentions 7 mistakes in 119 data points. 
 # It is unclear why there would be 119 instead of 112 data points.
 # Thus we ammend this number to be 112.
 
# all inaccuracies
round(7/112 * 100, 2) 
 # 6.25% error rate

# strict annotation error
round(2/112 * 100, 2)
 # 1.79% error rate


########################################################
############## 8. WALS: Tukang Besi ####################
########################################################

# originally there was nothing to calculate, Hammarström (2016) gives 20/142
round(20/142 * 100, 2)
  # 14.08% error rate

# however there are only 120 data points on Tukang Besi
nrow(filter(wals_2008, Name == "Tukang Besi"))
 # 120

# thus it would need to be
round(20/120 * 100, 2)
 # 16.67% error rate


########################################################
############# 9. WALS: 81A (Word order) ################
########################################################

# 16.3% disagreement on 1228 values.

# ~ 1/3 of this is about disagreement about the same source
round(16.3/3, 2)
 # 5.43%

# from disagreement to error rate


########################################################
############# 10. GB: Inter-rater study ################
########################################################

disagreement_total <- (7876-1557-3573)/7876 * 100
error_total <- round((disagreement_total / 2), 2)
# 17.43%

disagreement_noNA <- (1 - (3753/4323)) * 100
error_noNA <- round(c((disagreement_noNA / 2) , ((disagreement_noNA / 2) + (disagreement_noNA / 2) * (6/195))), 2) # adjusting for 3% non binary features
# 6.59 -- 6.80 %
round(mean(error_noNA), 2)
# 6.70 %

disagreement_noNA_Same <- (1 - 0.9) * 100
error_noNA_Same <- round(c((disagreement_noNA_Same / 2) , ((disagreement_noNA_Same / 2) + (disagreement_noNA_Same / 2) * (6/195))), 2) # adjusting for 3% non binary features
# 5.00 -- 5.15 %
round(mean(error_noNA_Same), 2)
# 5.08 %


########################################################
############# 11. GB Double-Coding #####################
########################################################

average_error_RAs <- round(c((((22 + 21 + 13 + 9 + 9) / 2) / 5), (((22 + 21 + 13 + 9 + 9) / 2) / 5) + (6/195) * (22 + 21 + 13 + 9 + 9) / 2 / 5), 2) # adjusting for 3% non binary features
# 7.40 -- 7.63 %
round(mean(average_error_RAs), 2)
# 7.52

average_error_RAvL <- round(c((13 + 11 + 5 + 4) / 2 / 5, ((13 + 11 + 5 + 4) / 2 / 5) + (6/195) * ((13 + 11 + 5 + 4) / 2 / 5)), 2)
# 3.30 -- 3.40 
round(mean(average_error_RAvL), 2)
# 3.35 %

average_error_both <- round(c((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9, ((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9) + ((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9) * (6/195)), 2)
# 5.94 -- 6.13 %
round(mean(average_error_both), 2)
# 6.04 %


########################################################
################### 12. Kinbank ########################
########################################################

structural_error <- (1 - 0.8) / 2 * 100
# 10.00 %