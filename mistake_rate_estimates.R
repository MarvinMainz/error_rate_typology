
## set working directory (optional, only in Rstudio)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import packages
if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse", quiet = TRUE)
library(tidyverse, quietly = TRUE)
if (!require("stringr", quietly = TRUE)) install.packages("stringr", quiet = TRUE)
library(stringr, quietly = TRUE)


########################################################
############### 1. WALS (versioning) ###################
########################################################

# import data
wals_values_2008 <- read_csv("../data/wals_2008_values.csv")
wals_languages_2008 <- read_csv("../data/wals_2008_languages.csv")

wals_values_2020.3 <- read_csv("../data/wals_2020v3_values.csv")
wals_languages_2020.3 <- read_csv("../data/wals_2020v3_languages_corr.csv")


# data transformation

## change old ID format (2008) into new format for comparability
wals_values_2008_A <-
  wals_values_2008 %>%
  mutate(Parameter_ID = str_c(Parameter_ID, "A"))

## combining 2008 and 2020v3 (using 'full_join', keeping all rows with unmatched IDs!)
wals_values_2008_A <- wals_values_2008_A %>% 
  select(Language_ID, Parameter_ID, Source, Value)  %>%
  rename(Value_2008 = Value, Source_2008 = Source)

wals_values_2020.3 <- wals_values_2020.3 %>% 
  select(Language_ID, Parameter_ID, Source, Value) %>%
  rename(Value_2020.3 = Value, Source_2020.3 = Source)

values_both <- full_join(
  wals_values_2008_A, wals_values_2020.3) %>%
  mutate(
    Value_2020.3 = replace_na(Value_2020.3, -1) # replacing NA with negative numbers for better computability
    , Value_2008 = replace_na(Value_2008, -2)
    , value_discrepancy = (Value_2008 != Value_2020.3))


# data analysis

### first survey (including all NAs)
#sum(values_both$Value_2008 != values_both$Value_2020.3)
#  # 19842
#round(sum(values_both$Value_2008 != values_both$Value_2020.3) / nrow(values_both) * 100, 2)
#  # 25.77%
### additionally: checking all NAs
#nrow(filter(values_both, Value_2020.3 == -1))
#  # 522 NAs in 2020v3 ('removed datapoints')
#nrow(filter(values_both, Value_2008 == -2))
#  # 18932 NAs in 2008 ('added datapoints')
#nrow(values_both) - nrow(filter(values_both, Value_2020.3 == -1)) - nrow(filter(values_both, Value_2008 == -2))
#  # 57543 (non-NA values)


# preparing for distinguishing annotator and total mistakes
# only looking at publications, not pages (assuming that overlooking info in same publication is annotator mistake)
values_both$Source_2008 <- str_replace_all(values_both$Source_2008, "\\[.*?\\]", "")
values_both$Source_2020.3 <- str_replace_all(values_both$Source_2020.3, "\\[.*?\\]", "")  


## version 1: omitting all NAs

values_both1 <- values_both %>% 
  filter(Value_2008 != -2 & Value_2020.3 != -1)

## total mistakes 
wals_mistakes_total1 <- nrow(filter(values_both1, value_discrepancy == T))
round(wals_mistakes_total1 / nrow(values_both1) * 100, 2)
  # 0.67%

## annotator mistakes
wals_mistakes_annotator1 <- values_both1 %>% 
  filter(value_discrepancy == T) %>% 
  mutate(sources_diff = (Source_2008 != Source_2020.3))
wals_mistakes_annotator1 <- nrow(filter(annotator_mistakes1, sources_diff == F))
round(wals_mistakes_annotator1 / nrow(values_both1) * 100, 2)
  # 0.50%


## version 2: omitting only 2008's NAs 
  # (considering *removed* but not *added* data points for mistakes) -> mostly changes/mistakes of language_ID

values_both2 <- filter(values_both, Value_2008 != -2)

## total mistakes 
wals_mistakes_total2 <- nrow(filter(values_both2, value_discrepancy == T))
round(wals_mistakes_total2 / nrow(values_both2) * 100, 2)
# 1.57%

## annotator mistakes
wals_mistakes_annotator2 <- values_both2 %>% 
  filter(value_discrepancy == T) %>% 
  mutate(sources_diff = (Source_2008 != Source_2020.3))
wals_mistakes_annotator2 <- nrow(filter(annotator_mistakes2, sources_diff == F))
round(wals_mistakes_annotator2 / nrow(values_both2) * 100, 2)
# 0.49%


########################################################
################ 2. Grambank: Siwi #####################
########################################################

# import data
Siwi_orig <- readr::read_tsv("data/NB_siwi1239.tsv") # original Grambank 1.0 version
Siwi_corr <- readr::read_tsv("data/NB_siwi1239_corrections.tsv") # version with all *accepted* corrections
GB <- readr::read_csv("../data/gb_values_v1.03.csv")

# merge original and corrected
Siwi_comp <- dplyr::left_join(Siwi_corr, Siwi_orig, by = "Feature_ID")
Siwi_comp <- Siwi_comp %>% 
  dplyr::transmute(
    Parameter_ID = Feature_ID,
    value_orig = Value.y,
    value_corr = Value.x,
    Source_orig = Source.y,
    Source_corr = Source.x 
  )

## note: there are 220 instead of 195 rows
# length(unique(Siwi_comp$Parameter_ID))
## 25 features (with unique IDs!) which are no longer used in Grambank are still present
# comparing only retained (canonical) features
Siwi_comp_195 <- filter(Siwi_comp, Parameter_ID %in% GB$Parameter_ID)

# total mistake rate (incl. error-by-omission)
Siwi_comp_195_total <- Siwi_comp_195
Siwi_comp_195_total[2][is.na(Siwi_comp_195_total[2])] <- "?"
GB_Siwi_total <- sum(Siwi_comp_195_total[2] != Siwi_comp_195_total[3])/nrow(Siwi_comp_195_total) * 100
round(GB_Siwi_total, 2)
  # 25.64 %

# total mistake rate (no error-by-omission)
Siwi_comp_195_total2 <- filter(Siwi_comp_195, !is.na(value_orig))
GB_Siwi_total2 <- sum(Siwi_comp_195_total2[2] != Siwi_comp_195_total2[3])/nrow(Siwi_comp_195_total2) * 100
round(GB_Siwi_total2, 2)
  # 16.67%

# annotator mistake rate
Siwi_comp_195_annotator <- Siwi_comp_195

Siwi_comp_195_annotator$Source_orig <- str_remove_all(Siwi_comp_195_annotator$Source_orig, ":\\d+[^)]*\\)|[():]")
Siwi_comp_195_annotator$Source_corr <- str_remove_all(Siwi_comp_195_annotator$Source_corr, ":\\d+[^)]*\\)|[():]")
Siwi_comp_195_annotator$Source_corr <- str_remove_all(Siwi_comp_195_annotator$Source_corr, "Lameen Souag p.c. 2023")
Siwi_comp_195_annotator$Source_orig[is.na(Siwi_comp_195_annotator$Source_orig)] <- "null"
Siwi_comp_195_annotator$Source_corr <- str_replace(str_squish(Siwi_comp_195_annotator$Source_corr), ";$", "")

Siwi_comp_195_annotator <- Siwi_comp_195_annotator %>% 
  mutate(same_source = (Siwi_comp_195_annotator[4] == Siwi_comp_195_annotator[5]))
Siwi_comp_saso <- filter(Siwi_comp_195_annotator, same_source == T)

Siwi_comp_saso <- sum(Siwi_comp_saso[2] != Siwi_comp_saso[3])/nrow(Siwi_comp_saso) * 100
round(Siwi_comp_saso, 2)
  # 2.74%


########################################################
################ 3. Grambank: Hebrew ###################
########################################################

# https://github.com/grambank/grambank/issues/34
# 7 mistakes were criticized, 7 were accepted
round(7/195 * 100, 2)
 # 3.59%


########################################################
################ 4. Grambank: Karok ####################
########################################################

# https://github.com/grambank/grambank/issues/43
# 9 comments relating to feature values, one of them relating to 3 features at once, which were accepted as mistakes
round(11/195 * 100, 2)
 # 5.64%


########################################################
######### 5. Jazyki Mira (Nikuliceva) ##################
########################################################

## from Polyakov et al. 2009 we get the estimates
## Danish: 2.2%, Norwegian: 0.9% and Swedish: 1.3%
## all we want to know is the number of data points per language
#
#jazyki_mira <- read_csv("jazyki_mira_values.csv")
#jazyki_mira_langs <- read_csv("jazyki_mira_languages.csv")
#
#by <- join_by("Language_ID" == "ID")
#jazyki_mira <- left_join(jazyki_mira, jazyki_mira_langs, by = by)
#
### 1. Danish
#nrow(filter(jazyki_mira, Glottocode == "dani1285; juti1236"))
# # 110 (110 * 0.022 ~ 2.4 ??)
#
### 2. Norwegian
#nrow(filter(jazyki_mira, Glottocode == "norw1258"))
# # 111 (111 * 0.009 = 1)
#
### 3. Swedish
#nrow(filter(jazyki_mira, Glottocode == "swed1254"))
# # 121 (121 * 0.013 = 1.573 ??)
#
## in conclusion: clearly a mismatch; unclear what the basis for the estimates in Polyakov et al. (2009) actually is.
## Cf. email from D. Nikulicheva (addenda)


########################################################
################# 6. WALS: German ######################
########################################################

by <- join_by("Language_ID" == "ID")
wals_2008 <- left_join(wals_values_2008, wals_languages_2008, by = by)

# number of data points
nrow(filter(wals_2008, Name == "German"))
  # 129 

# cf. Plank (2009) or the explanatory file about mistake rates.
erroneous <- 10
missing_value <- 9
# doubtful <- 25

# total mistake rate (no error-by-omission)
round(erroneous / 129 * 100, 2)
  # 7.75%

# total mistake rate (incl. errors-by-omission; over 'applicable' WALS features cf. Plank 2009: 48)
round((erroneous + missing_value) / 140 * 100, 2)
  # 13.57%

## total mistake rate (incl. doubtful cases)
#  round((erroneous + doubtful) / 129 * 100, 2)
#  # 27.13%


########################################################
################# 7. WALS: Latvian #####################
########################################################

## Version 1 (following Hammarström 2016)

# double-check n of data points
nrow(filter(wals_2008, Name == "Latvian"))
 # 112 
  
# total mistake rate
round(10/112 * 100, 2)
 # 8.93%

# annotator mistake rate
round(7/112 * 100, 2)
 # 6.25%
 
## Version 2 (following Cysouw 2011)
 
 # total mistake rate
round(7/119 * 100, 2) 
 # 5.88%

# annotator mistake rate
round(2/119 * 100, 2)
 # 1.68%


########################################################
############## 8. WALS: Tukang Besi ####################
########################################################

# Hammarström (2016) gives 20/142 (total mistake rate)
round(20/142 * 100, 2)
  # 14.08%

# however in the first digitized version there are only 120 data points on Tukang Besi
nrow(filter(wals_2008, Name == "Tukang Besi"))
  # 120

# total mistake rate (no error-by-omission)
round(20/120 * 100, 2)
 # 16.67%

# if we accept this, we should exclude 2 WALS features which are only relevant to sign languages
# total mistake rate (incl. error-by-omission) version 2
round(20/140 * 100, 2)
  # 14.29%


########################################################
############# 9. WALS: 81A (Word order) ################
########################################################

# 16.3% disagreement on 1228 values.
# under the assumption that in disagreement one of the two coders will be correct:

# total mistake rate
16.3 / 2
  # 8.15%

# annotator mistake rate
# ~ 1/3 of this is about disagreement about the same source
# again under the assumption that one will be correct
round((16.3/3)/2)
  # 3.00%


########################################################
############# 10. GB: Inter-rater study ################
########################################################

# total mistake rate (including over '?')
round(((1996+(4323-3753))/7876)/2 *100, 2)
  # 16.29%

# total mistake rate without disagreement over '?'
disagreement_noNA <- (1 - (3753/4323)) * 100
round(disagreement_noNA / 2, 2)
  # 6.59 %

# annotator mistake rate
disagreement_noNA_Same <- (1 - 0.9) * 100
round(disagreement_noNA_Same / 2, 2) 
  # 5.00%


########################################################
########## 11. GB: unlanned double-coding ##############
########################################################

# for the unplanned double-coding, we took the disagreement values mentioned in Skirgaard et al. 2023
# and gave them equal weight by adding them up and dividing by their number
# (assuming that the pairwise comparisons contain the same number of data points)

# total mistake rates

# among research assistants
average_mistake_RAs <- round(c((((22 + 21 + 13 + 9 + 9) / 2) / 5), (((22 + 21 + 13 + 9 + 9) / 2) / 5) + (6/195) * (22 + 21 + 13 + 9 + 9) / 2 / 5), 2) # adjusting for 3% non binary features
average_mistake_RAs[1] # because of our assumption that 1 annotator will be right, using only the first value
  # 7.40%

# research assistant vs. linguist ('language expert')
average_mistake_RAvL <- round(c((13 + 11 + 5 + 4) / 2 / 5, ((13 + 11 + 5 + 4) / 2 / 5) + (6/195) * ((13 + 11 + 5 + 4) / 2 / 5)), 2)
average_mistake_RAvL[1] # because of our assumption that 1 annotator will be right, using only the first value
  # 3.3

# BOTH (research assistant vs. linguist and research assistant vs research assistant)
average_mistake_both <- round(c((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9, ((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9) + ((22 + 21 + 13 + 9 + 9 + 13 + 11 + 5 + 4) / 2 / 9) * (6/195)), 2)
average_mistake_both[1] # because of our assumption that 1 annotator will be right, using only the first value
  # 5.94


########################################################
################### 12. Kinbank ########################
########################################################

# total mistake rate
# binarized structural pattern (dis)agreement, thus simply /2
round((1 - 0.8) / 2 * 100, 2)
  # 10.00 %

