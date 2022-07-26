library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(lvmisc)
locale("he")

######### The association between ambient air pollution and child development #########

######## outcome variables ########
######## import tipat halav data sets # using readr package 
dev_2015 <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/dev_raw/dev_2015.csv")
dev_2016 <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/dev_raw/dev_2016.csv")
dev_2017 <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/dev_raw/dev_2017.csv")
dev_2018 <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/dev_raw/dev_2018.csv")
dev_2019 <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/dev_raw/dev_2019.csv")

####### combine data frames #######
dev <- rbind(dev_2015, dev_2016, dev_2017, dev_2018, dev_2019)

##### find cohort's size
cohort_size <- nrow(dev %>% distinct(PersonId))

###### columns selection ######
###### remove tests were made after 24 months
#dev <- dev[,0:35]

###### remove tests which were not conducted # using dplyr package
dev <- dev %>% select(-`6m_headup_while_lying_on_back`, -responds_to_loud_sounds)

###### add a letter as first item of columns' names ######
colnames(dev)[5:50] <- paste("T", colnames(dev)[5:50], sep = "")

##### separate data for different models by tests' relevant age # using dplyr package
tests_2m <- select(dev, c(1:4, 5:8))
tests_4m <- select(dev, c(1:4, 9:15))
tests_6m <- select(dev, c(1:4, 16:20))
tests_9m <- select(dev, c(1:4, 21:29))
tests_12m <- select(dev, c(1:4, 30:35))
tests_18m <- select(dev, c(1:4, 36:42))
tests_24m <- select(dev, c(1:4, 43:50))

### create a function for removing incomplete cases (=irrelevant visit) and keeping kids' first meeting only
remove_cases <- function(df) {
  df <- df[complete.cases(df), ]
  df <- df %>%
    distinct(PersonId, .keep_all = TRUE)
  return(df)
}

### apply function on models
tests_2m <- remove_cases(tests_2m)
tests_4m <- remove_cases(tests_4m)
tests_6m <- remove_cases(tests_6m)
tests_9m <- remove_cases(tests_9m)
tests_12m <- remove_cases(tests_12m)
tests_18m <- remove_cases(tests_18m)
tests_24m <- remove_cases(tests_24m)

### build show up variable
tests_2m <- tests_2m %>% mutate(show_up_2m = ifelse(ExaminationAge >=1 & ExaminationAge <=3, 'ontime', 'late'))
tests_4m <- tests_4m %>% mutate(show_up_4m = ifelse(ExaminationAge >=3 & ExaminationAge <=5, 'ontime', 'late'))
tests_6m <- tests_6m %>% mutate(show_up_6m = ifelse(ExaminationAge >=5 & ExaminationAge <=7, 'ontime', 'late'))
tests_9m <- tests_9m %>% mutate(show_up_9m = ifelse(ExaminationAge >=8 & ExaminationAge <=10, 'ontime', 'late'))
tests_12m <- tests_12m %>% mutate(show_up_12m = ifelse(ExaminationAge >=10 & ExaminationAge <=14, 'ontime', 'late'))
tests_18m <- tests_18m %>% mutate(show_up_18m = ifelse(ExaminationAge >=15 & ExaminationAge <=19, 'ontime', 'late'))
tests_24m <- tests_24m %>% mutate(show_up_24m = ifelse(ExaminationAge >=21 & ExaminationAge <=27, 'ontime', 'late'))


### remove irrelevant tests
tests_12m$T12M_knows_one_body_part <- 999
tests_18m$T18M_2Y_one_pharse <- 999

## build an outcome variable ##
## count number of positive tests
tests_2m$positive_2m <- rowSums(tests_2m == "n")
tests_4m$positive_4m <- rowSums(tests_4m == "n")
tests_6m$positive_6m <- rowSums(tests_6m == "n")
tests_9m$positive_9m <- rowSums(tests_9m == "n")
tests_12m$positive_12m <- rowSums(tests_12m == "n")
tests_18m$positive_18m <- rowSums(tests_18m == "n")
tests_24m$positive_24m <- rowSums(tests_24m == "n")

# set binary variables for outcome variables
tests_2m <- tests_2m %>% mutate(outcome_2m = ifelse(positive_2m >=1, 1, 0)) 
tests_4m <- tests_4m %>% mutate(outcome_4m = ifelse(positive_4m >=1, 1, 0)) 
tests_6m <- tests_6m %>% mutate(outcome_6m = ifelse(positive_6m >=1, 1, 0)) 
tests_9m <- tests_9m %>% mutate(outcome_9m = ifelse(positive_9m >=1, 1, 0)) 
tests_12m <- tests_12m %>% mutate(outcome_12m = ifelse(positive_12m >=1, 1, 0)) 
tests_18m <- tests_18m %>% mutate(outcome_18m = ifelse(positive_18m >=1, 1, 0)) 
tests_24m <- tests_24m %>% mutate(outcome_24m = ifelse(positive_24m >=1, 1, 0)) 

#### merge all data sets ####
### build function for remove irrelevant columns ### using dplyr
remove_col <- function(df) {
  select(df, -ExaminationDate, -ExaminationAge)
}

### apply function
tests_2m_removed <- remove_col(tests_2m)
tests_4m_removed <- remove_col(tests_4m)
tests_6m_removed <- remove_col(tests_6m)
tests_9m_removed <- remove_col(tests_9m)
tests_12m_removed <- remove_col(tests_12m)
tests_18m_removed <- remove_col(tests_18m)
tests_24m_removed <- remove_col(tests_24m)

## merge data sets until 12 months##
outcome_df <- merge(x = tests_2m_removed, y = tests_4m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df <- merge(x = outcome_df, y = tests_6m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df <- merge(x = outcome_df, y = tests_9m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df <- merge(x = outcome_df, y = tests_12m_removed, by = c("PersonId", "BirthDate"), all = T)

## merge data sets with all tests ##
outcome_df2 <- merge(x = tests_2m_removed, y = tests_4m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df2 <- merge(x = outcome_df2, y = tests_6m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df2 <- merge(x = outcome_df2, y = tests_9m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df2 <- merge(x = outcome_df2, y = tests_12m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df2 <- merge(x = outcome_df2, y = tests_18m_removed, by = c("PersonId", "BirthDate"), all = T)
outcome_df2 <- merge(x = outcome_df2, y = tests_24m_removed, by = c("PersonId", "BirthDate"), all = T)


# add didn't show up value to show_up columns
outcome_df[c("show_up_2m", "show_up_4m", "show_up_6m", "show_up_9m", "show_up_12m")][is.na(outcome_df[c("show_up_2m", "show_up_4m", "show_up_6m", "show_up_9m", "show_up_12m")])] <- "no_showup"
outcome_df2[c("show_up_2m", "show_up_4m", "show_up_6m", "show_up_9m", "show_up_12m", "show_up_18m", "show_up_24m")][is.na(outcome_df2[c("show_up_2m", "show_up_4m", "show_up_6m", "show_up_9m", "show_up_12m", "show_up_18m", "show_up_24m")])] <- "no_showup"

write_csv(outcome_df, "C:/Users/danie/Desktop/thesis/data_sets/data_processed/outcome_df.csv")
write_csv(outcome_df2, "C:/Users/danie/Desktop/thesis/data_sets/data_processed/outcome_df2.csv")


######## population's information ########

#### import files of population information #### using readr
pop_info <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/exposure_info_raw/pop_info.csv")
settlement <- read_csv("C:/Users/danie/Desktop/thesis/data_sets/exposure_info_raw/settlement.csv")

### clean irrelevant data ### using dplyr
pop_info <- pop_info %>% select(-NOx_first4weeks, -NOx_firstyear)

## merge files ##
pop_info_united <- merge(x = pop_info, y = settlement, by = "PersonId", all.x = T)

# create sector column #
# exploring settlements' columns 
settlement_table <- as.data.frame(table(pop_info_united$SettlementName))
#haredi settlements on dataset: Elad, Bnei Brak, Beitar, Modiin Ilit, Rehasim

# create the sector column
pop_info_united$sector <- pop_info_united$pop_group

#define haredi as someone who lives in main haredi settlements
pop_info_united$sector[pop_info_united$SettlementName == "אלעד"]<- "Haredi"
pop_info_united$sector[pop_info_united$SettlementName == "בני ברק"] <- "Haredi"
pop_info_united$sector[pop_info_united$SettlementName == 'בית"ר עילית'] <- "Haredi"
pop_info_united$sector[pop_info_united$SettlementName == "מודיעין עילית"] <- "Haredi"
pop_info_united$sector[pop_info_united$SettlementName == "רכסים"] <- "Haredi"

#define other Jewish as Jewish not Haredi
pop_info_united$sector[pop_info_united$sector == "Jewish"] <- "Jewish not haredi"

#sector frequencies table
#table(pop_info_united$sector)

#remove irrelevant columns and edit columns names #using dplyr
pop_info_united <- pop_info_united %>%
  filter(child_yob > 2014) %>%
  select(-settl_flag) %>%
  rename("SIE" = "IV_2017or2008_lamas",
         "mother_country" = "mother_country_cat_short")

# record values to categorical for the model 
#record the SIE variable into quintiles # using lvmisc
pop_info_united$SIE_quin <- divide_by_quantile(pop_info_united$SIE, 5, na.rm = T)

pop_info_united <- pop_info_united %>% relocate(SIE_quin, .after = SIE)

#export file # using writexl (since there are hebrew strings)
#write_xlsx(pop_info_united, "C:/Users/danie/Desktop/thesis/data_sets/data_processed/pop_info_united.xlsx")

##### merge outcome file with population information file #####
tipat_halav_df <- merge(x = outcome_df, y = pop_info_united, by = "PersonId", all = T)
tipat_halav_df2 <- merge(x = outcome_df2, y = pop_info_united, by = "PersonId", all = T)


##### export file # using readr
write_csv(tipat_halav_df, "C:/Users/danie/Desktop/thesis/data_sets/data_processed/tipat_halav_df.csv")
write_csv(tipat_halav_df2, "C:/Users/danie/Desktop/thesis/data_sets/data_processed/tipat_halav_df2.csv")





