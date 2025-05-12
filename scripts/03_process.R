require("readr")
require("dplyr")
require("stringr")
library(readr)
library(dplyr)
library(stringr)

combined_data <- readRDS("data/combined/combined_data.rds")

############################### Process the DPQ ################################
phq_levels <- c(
  "Not at all" = 0,
  "Several days" = 1,
  "More than half the days" = 2,
  "Nearly every day" = 3
)

phq_items <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
               "DPQ060", "DPQ070", "DPQ080", "DPQ090")


combined_data <- combined_data %>%
  mutate(across(all_of(phq_items), ~ phq_levels[.])) %>%
  mutate(PHQ_total = rowSums(across(all_of(phq_items)), na.rm = FALSE))


combined_data <- combined_data %>%
  mutate(PHQ_total = rowSums(across(all_of(phq_items)), na.rm = FALSE))


################################ Process the DEMO ##############################

## Gender
combined_data$gender <- factor(combined_data$RIAGENDR)

## Age
combined_data$age <- as.numeric(combined_data$RIDAGEYR)

## Race / Ethnicity
combined_data$race <- factor(combined_data$RIDRETH3)

## Education
combined_data$edu <- ifelse(
  combined_data$DMDEDUC2 %in% c("Don't Know", "Don't know", "Refused"),
  NA,
  combined_data$DMDEDUC2
)

combined_data$edu <- factor(
  combined_data$edu,
  ordered = FALSE,
  levels = c(
    "Less than 9th grade",
    "9-11th grade (Includes 12th grade with no diploma)",
    "High school graduate/GED or equivalent",
    "Some college or AA degree",
    "College graduate or above"
  )
)

## Country of Birth
combined_data$birth_country <- case_when(
  combined_data$DMDBORN4 %in% c("Born in 50 US states or Washington, DC",
                                "Born in 50 US states or Washington,") ~ "US-born",
  combined_data$DMDBORN4 == "Others" ~ "Foreign-born",
  TRUE ~ NA_character_
)
combined_data$birth_country <- factor(combined_data$birth_country)

## Military
combined_data$military <- ifelse(combined_data$DMQMILIZ %in% c("Refused", "Don't Know"), 
                                 NA, combined_data$DMQMILIZ)
combined_data$military <- factor(combined_data$military)


## Pregnancy Status
combined_data$pregnancy <- case_when(
  combined_data$RIDEXPRG == "Yes, positive lab pregnancy test or self-reported pregnant at exam" ~ "Pregnant",
  combined_data$RIDEXPRG == "The participant was not pregnant at exam" ~ "Not pregnant",
  TRUE ~ NA_character_
)
combined_data$pregnancy <- factor(combined_data$pregnancy)

## Household Size
hh_raw <- ifelse(combined_data$DMDHHSIZ == "7 or more people in the Household",
                 "7", combined_data$DMDHHSIZ)
combined_data$hh_size <- as.numeric(hh_raw)


## poverty ratio
combined_data$poverty_ratio <- as.numeric(combined_data$INDFMPIR)
################################ Process the ALQ ###############################
## Average drink
combined_data$drinks_per_day <- as.numeric(combined_data$ALQ130)
combined_data$drinks_per_day <- ifelse(
  combined_data$drinks_per_day >= 1 & combined_data$drinks_per_day <= 15,
  combined_data$drinks_per_day,
  NA
)

## Drink a lot 
combined_data$heavy_drinker <- ifelse(
  combined_data$ALQ151 %in% c("Yes", "No"),
  combined_data$ALQ151,
  NA
)
combined_data$heavy_drinker <- factor(combined_data$heavy_drinker, levels = c("No", "Yes"))

############################### Process the SMQ ################################

## Ever smoked
combined_data$ever_smoked <- ifelse(
  combined_data$SMQ020 %in% c("Yes", "No"),
  combined_data$SMQ020,
  NA
)
combined_data$ever_smoked <- factor(combined_data$ever_smoked, levels = c("No", "Yes"))

## current smoke
combined_data$current_smoke_status <- ifelse(
combined_data$SMQ040 %in% c("Not at all", "Some days", "Every day"),
combined_data$SMQ040,
NA
)
combined_data$current_smoke_status <- factor(
  combined_data$current_smoke_status,
  levels = c("Not at all", "Some days", "Every day")
)

############################### Process the PAQ ################################

## Sedentary minutes
combined_data$sedentary_minutes <- as.numeric(combined_data$PAD680)
combined_data$sedentary_minutes <- ifelse(
  combined_data$sedentary_minutes %in% c(7777, 9999) |
    combined_data$sedentary_minutes < 0 |
    combined_data$sedentary_minutes > 1440,  # max 1440 minuets per day
  NA,
  combined_data$sedentary_minutes
)

############################### Process the DIQ ################################

## Diabetes

combined_data$diabetes <- ifelse(
  combined_data$DIQ010 %in% c("Yes", "No"),
  combined_data$DIQ010,
  NA
)
combined_data$diabetes <- factor(combined_data$diabetes, levels = c("No", "Yes"))

############################### Process the MCQ ################################

## Asthma
combined_data$has_asthma <- ifelse(
  combined_data$MCQ010 %in% c("Yes", "No"),
  combined_data$MCQ010,
  NA
)
combined_data$has_asthma <- factor(combined_data$has_asthma, levels = c("No", "Yes"))


## hay_fever
combined_data$hay_fever <- ifelse(
  combined_data$AGQ030 %in% c("Yes", "No"),
  combined_data$AGQ030,
  NA
)
combined_data$hay_fever <- factor(combined_data$hay_fever, levels = c("No", "Yes"))


## liver
combined_data$has_liver <- ifelse(
  combined_data$MCQ160L %in% c("Yes", "No"),
  combined_data$MCQ160L,
  NA
)
combined_data$has_liver <- factor(combined_data$has_liver, levels = c("No", "Yes"))


## cancer
combined_data$has_cancer <- ifelse(
  combined_data$MCQ220 %in% c("Yes", "No"),
  combined_data$MCQ220,
  NA
)
combined_data$has_cancer <- factor(combined_data$has_cancer, levels = c("No", "Yes"))

######################### New dataset with selected variables ##################

analysis_data <- combined_data %>%
  dplyr::select(
    SEQN, gender, race, edu, birth_country, military, heavy_drinker, 
    ever_smoked, diabetes, has_asthma, hay_fever, has_liver, 
    has_cancer, PHQ_total, age, hh_size, drinks_per_day, 
    sedentary_minutes, poverty_ratio
  )

summary(analysis_data)

analysis_data <- analysis_data %>%
  filter(if_all(c(
    "edu", "birth_country", "military",
    "ever_smoked", "PHQ_total", "has_asthma", "sedentary_minutes", "has_liver"
  ), ~ !is.na(.)))

summary(analysis_data)
saveRDS(analysis_data, file = "data/processed/analysis_data.rds")


