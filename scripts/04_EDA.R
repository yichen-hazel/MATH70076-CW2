##################################### EDA ######################################


require("ggplot2")
require("patchwork")
require("MASS")
require("dplyr")
require("forcats")

library(ggplot2)
library(patchwork) 
library(MASS)
library(dplyr)
library(forcats)



# import data
analysis_data <- readRDS("data/processed/analysis_data.rds")
summary(analysis_data)

# choose the reference category for categorical variable
analysis_data$race <- relevel(analysis_data$race, ref = "Non-Hispanic White")
analysis_data$edu <- relevel(analysis_data$edu, ref = "College graduate or above")
analysis_data$birth_country <- relevel(analysis_data$birth_country, ref = "US-born")

# drop the variable with high missing rate (after view the summary)
analysis_data <- analysis_data[, !names(analysis_data) %in% c("drinks_per_day", "hay_fever")]

# Plot 1: Density of PHQ_total
mean_phq <- mean(analysis_data$PHQ_total, na.rm = TRUE)

ggplot(analysis_data, aes(x = PHQ_total)) +
  geom_density(fill = "#5C88DA99", color = "#1f2d50", alpha = 0.7) +
  geom_vline(xintercept = mean_phq, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Figure 1: Density Plot of PHQ Total Score",
       x = "PHQ Total Score",
       y = "Density",
       subtitle = paste("Mean =", round(mean_phq, 2))) +
  theme_minimal()

ggsave("graph/F1_phq_density.png", width = 6, height = 4, dpi = 300)



############################# Categorical Variables ############################

########## 1.Correlated variable for PHQ score #################################

# Plot 2: Density by Gender
p2 <- ggplot(analysis_data, aes(x = PHQ_total, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Figure 2: PHQ Total Score by Gender", x = "PHQ Total Score", y = "Density") +
  theme_minimal()

ggsave("graph/F2_phq_density_by_gender.png", p2, width = 6, height = 4, dpi = 300)
print(p2)

# Plot 3: Violin by Asthma
p3 <- ggplot(analysis_data, aes(x = has_asthma, y = PHQ_total, fill = has_asthma)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  labs(title = "Figure 3: PHQ Total Score\nby Asthma Diagnosis", x = "Has Asthma", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/F3_phq_by_asthma.png", p3, width = 6, height = 4, dpi = 300)
print(p3)

# Plot 4: Heavy Drinking
custom_colors <- c( "#CC0C0099", "#5C88DA99", "#84BD0099", "#FFCD0099", "#7C878E99", "#00B5E299", "#00AF6699")


p4 <- ggplot(analysis_data, aes(x = heavy_drinker, y = PHQ_total, fill = heavy_drinker)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors[1:length(unique(na.omit(analysis_data$heavy_drinker)))]) +
  labs(title = "Figure 4: PHQ Total Score\n by Heavy Drinking", x = "Heavy Drinker", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/F4_phq_by_heavy_drinker.png", p4, width = 6, height = 4, dpi = 300)
print(p4)


# Plot 5: Liver Condition
p5 <- ggplot(analysis_data, aes(x = has_liver, y = PHQ_total, fill = has_liver)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors[1:length(unique(na.omit(analysis_data$has_liver)))]) +
  labs(title = "Figure 5: PHQ Total Score\n by Liver Condition", x = "Has Liver Condition", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/F5_phq_by_liver.png", p5, width = 6, height = 4, dpi = 300)
print(p5)


# Plot 6: Race/Ethnicity
###### Create race label for better vis
analysis_data$race_label <- recode(analysis_data$race,
                                   "Non-Hispanic White" = "Non\nHispanic\nWhite",
                                   "Mexican American" = "Mexican\nAmerican",
                                   "Non-Hispanic Asian" = "Non\nHispanic\nAsian",
                                   "Non-Hispanic Black" = "Non\nHispanic\nBlack",
                                   "Other Hispanic" = "Other\nHispanic",
                                   "Other Race - Including Multi-Racial" = "Other Race\nMulti-Racial"
)

###### Boxplot
p6_1 <- ggplot(analysis_data, aes(x = race_label, y = PHQ_total, fill = race_label)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors[1:length(unique(analysis_data$race_label))]) +
  labs(title = "Figure 6:PHQ Total Score by Race", x = "Race/Ethnicity", y = "PHQ Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none")

###### Pie Chart
race_dist <- analysis_data %>%
  count(race_label) %>%
  mutate(percentage = round(100 * n / sum(n), 1),
         label = paste0(race_label, "\n", percentage, "%"))

p6_2 <- ggplot(race_dist, aes(x = "", y = n, fill = race_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors[1:nrow(race_dist)]) +
  labs(title = "         Race Distribution") +
  theme_void() +
  theme(legend.position = "none")

(p6_1 | p6_2) + plot_layout(widths = c(1.7, 1))
ggsave("graph/F6_phq_by_race&pie.png", width = 6, height = 4, dpi = 300)




# Summary Table of has_asthma, diabetes, birth_country
##### has_asthma
asthma_summary <- analysis_data %>%
  group_by(group = has_asthma) %>%
  summarise(
    count = n(),
    mean_PHQ = round(mean(PHQ_total, na.rm = TRUE), 2),
    median_PHQ = median(PHQ_total, na.rm = TRUE),
    sd_PHQ = round(sd(PHQ_total, na.rm = TRUE), 2),
    IQR_PHQ = IQR(PHQ_total, na.rm = TRUE)
  ) %>%
  mutate(variable = "Has Asthma")

##### diabetes
diabetes_summary <- analysis_data %>%
  group_by(group = diabetes) %>%
  summarise(
    count = n(),
    mean_PHQ = round(mean(PHQ_total, na.rm = TRUE), 2),
    median_PHQ = median(PHQ_total, na.rm = TRUE),
    sd_PHQ = round(sd(PHQ_total, na.rm = TRUE), 2),
    IQR_PHQ = IQR(PHQ_total, na.rm = TRUE)
  ) %>%
  mutate(variable = "Diabetes")

##### birth_country
birth_summary <- analysis_data %>%
  group_by(group = birth_country) %>%
  summarise(
    count = n(),
    mean_PHQ = round(mean(PHQ_total, na.rm = TRUE), 2),
    median_PHQ = median(PHQ_total, na.rm = TRUE),
    sd_PHQ = round(sd(PHQ_total, na.rm = TRUE), 2),
    IQR_PHQ = IQR(PHQ_total, na.rm = TRUE)
  ) %>%
  mutate(variable = "Birth Country")

# Combine all summaries
combined_summary <- bind_rows(asthma_summary, diabetes_summary, birth_summary) %>%
  dplyr::select(variable, group, count, mean_PHQ, median_PHQ, sd_PHQ, IQR_PHQ)

print(combined_summary)


# Boxplot for has_asthma
ggplot(analysis_data, aes(x = has_asthma, y = PHQ_total, fill = has_asthma)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "PHQ Total Score by Asthma Diagnosis", x = "Has Asthma", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/phq_by_asthma.png", width = 6, height = 4, dpi = 300)

# Boxplot for diabetes
ggplot(analysis_data, aes(x = diabetes, y = PHQ_total, fill = diabetes)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "PHQ Total Score by Diabetes Status", x = "Has Diabetes", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/phq_by_diabetes.png", width = 6, height = 4, dpi = 300)


# Boxplot for birth_country
ggplot(analysis_data, aes(x = birth_country, y = PHQ_total, fill = birth_country)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "PHQ Total Score by Country of Birth", x = "Birth Country", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/phq_by_birth_country.png", width = 6, height = 4, dpi = 300)



########## 2.Uncorrelated variable for PHQ score ###############################
# Boxplot for ever_smoked
ggplot(analysis_data, aes(x = ever_smoked, y = PHQ_total, fill = ever_smoked)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "PHQ Total Score by Smoking History", x = "Ever Smoked", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("graph/phq_by_smoke.png", width = 6, height = 4, dpi = 300)

# Boxplot for has_cancer
ggplot(analysis_data, aes(x = has_cancer, y = PHQ_total)) +
  geom_boxplot() +
  labs(title = "PHQ Total Score by Cancer") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
ggsave("graph/phq_by_cancer.png", width = 6, height = 4, dpi = 300)

# Boxplot for military
ggplot(analysis_data, aes(x = military, y = PHQ_total)) +
  geom_boxplot() +
  labs(title = "PHQ Total Score by Military Service") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

ggsave("graph/phq_by_military.png", width = 6, height = 4, dpi = 300)


# Boxplot for Eductaion
analysis_data <- analysis_data %>%
  mutate(
    edu_combined = recode(edu,
                          "Less than 9th grade" = "Less than high school",
                          "9-11th grade (Includes 12th grade with no diploma)" = "Less than high school",
                          "High school graduate/GED or equivalent" = "High school\nor GED",
                          "Some college or AA degree" = "Some college\nor AA degree",
                          "College graduate or above" = "College\ngraduate or above"
    )
  )
ggplot(analysis_data, aes(x = edu_combined, y = PHQ_total, fill = edu_combined)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors[1:length(unique(analysis_data$edu_combined))]) +
  labs(title = "PHQ Total Score by Education Level", x = "Education Level", y = "PHQ Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none")

ggsave("graph/phq_by_edu.png", width = 6, height = 4, dpi = 300)



############################# Numerical Variables ##############################

########## 1.Correlated variable for PHQ score #################################

# sedentary_minutes
p7 <- ggplot(analysis_data %>% filter(!is.na(sedentary_minutes), !is.na(PHQ_total)),
             aes(x = sedentary_minutes, y = PHQ_total)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Figure 7: PHQ Total vs Sedentary Minutes",
       x = "Sedentary Minutes", y = "PHQ Total") +
  theme_minimal()
ggsave("graph/F7_phq_by_sedentary.png", width = 6, height = 4, dpi = 300)


# poverty_ratio
p8 <- ggplot(analysis_data %>% filter(!is.na(poverty_ratio), !is.na(PHQ_total)),
             aes(x = poverty_ratio, y = PHQ_total)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Figure 8: PHQ Total vs Poverty Ratio",
       x = "Poverty Ratio", y = "PHQ Total") +
  theme_minimal()
ggsave("graph/F8_phq_by_poverty_ratio.png", width = 6, height = 4, dpi = 300)

print(p7 + p8)


# age 
ggplot(analysis_data, aes(x = age, y = PHQ_total)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "PHQ_total vs age")

ggsave("graph/phq_by_age.png", width = 6, height = 4, dpi = 300)


########## 2.Uncorrelated variable for PHQ score ###############################

## scatter plot for age
ggplot(analysis_data, aes(x = age, y = PHQ_total)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "PHQ_total vs age")

ggsave("graph/phq_by_age.png", width = 6, height = 4, dpi = 300)


## scatter plot for household size
ggplot(analysis_data, aes(x = as.factor(hh_size), y = PHQ_total, fill = as.factor(hh_size))) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors[1:length(unique(na.omit(analysis_data$hh_size)))]) +
  labs(title = "PHQ Total Score by Household Size", x = "Household Size", y = "PHQ Total") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("graph/phq_by_household_size.png", width = 6, height = 4, dpi = 300)


