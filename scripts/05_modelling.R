require("ggplot2")
require("patchwork")
require("dplyr")
require("forcats")
require("MASS")


library(ggplot2)
library(patchwork) 
library(dplyr)
library(forcats)
library(MASS)

# we dont want to update the processed data from R scrip "03_process.R"
# so we repeat the code of tiny change on dataset from "04_EDA.R"
# just redefine the reference category for better interpretation


# import data
analysis_data <- readRDS("data/processed/analysis_data.rds")

# choose the reference category for categorical variable
analysis_data$race <- relevel(analysis_data$race, ref = "Non-Hispanic White")
analysis_data$edu <- relevel(analysis_data$edu, ref = "College graduate or above")
analysis_data$birth_country <- relevel(analysis_data$birth_country, ref = "US-born")

# drop the variable with high missing rate (after view the summary)
analysis_data <- analysis_data[, !names(analysis_data) %in% c("drinks_per_day", "hay_fever")]

# fill the median for missing valuye
analysis_data <- analysis_data %>%
  mutate(poverty_ratio = ifelse(is.na(poverty_ratio),
                                median(poverty_ratio, na.rm = TRUE),
                                poverty_ratio))

# Apply to each categorical variable, make NA as an answer
analysis_data$heavy_drinker <- fct_explicit_na(as.factor(analysis_data$heavy_drinker), na_level = "NA")
analysis_data$diabetes <- fct_explicit_na(as.factor(analysis_data$diabetes), na_level = "NA")


############################## Possion Model ###################################
model_pois <- glm(PHQ_total ~ gender + race + birth_country + heavy_drinker +
                    diabetes + has_asthma + has_liver +
                    sedentary_minutes + poverty_ratio + age, family = poisson, data = analysis_data)
summary(model_pois)

dispersion <- sum(residuals(model_pois, type = "pearson")^2) / df.residual(model_pois)
print(dispersion)



########################### Negative binomial Model ############################

# 1. Fit full negative binomial model
model_nb <- glm.nb(
  PHQ_total ~ gender + race + birth_country + heavy_drinker +
    diabetes + has_asthma + has_liver +
    sedentary_minutes + poverty_ratio +age,
  data = analysis_data
)

# View summary and AIC
summary(model_nb)

cat("AIC Comparison for Negative binomial & Possion :\n")
AIC(model_pois, model_nb)



# 2. Fit reduced negative binomial model
# without race
model_nb_race <- glm.nb(
  PHQ_total ~ gender + birth_country + heavy_drinker +
    diabetes + has_asthma + has_liver +
    sedentary_minutes + poverty_ratio +age,
  data = analysis_data
)
# without heavy_drink
model_nb_heavy_drink <- glm.nb(
  PHQ_total ~ gender + race + birth_country +
    diabetes + has_asthma + has_liver +
    sedentary_minutes + poverty_ratio +age,
  data = analysis_data
)

# without diabetes
model_nb_diabetes <- glm.nb(
  PHQ_total ~ gender + race + birth_country + heavy_drinker +
    has_asthma + has_liver +
    sedentary_minutes + poverty_ratio +age,
  data = analysis_data
)

# View summary and AIC
cat("AIC Comparison for Negative Binomial model without race, heavy_drink, diabetes :\n")
AIC(model_nb, model_nb_race,model_nb_heavy_drink,model_nb_diabetes)


############################ Introduce Interaction #############################

########### 1.Visulize Interaction #############################################
# p8: Gender × Sedentary Minutes
p8 <- ggplot(analysis_data %>% filter(!is.na(sedentary_minutes), !is.na(PHQ_total)),
             aes(x = sedentary_minutes, y = PHQ_total, color = gender)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Figure 8: Interaction \n Gender × Sedentary Minutes",
       x = "Sedentary Minutes", y = "PHQ Total") +
  theme_minimal()

ggsave("graph/F8_gender_by_sedentary.png", plot = p8, width = 6, height = 4, dpi = 300)


# p9: Gender × Age
p9 <- ggplot(analysis_data %>% filter(!is.na(age), !is.na(PHQ_total)),
             aes(x = age, y = PHQ_total, color = gender)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Figure 9: Interaction \n Gender × Age",
       x = "Age", y = "PHQ Total") +
  theme_minimal()

ggsave("graph/F9_gender_by_age.png", plot = p9, width = 6, height = 4, dpi = 300)

print(p8 + p9)

# Has Asthma × Age
p10 <- ggplot(analysis_data %>% filter(!is.na(age), !is.na(PHQ_total), !is.na(has_asthma)),
              aes(x = age, y = PHQ_total, color = has_asthma)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Figure 10: Interaction - Has Asthma × Age",
       x = "Age", y = "PHQ Total") +
  theme_minimal()

ggsave("graph/F10_asthma_by_age.png", plot = p10, width = 6, height = 4, dpi = 300)

print(p10)

############ 2.Add Interaction into Model ######################################

# full model with all three interaction
model_nb_interact3 <- glm.nb(
  PHQ_total ~ gender * sedentary_minutes + 
    gender * age + 
    has_asthma * age +
    race + birth_country + heavy_drinker + 
    diabetes + has_liver + poverty_ratio,
  data = analysis_data
)

summary(model_nb_interact3)


# full model with all only one interaction (Gender × Age)
model_nb_interact1 <- glm.nb(
  PHQ_total ~ sedentary_minutes + 
    gender * age + 
    has_asthma+
    race + birth_country + heavy_drinker + 
    diabetes + has_liver + poverty_ratio,
  data = analysis_data
)


# Compare AIC with previous full model
AIC(model_nb, model_nb_interact3, model_nb_interact1)


############################## Model Diagnostics ###############################

############ 1.Goodness of fit #################################################
# Residual Deviance and Degrees of Freedom
resid_dev <- deviance(model_nb_interact1)
df_resid <- df.residual(model_nb_interact1)

cat("Residual Deviance:", resid_dev, "\n")
cat("Degrees of Freedom:", df_resid, "\n")
cat("Ratio (Deviance / DF):", resid_dev / df_resid, "\n")

# AIC
aic_val <- AIC(model_nb_interact1)
cat("AIC:", aic_val, "\n")

# Log-likelihood
loglik_val <- logLik(model_nb_interact1)
cat("Log-Likelihood:", loglik_val, "\n")

############ 2.Residual Diagnostics ############################################

# Residuals
df_diag <- data.frame(
  index = 1:nrow(analysis_data),
  pearson = residuals(model_nb_interact1, type = "pearson"),
  deviance = residuals(model_nb_interact1, type = "deviance"),
  fitted = fitted(model_nb_interact1),
  observed = analysis_data$PHQ_total,
  cooks = cooks.distance(model_nb_interact1)
)

# 1. Pearson residuals vs. index
pR1 <- ggplot(df_diag, aes(x = index, y = pearson)) +
  geom_point(alpha = 0.4, color = "#5C88DA", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_standard+
  labs(title = "Pearson Residuals", x = "Observation Index", y = "Residual")

# 2. Fitted vs Observed
pR2 <- ggplot(df_diag, aes(x = fitted, y = observed)) +
  geom_point(alpha = 0.4, color = "#84BD00", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_standard+
  labs(title = "Fitted vs Observed", x = "Fitted Values", y = "Observed PHQ_total")

print(pR1 + pR2)
ggsave("graph/Pearson_Fitted.png", width = 6, height = 4, dpi = 300)

# 3. QQ plot of deviance residuals
pR3 <- ggplot(df_diag, aes(sample = deviance)) +
  stat_qq(alpha = 0.4, color = "#FFCD00", size = 0.7) +
  stat_qq_line(color = "red") +
  theme_standard+
  labs(title = "QQ Plot of Deviance Residuals")

# 4. Cook's Distance
pR4 <- ggplot(df_diag, aes(x = index, y = cooks)) +
  geom_point(alpha = 0.4, color = "#00B5E2", size = 0.7) +
  geom_hline(yintercept = 4 / nrow(df_diag), linetype = "dashed", color = "red") +
  theme_standard+
  labs(title = "Cook's Distance", y = "Cook's D", x = "Observation Index")


print(pR3 + pR4)
ggsave("graph/QQ_cook.png", width = 6, height = 4, dpi = 300)



