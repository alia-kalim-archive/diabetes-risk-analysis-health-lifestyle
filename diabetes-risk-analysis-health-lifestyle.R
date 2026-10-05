# Load data
setwd("C:/Users/Alia/Downloads")

diab <- read.csv("Diabetes.csv")
file.rename("Untitled2.R", "diabetes-risk-analysis-health-lifestyle.R")

# Convert binary variables to factors for plotting
diab$Diabetes <- factor(diab$Diabetes, levels = c(0,1),
                        labels = c("No diabetes", "Diabetes"))
diab$HighBP   <- factor(diab$HighBP)
diab$HighChol <- factor(diab$HighChol)
diab$Smoker   <- factor(diab$Smoker)
diab$Fruits   <- factor(diab$Fruits)

# Table of counts and proportions

table(diab$HighBP, diab$Diabetes)
prop.table(table(diab$HighBP, diab$Diabetes), margin = 1)

table(diab$HighChol, diab$Diabetes)
prop.table(table(diab$HighChol, diab$Diabetes), margin = 1)

table(diab$Smoker, diab$Diabetes)
prop.table(table(diab$Smoker, diab$Diabetes), margin = 1)

table(diab$Fruits, diab$Diabetes)
prop.table(table(diab$Fruits, diab$Diabetes), margin = 1)

# Summary statistics for BMI and Age

aggregate(BMI ~ Diabetes, data = diab, summary)
aggregate(Age ~ Diabetes, data = diab, summary)

# Bar plots (proportion plots)

barplot(prop.table(table(diab$HighBP, diab$Diabetes), 1),
        beside = TRUE, legend = TRUE,
        main = "Table 1: Diabetes Status by High Blood Pressure",
        ylab = "Proportion")

barplot(prop.table(table(diab$HighChol, diab$Diabetes), 1),
        fill= 
        beside = TRUE, legend = TRUE,
        main = "Table 2: Diabetes Status by High Cholesterol",
        ylab = "Proportion")

barplot(prop.table(table(diab$Smoker, diab$Diabetes), 1),
        beside = TRUE, legend = TRUE,
        main = "Table 3: Diabetes Status by Smoking",
        ylab = "Proportion")

barplot(prop.table(table(diab$Fruits, diab$Diabetes), 1),
        beside = TRUE, legend = TRUE,
        main = "Table 4: Diabetes Status by Fruit Consumption",
        ylab = "Proportion")

# COLOURED AND GROUPED BARPLOTS

library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data in long format
diab_long <- diab %>%
  select(Diabetes, HighBP, HighChol, Smoker, Fruits) %>%
  pivot_longer(
    cols = c(HighBP, HighChol, Smoker, Fruits),
    names_to = "Predictor",
    values_to = "Category"
  )

# Custom colors
my_colors <- c("No diabetes" = "#FFC0CB", "Diabetes" = "#89CFF0")  # pick your hex codes or color names

# Plot
ggplot(diab_long, aes(x = Category, fill = Diabetes)) +
  geom_bar(position = "fill") +         # stacked proportions
  facet_wrap(~ Predictor, ncol = 2) +   # separate plots for each predictor
  scale_fill_manual(values = my_colors) +  # use your chosen colors
  labs(
    title = "Figure 2.1: Proportion of Diabetes by Binary Predictors",
    x = "Category",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


# Boxplots for continuous variables

par(mfrow = c(1, 2))  

# BMI boxplot with color
boxplot(BMI ~ Diabetes, data = diab,
        main = "Figure 2.2: BMI by Diabetes Status",
        ylab = "BMI",
        col = c("darkgreen", "#9CAF88"))

# Age boxplot with color
boxplot(Age ~ Diabetes, data = diab,
        main = "Figure 2.3: Age by Diabetes Status",
        ylab = "Age (years)",
        col = c("darkgreen", "#9CAF88"))

# Reset layout to default
par(mfrow = c(1, 1))

# Initial full model fit!

model_logit <- glm(Diabetes ~ HighBP + HighChol + BMI + Smoker + Fruits + Age,
                   data = diab,
                   family = binomial(link = "logit"))
summary(model_logit)

deviance(model_logit)       # Residual deviance
model_logit$null.deviance    
AIC(model_logit)             

m_no_fruits <- glm(Diabetes ~ HighBP + HighChol + BMI + Smoker + Age,
                   data = diab,
                   family = binomial(link = "logit"))

summary(m_no_fruits)

anova(model_logit, m_no_fruits, test = "Chisq")

AIC(m_no_fruits)  

m_no_smoker <- glm(Diabetes ~ HighBP + HighChol + BMI + Age,
                   data = diab,
                   family = binomial)

anova(m_no_fruits, m_no_smoker, test = "Chisq")
AIC(m_no_fruits, m_no_smoker)

# Predicted probabilities
pred_probs <- predict(m_no_fruits, type = "response")

# Use same labels as actual Diabetes factor
pred_class <- ifelse(pred_probs >= 0.5, "Diabetes", "No diabetes")
pred_class <- factor(pred_class, levels = c("No diabetes", "Diabetes"))

# Actual classes
actual_class <- diab$Diabetes  # already factor with "No diabetes", "Diabetes"

# Confusion matrix
conf_mat <- table(Predicted = pred_class, Actual = actual_class)
print(conf_mat)

# Overall accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
print(paste("Overall accuracy:", round(accuracy,3)))

# False positives / false negatives
false_positives <- conf_mat["Diabetes","No diabetes"]
false_negatives <- conf_mat["No diabetes","Diabetes"]
print(paste("False positives:", false_positives))
print(paste("False negatives:", false_negatives))

