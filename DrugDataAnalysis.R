# In this project, I generate a sample dataset on R within the context of pharmacy 
# to demonstrate how to analyze utilizing various statistical techniques.

# First of all, we need to load the necessary libraries:
library(tibble)
library(knitr)
library(ggplot2)

# Create a sample dataset using pharmacy concepts.
pharma_data <- tibble(
  drug_name = sample(c("Drug A", "Drug B", "Drug C"), 5000, replace = TRUE),
  route = sample(c("Oral", "Intravenous", "Topical"), 5000, replace = TRUE),
  bioavailability = runif(5000, 0.1, 1),
  half_life = runif(5000, 1, 24)
)

# drug_name: Name of the drug, with three levels: "Drug A", "Drug B", and "Drug C".
# route: Administration for the drug, with three levels: "Oral", "Intravenous", and "Topical".
# bioavailability: Which is a measure of the proportion of the drug that reaches the systemic circulation after administration. It is randomly generated using the runif() function to generate random numbers between 0.1 and 1.
# half_life: Time it takes for the concentration of the drug in the body to decrease by half. It is randomly generated using the runif() function to generate random numbers between 1 and 24, representing hours.

# Display the dataset as a table using kable()
kable(pharma_data, caption = "Pharma-drug Dataset")

# Check the data types using summary () or typeof()
summary(pharma_data)
print(typeof(pharma_data$half_life))

# Compute basic descriptive stats:
sd(pharma_data$half_life)

# --------------------------------------------------------------------------------------------------------
# Want CHARTS/GRAPHS of our data using a few options to get some insights into the context. 

# Create a simple plot with route vs half-life variables:
p <- ggplot(data = pharma_data, aes(x = route, y = half_life))

p <- p + geom_point()

p <- p + ggtitle("Plot of Route vs Half-life")

plot(p)

# Add a Boxplot to see if it makes the data look better.
boxplot(pharma_data$bioavailability ~ pharma_data$drug_name, 
        data = df, 
        main = "Box Plot of Drugs vs Bioavailability",    # Add a title to the plot
        xlab = "Drug  Names",                             # Label the x-axis
        ylab = "Bioavailability",                         # Label the y-axis
        notch = TRUE,                                     # Add notches to the boxes
        outline = FALSE,                                  # Remove outliers
        col = c("lightblue", "lightgreen"),               # Set color of the boxes for each group
        border = "black"                                  # Set color of the box borders
)

# Create a different boxplot with 'Drug Name' on x-axis, 'Half-life' on y-axis, and 'Route' as grouping variable
p <- ggplot(data = pharma_data, aes(x = drug_name, y = half_life, fill = route)) +
  geom_boxplot() +
  labs(x = "Drug Names", y = "Half-life") +
  ggtitle("Boxplot of Half-life of Different Drugs Based on Administration") +
  theme_minimal()

print(p)

# Maybe we like to examine bioavailability, half-life for different drugs all together:
p <- ggplot(data = pharma_data, aes(x = bioavailability, y = half_life, color = drug_name))

p <- p + geom_point()

p <- p + xlab("Bioavailability") + ylab("Half_life")

p <- p + ggtitle("Bioavailability and Half-life of All Drugs")

print(p)

# Create a butterfly chart using ggplot2
butterfly_chart <- ggplot(data = pharma_data, aes(x = drug_name)) +
  geom_col(aes(y = - half_life, fill = "half_life"), width = 0.4, position = "identity") +
  geom_col(aes(y = bioavailability, fill = "bioavailability"), width = 0.4, position = "identity") +
  scale_fill_manual(name = "Drug Characteristics", values = c("half_life" = "blue", "bioavailability" = "red"),
                    labels = c("half_life", "bioavailability")) +
  coord_flip() +
  labs(title = "Butterfly Chart",
       x = "Drug Name",
       y = "Values") +
  theme_minimal()

print(butterfly_chart)

# BETTER looking butterfly chart:
# Create sample data
data <- data.frame(Category = c("Category 1", "Category 2", "Category 3", "Category 4"),
                   Value1 = c(20, 30, 40, 50),
                   Value2 = c(-10, -25, -15, -5))

butterfly_chart <- ggplot(data, aes(x = Category)) +
  geom_col(aes(y = Value1, fill = "Value 1"), width = 0.4, position = "identity") +
  geom_col(aes(y = Value2, fill = "Value 2"), width = -0.4, position = "identity") +
  scale_fill_manual(name = "Value", values = c("Value 1" = "green", "Value 2" = "purple"),
                    labels = c("Value 1", "Value 2")) +
  coord_flip() +
  labs(title = "Butterfly Chart",
       x = "Category",
       y = "Value") +
  theme_minimal()

print(butterfly_chart)

# --------------------------------------------------------------------------------------------------------
# Next, we do some sample Data Analysis

# Chi-squared test for independence: This test is applied for categorical variables.

# Create a contingency table for drug_name and route
contingency_table <- table(pharma_data$drug_name, pharma_data$route)

result <- chisq.test(contingency_table)

print(result)
# Since p-value is above 0.05, we fail to reject the null hypothesis of 
# independence between the "drug_name" and "route" variables, and 
# conclude that there is no evidence of an association between these two variables.


# Simple Linear Regression Model for simple predictions
# Q1 - Is there a relationship between drug half-life and route?
# Y = B1(X1) + e | where Y is response variable, X1, X2 are predictors, e is error term.

model <- lm(half_life ~ route, data = pharma_data, family = binomial)

summary(model)
# Interpret: We do not see any significant association between route and drug half-life.
# R-squared gives you the variation in response variable explained by the model.


# Multiple Linear Regression Model for simple predictions
# Q2 - What other variables are associated with drug half-life?
# Y = B1(X1) + B2(X2) + e | where Y is response variable, X1, X2 are predictors, e is error.

model <- lm(half_life ~ route + drug_name + bioavailability, data = pharma_data, family = binomial)

summary(model)
# Interpret: It is expected that Drug C increased half-life by 0.43 units with a statistical 
# significance at p = 0.1 (rather than p = 0.05) when all the other variables held constant.


# Compute the correlation matrix. It can only be applied to numeric values.
pharma_data <- tibble(
  drug_name = sample(c("Drug A", "Drug B", "Drug C"), 5000, replace = TRUE),
  route = sample(c("Oral", "Intravenous", "Topical"), 5000, replace = TRUE),
  bioavailability = runif(5000, 0.1, 1),
  half_life = runif(5000, 1, 24)
)

df <- data.frame(pharma_data)

numeric_vars <- df[, sapply(df, is.numeric)]

cor_matrix <- cor(numeric_vars)

print(cor_matrix)

# Interaction effect (synergy) - it's an important aspect of data analysis. 
# Usually variables that are higly correlated would be considered for this. 

model <- lm(half_life ~ route*drug_name + bioavailability, data = pharma_data, family = binomial)

summary(model)

# In order to make our analysis more sophisticated, we can turn it into a classification problem.
# Example Logit function
x <- seq(-6, 6, by = 0.1)
y <- exp(x) / (1 + exp(x))

plot(x, y, type = "l", lty = 1, lwd = 2, ylim = c(0, 1), 
     xlab = "Predictor (X)", ylab = "Probability of Outcome (Y)", 
     main = "Logistic Regression Function")

# Fit a logistic regression model. But we need to convert our bioavailibility into a binary variable - low/high.
pharma_data <- tibble(
  drug_name = sample(c("Drug A", "Drug B", "Drug C"), 5000, replace = TRUE),
  route = sample(c("Oral", "Intravenous", "Topical"), 5000, replace = TRUE),
  bioavailability = as.factor(ifelse(runif(5000) > 0.5, "High", "Low")),
  half_life = runif(5000, 1, 24)
)

model <- glm(bioavailability ~ route + half_life, data = pharma_data, family = binomial)

summary(model)

# We take exp() of coefficients to compute odds-ratio in percentage:
# Extract coefficient estimates, standard errors, and p-values
coef_estimates <- coef(summary(model))[, "Estimate"]
std_errors <- coef(summary(model))[, "Std. Error"]
p_values <- coef(summary(model))[, "Pr(>|z|)"]

# Compute odds ratios and convert to percentages
odds_ratios <- exp(coef_estimates)
odds_ratios_percent <- (odds_ratios - 1) * 100

# Create a data frame to store the results
results <- data.frame(
  Predictor = names(coef_estimates),
  Coefficient = coef_estimates,
  Standard_Error = std_errors,
  P_Value = p_values,
  Odds_Ratio = odds_ratios,
  Odds_Ratio_Percent = odds_ratios_percent
)

print(results)

# Odds_Ratio_Percent column as the percentage change in the odds of the binary response variable
# for a one-unit increase in the corresponding predictor variable, holding other variables constant.

# Example Interpretation: The Odds_Ratio_Percent for "Topical" is -4%. 
# This means that for a one-unit increase in the route being "Topical", while holding other 
# variables constant, the odds of the binary outcome ("High" bio-availability) 
# is expected to decrease by 4%.
