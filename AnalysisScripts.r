---
title: "Sleep Patterns and Psychological Well-being: An In-depth Analysis of College Students"
author: "Emma Hoffmann"
date: "2023-11-8"
---

```{r}
```
# Question 1

# Load the readxl library
library(readxl)

# Set data from excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Separate data into male and female groups
female_scores <- data$StressScore[data$Gender == 0]
male_scores <- data$StressScore[data$Gender == 1]

# Perform a two-sample t-test
t_test_result <- t.test(female_scores, male_scores)

# Print the results
print(t_test_result)

# Create a boxplot to visualize the data
# Use col argument to specify colors (red for female, blue for male)
boxplot(data$StressScore ~ data$Gender, xlab = "Gender", ylab = "Stress Score", main = "Stress Scores by Gender", col = c("red", "blue"))

```{r}
```
# Question 2

# Load the readxl library
library(readxl)

# Set data from Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Reorder the levels of the "Stress" variable to place "normal" on the left and "high" on the right
data$Stress <- factor(data$Stress, levels = c("normal", "high"))

# Perform a two-sample t-test
t_test_result <- t.test(ClassesMissed ~ Stress, data = data)

# Rename the sample estimates
t_test_result$estimate <- c("mean in normal" = t_test_result$estimate[1], "mean in high" = t_test_result$estimate[2])

# Print the t-test result with the modified sample estimate labels
t_test_result


# Create a boxplot with "normal" in green and "high" in red
boxplot_plot <- ggplot(data, aes(x = Stress, y = ClassesMissed, fill = Stress)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "green", "high" = "red")) +
  labs(title = "Boxplot of Classes Missed by Stress Level", x = "Stress Level", y = "Classes Missed") +
  theme(plot.title = element_text(hjust = 0.5))

# Print the boxplot
print(boxplot_plot)

```{r}
```
# Question 3

# Load the readxl library
library(readxl)

# Set data from the Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Reorder the levels of the "Stress" variable to place "normal" on the left and "high" on the right
data$Stress <- factor(data$Stress, levels = c("normal", "high"))

# Separate data into two groups: normal stress and high stress
normal_stress_missed <- data$ClassesMissed[data$Stress == "normal"]
high_stress_missed <- data$ClassesMissed[data$Stress == "high"]

# Perform a Welch's two-sample t-test
t_test_result <- t.test(normal_stress_missed, high_stress_missed, var.equal = FALSE)

# Create a boxplot to visualize the data with normal on the left and high on the right
# Use col argument to specify colors (green for "normal" and red for "high")
boxplot(ClassesMissed ~ Stress, data = data, main = "Classes Missed by Stress Levels", col = c("green", "red"))

# Switch the order of the sample estimates and retitle them
t_test_result$estimate <- c(t_test_result$estimate[2], t_test_result$estimate[1])
names(t_test_result$estimate) <- c("mean of normal", "mean of high")

# Print the Welch's two-sample t-test result
t_test_result

```{r}
```
# Question 4

# Load libraries
library(ggplot2)
library(readxl)

# Set data from the Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Calculate the correlation coefficient between Drinks and ClassesMissed
correlation_coefficient <- cor(data$Drinks, data$ClassesMissed)

# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Create a scatter plot to visualize the data with a correlation line and centered title
ggplot(data, aes(x = Drinks, y = ClassesMissed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Add a linear regression line
  xlab("Number of Drinks") +
  ylab("Number of Classes Missed") +
  ggtitle("Correlation Between Drinks and Classes Missed") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```
# Question 5

# Load libraries
library(ggplot2)
library(readxl)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Calculate the correlation coefficient between DepressionScore and ClassesMissed
correlation_coefficient <- cor(data$DepressionScore, data$ClassesMissed)

# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Create a scatter plot to visualize the data with a correlation line and centered title
ggplot(data, aes(x = DepressionScore, y = ClassesMissed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Add a linear regression line
  xlab("Depression Score") +
  ylab("Number of Classes Missed") +
  ggtitle("Correlation Between Depression Scores and Classes Missed") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```
# Question 6

# Load libraries
library(ggplot2)
library(readxl)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Calculate the correlation coefficient between AnxietyScore and ClassesMissed
correlation_coefficient <- cor(data$AnxietyScore, data$ClassesMissed)

# Print the correlation coefficient
cat("Correlation Coefficient:", correlation_coefficient, "\n")

# Create a scatter plot to visualize the data with a correlation line and centered title
ggplot(data, aes(x = AnxietyScore, y = ClassesMissed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Add a linear regression line
  xlab("Anxiety Score") +
  ylab("Number of Classes Missed") +
  ggtitle("Correlation Between Anxiety Scores and Classes Missed") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```
# Question 7

# Load libraries
library(ggplot2)
library(readxl)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Filter data for Larks
larks_data <- data[data$LarkOwl == "Lark", ]

# Filter data for Owls
owls_data <- data[data$LarkOwl == "Owl", ]

# Check for non-numeric or missing values in the "GPA" column for Larks
larks_data$GPA <- as.numeric(as.character(larks_data$GPA))
larks_data <- larks_data[!is.na(larks_data$GPA), ]

# Check for non-numeric or missing values in the "GPA" column for Owls
owls_data$GPA <- as.numeric(as.character(owls_data$GPA))
owls_data <- owls_data[!is.na(owls_data$GPA), ]

# Calculate the average GPA for Larks
average_gpa_larks <- mean(larks_data$GPA)

# Calculate the average GPA for Owls
average_gpa_owls <- mean(owls_data$GPA)

# Print the average GPAs for Larks and Owls
cat("Average GPA for Larks: ", average_gpa_larks, "\n")
cat("Average GPA for Owls: ", average_gpa_owls, "\n")

# Create a box plot to compare GPA distributions for Larks and Owls
ggplot(rbind(larks_data, owls_data), aes(x = factor(LarkOwl), y = GPA, fill = LarkOwl)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Lark" = "skyblue", "Owl" = "mediumpurple")) +  # Set colors
  labs(x = "LarkOwl Group", y = "GPA") +
  ggtitle("GPA: Larks vs. Owls") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend
        plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```
# Question 8

# Load libraries
library(ggplot2)
library(readxl)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Check for non-numeric or missing values in the three columns
data$DepressionScore <- as.numeric(as.character(data$DepressionScore))
data$AnxietyScore <- as.numeric(as.character(data$AnxietyScore))
data$StressScore <- as.numeric(as.character(data$StressScore))

# Filter out any NA or NaN values in the three columns
data <- data[!is.na(data$DepressionScore) & !is.na(data$AnxietyScore) & !is.na(data$StressScore), ]

# Perform a correlation analysis
correlation_matrix <- cor(data[c("DepressionScore", "AnxietyScore", "StressScore")])

# Print the correlation matrix
print(correlation_matrix)

# Create scatterplots with centered titles
scatterplot_depression_anxiety <- ggplot(data, aes(x = DepressionScore, y = AnxietyScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs. Anxiety") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

scatterplot_depression_stress <- ggplot(data, aes(x = DepressionScore, y = StressScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Depression vs. Stress") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

scatterplot_stress_anxiety <- ggplot(data, aes(x = StressScore, y = AnxietyScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Stress vs. Anxiety") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Display the scatterplots
print(scatterplot_depression_anxiety)
print(scatterplot_depression_stress)
print(scatterplot_stress_anxiety)

```{r}
```
# Question 9

# Load libraries
library(readxl)
library(ggplot2)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Specify the order of levels for AlcoholUse
data$AlcoholUse <- factor(data$AlcoholUse, levels = c("Abstain", "Light", "Moderate", "Heavy"))

# Create a boxplot with specified colors for each category and remove the legend
plot <- ggplot(data, aes(x = AlcoholUse, y = DASScore, fill = AlcoholUse)) +
  geom_boxplot() +
  xlab("Alcohol Use") +
  ylab("DASScore") +
  ggtitle("DAS Score by Alcohol Use") +
  scale_fill_manual(values = c("Abstain" = "snow3", "Light" = "blue", "Moderate" = "yellow", "Heavy" = "red")) +  # Assign colors
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  guides(fill = FALSE)  # Remove the legend

# Print the box plot
print(plot)

# Calculate the mean DASSScore for each level of AlcoholUse
mean_scores <- tapply(data$DASScore, data$AlcoholUse, mean)

# Print the mean scores
print(mean_scores)

```{r}
```
# Question 10

# Load libraries
library(ggplot2)
library(readxl)

# Set data from an Excel file
data <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 2/SleepStudy.xlsx")

# Create a scatter plot with a centered title and central tendency line
ggplot(data, aes(x = Happiness, y = DASScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add linear regression line
  xlab("Happiness Levels") +
  ylab("DASScore") +
  ggtitle("Happiness vs. DASScore") +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# Calculate the correlation coefficient
correlation <- cor(data$Happiness, data$DASScore)

# Print the correlation coefficient
print(paste("Correlation Coefficient: ", correlation))

```{r}
```