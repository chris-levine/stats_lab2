install.packages("moderndive")

library("ggplot2")
library("dplyr")
library("readr")
library("moderndive")

# reading in csv file
df <- read_csv("/Users/christopherlevine/Desktop/Stats_Inference/Lab 2/lex.csv")

# dropping nas
cleaned_df <- na.omit(df)

# removing specific row
cleaned_df <- cleaned_df %>%
  filter(country != "Hong Kong, China")

# getting 1923 df
life_expectancy_1923 <- cleaned_df %>%
  select(country, "1923") %>%
  rename(life_expectancy_1923 = "1923")

# getting 2023 df 
life_expectancy_2023 <- cleaned_df %>%
  select(country, "2023") %>%
  rename(life_expectancy_2023 = "2023")

# histogram for life expectancy in 1923
ggplot(life_expectancy_1923, aes(x = life_expectancy_1923)) + 
  geom_histogram(binwidth = 5) + 
  labs(title = "Life Expectancy in 1923",
       x = "Life Expectancy",
       y = "Count")

# histogram for life expectancy in 2023
ggplot(life_expectancy_2023, aes(x = life_expectancy_2023)) + 
  geom_histogram(binwidth = 5) + 
  labs(title = "Life Expectancy in 2023",
       x = "Life Expectancy",
       y = "Count")

# filtering data for both 1923 and 2023
life_expectancy_both <- cleaned_df %>%
  select(country, "1923", "2023") %>%
  rename(life_expectancy_1923 = "1923",
         life_expectancy_2023 = "2023")

# creating a scatter plot 
ggplot(life_expectancy_both, aes(x=life_expectancy_1923, y=life_expectancy_2023)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Life Expectancy in 2023 vs. 1923",
       x = "Life Expectancy in 1923",
       y = "Life Expectancy in 2023")

# getting correlation
life_expectancy_both %>%
  get_correlation(formula = life_expectancy_2023 ~ life_expectancy_1923)

# getting model
score_model <- lm(life_expectancy_2023 ~ life_expectancy_1923, data = life_expectancy_both)

get_regression_table(score_model)

# making residuals 
residuals <- residuals(score_model)

# adding residuals to the plot 
life_expectancy_both <- life_expectancy_both %>%
  mutate(residuals = residuals)

# making scatterplot of residuals 
ggplot(life_expectancy_both, aes(x = life_expectancy_1923, y = residuals)) +
  geom_point() + 
  labs(title= "Residuals against Life Expectancy in 1923",
       x = "Life Expectancy in 1923",
       y = "Residuals")

# making histogram of residuals
ggplot(life_expectancy_both, aes(x = residuals)) +
  geom_histogram() +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Count")

# extracting the variablilty
model_summary <- summary(score_model)
model_summary

##################################################################################################
# PART TWO

library("countrycode")

# adding a column with country code
life_expectancy_1923$continent <- countrycode(sourcevar = life_expectancy_1923[["country"]],
                                    origin = "country.name",
                                    destination = "continent")

# looking how many countries there are per continent
country_counts <- life_expectancy_1923 %>% 
  group_by(continent) %>%
  summarise(number_of_countries = n())

country_counts

# creating side by side boxplot 
ggplot(life_expectancy_1923, aes(x=continent, y = life_expectancy_1923)) + 
  geom_boxplot() + 
  labs(title = "Life Expectancy in 1923 by Continent",
       x = "Continent",
       y = "Life Expectancy in 1923")

# creating a histogram of life expectancy by continent
ggplot(life_expectancy_1923, aes(x = life_expectancy_1923)) + 
  geom_histogram() + 
  facet_wrap(~ continent) +
  labs(title = "Life Expectancy in 1923 by Continent",
       x = "Life Expectancy in 1923",
       y = "Count")

# making a summary table
summary_table <- life_expectancy_1923 %>%
  group_by(continent) %>%
  summarise(
    mean_life_expectancy_1923 = mean(life_expectancy_1923, na.rm = TRUE),
    median_life_expectancy_1923 = median(life_expectancy_1923, na.rm = TRUE)
  )

summary_table

# creating lm model
score_model <- lm(life_expectancy_1923 ~ continent, data = life_expectancy_1923)

get_regression_table(score_model)

# doing everything again for 2023
life_expectancy_2023$continent <- countrycode(sourcevar = life_expectancy_2023[["country"]],
                                              origin = "country.name",
                                              destination = "continent")

ggplot(life_expectancy_2023, aes(x=continent, y = life_expectancy_2023)) + 
  geom_boxplot() + 
  labs(title = "Life Expectancy in 2023 by Continent",
       x = "Continent",
       y = "Life Expectancy in 2023")

ggplot(life_expectancy_2023, aes(x = life_expectancy_2023)) + 
  geom_histogram() + 
  facet_wrap(~ continent) +
  labs(title = "Life Expectancy in 2023 by Continent",
       x = "Life Expectancy in 2023",
       y = "Count")

summary_table <- life_expectancy_2023 %>%
  group_by(continent) %>%
  summarise(
    mean_life_expectancy_2023 = mean(life_expectancy_2023, na.rm = TRUE),
    median_life_expectancy_2023 = median(life_expectancy_2023, na.rm = TRUE)
  )

summary_table

score_model <- lm(life_expectancy_2023 ~ continent, data = life_expectancy_2023)

get_regression_table(score_model)
