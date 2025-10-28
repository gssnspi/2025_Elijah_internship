#loading of packages####

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)


#loading of covid data set####
covid_data<- read_csv("C:/Users/Dell/Desktop/covid19-by-Elijah/tested_worldwide.csv")
getwd()


#View the first few rows and structure of the data set####
view(covid_data)

#Filter and select relevant columns thus keeping country_Region, total_tested, and daily_positive####
covid_filtered <- covid_data %>%
  filter(!is.na(total_tested), !is.na(daily_positive)) %>%
  select(Country_Region, total_tested, daily_positive)


#Aggregate and calculate summary statistics of  the covid_filtered data set and also it calculate the sum of the total_tested and daily_positive and after takes the aggregate of them####
summary_stats <- covid_filtered %>%
  group_by(Country_Region) %>%
  summarize(
    total_tests = sum(total_tested),
    total_positive_cases = sum(daily_positive),
    positivity_rate = (total_positive_cases / total_tests) * 100
  ) %>%
  arrange(desc(positivity_rate))

#Top 10 countries by total tests and we select the top 10 countries with the most or highest total test recorded####
top_test_countries <- summary_stats %>%
  arrange(desc(total_tests)) %>%
  slice_head(n = 10)


#Top 10 countries by positive rate and select the highest positive text in a descending order thus from highest to the lowest####
top_positive_countries <- summary_stats %>%
  arrange(desc(positivity_rate)) %>%
  slice_head(n = 10)

#Create vectors for country names and positivity rates####
top_countries <- top_positive_countries$Country_Region
positivity_rates <- top_positive_countries$positivity_rate


# Create a matrix with country names and positivity rates####
results_matrix <- cbind(top_countries, positivity_rates)


# Compile results into a list####
results_list <- list(
  summary_statistics = summary_stats,
  top_test_countries = top_test_countries,
  top_positive_countries = top_positive_countries,
  findings_matrix = results_matrix
)



# Plot the top 10 countries by positivity rate with stacked bar####
ggplot(summary_stats %>% slice_max(positivity_rate, n = 10),
       aes(x = reorder(Country_Region, positivity_rate),
           y = positivity_rate,
           fill = Country_Region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#512305FF", "#BCAF49FF", "#090A04FF", "#3E3C19FF", "#E5A335FF",
                               "#993D38FF", "#845852FF", "#E98440FF", "#5A5448FF", "#053C29FF")) +
  labs(title = "Top 10 Countries by COVID-19 Positivity Rate",
       x = "Country",
       y = "Positivity Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")




#Plot the top 10 countries by positivity rate with scatter plot####
ggplot(summary_stats %>% slice_max(positivity_rate, n = 10),
       aes(x = total_tests,
           y = positivity_rate,
           color = Country_Region)) +  
  geom_point(size = 4) +
  geom_text(aes(label = Country_Region), vjust = -0.8, size = 3) +
  labs(
    title = "Top 10 Countries by COVID-19 Positivity Rate",
    x = "Total Tests",
    y = "Positivity Rate (%)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c(
    "#AC5A42FF", "#865C1EFF", "#BCAF49FF", "#746D28FF", "#3E3C19FF",
    "#191C0CFF","#651F10FF", "#A86249FF", "#B19D6FFF",   "#E5A335FF"
    
  )) +                               
  theme(
    legend.position = "none",        
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )



