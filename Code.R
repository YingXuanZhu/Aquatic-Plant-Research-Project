library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

###### 1.Data Preprocessing
# Set working directory
setwd("C:/Users/DELL/Desktop/dataset")  
getwd()

# Import data
site_data <- read.csv("MACP_OPEN_DATA_SITE.csv", stringsAsFactors = FALSE)
metrics_data <- read.csv("MACP_OPEN_DATA_METRICS.csv", stringsAsFactors = FALSE)
taxa_data <- read.csv("MACP_OPEN_DATA_TAXA.csv", stringsAsFactors = FALSE)
taxon_info <- read.csv("OPEN_DATA_TAXON_INFO.csv", stringsAsFactors = FALSE)

## Do not have to merge all datasets, avoid many-to-many relationships
site <- subset(site_data,select = c(SITE_ID,ALKALINITY,BASE_DATA_DATE))
write.csv(site, "site.csv", row.names = FALSE)

## Select necessary columns from datasets for merging
taxa <- subset(taxa_data,select = c(ANALYSIS_ID,TAXON_LIST_ITEM_KEY,PERCENTAGE_COVER_BAND))
metrics <- subset(metrics_data,select = c(SITE_ID,ANALYSIS_ID,SAMPLE_DATE))
taxon <- subset(taxon_info,select = c(TAXON_LIST_ITEM_KEY,TAXON_NAME,TAXON_TYPE))
# Use same columns to merge dataset
merged_data1 <- taxa %>%
  inner_join(metrics, by = "ANALYSIS_ID") %>%
  inner_join(taxon, by = "TAXON_LIST_ITEM_KEY")

# Check unique values in TAXON_TYPE column
unique_taxon_type <- unique(merged_data1$TAXON_TYPE)
print(unique_taxon_type)
# Keep only rows where TAXON_TYPE is Macrophytes
merged_data2 <- subset(merged_data1, TAXON_TYPE == "Macrophytes")
write.csv(merged_data2, "merged_data2.csv", row.names = FALSE)

# Convert date column to date format
merged_data2$SAMPLE_DATE <- dmy(merged_data2$SAMPLE_DATE)
# Extract year
merged_data2$YEAR <- year(merged_data2$SAMPLE_DATE)
write.csv(merged_data2, "merged_data2.csv", row.names = FALSE)

# Check the range of years and data count per year
year_summary <- merged_data2 %>%
  group_by(YEAR) %>%
  summarize(count = n())
print(year_summary, n = Inf)
# noting that data is more abundant from 1994 onwards.


###### 2.Species Richness
# Calculate the total number of different aquatic plant species (species richness) per year.
yearly_species_richness <- merged_data2 %>%
  group_by(YEAR) %>%
  summarise(Species_Richness = n_distinct(TAXON_NAME)) %>%
  arrange(YEAR)

# Visualize species richness trend
ggplot(yearly_species_richness, aes(x = YEAR, y = Species_Richness)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", linewidth = 1) +  
  labs(title = "Aquatic Plant Species Richness Trend",
       x = "Year",
       y = "Species Richness") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Check the range of years and data count per year
year_summary1 <- merged_data2 %>%
  group_by(YEAR) %>%
  summarize(count = n(), TAXON_NAME_na_count = sum(is.na(TAXON_NAME)))
print(year_summary1, n = Inf) 
# noting that data is more abundant from 1994 onwards.

#### Filter Data From 1994
yearly_species_richness1 <- merged_data2 %>%
  filter(YEAR >= 1994) %>%
  group_by(YEAR) %>%
  summarise(Species_Richness = n_distinct(TAXON_NAME)) %>%
  arrange(YEAR)

# Visualize species richness trend (From 1994)
ggplot(yearly_species_richness1, aes(x = YEAR, y = Species_Richness)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "red", linewidth = 1) +  
  labs(x = "Year",
       y = "Species Richness") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 




###### 3.Species Abundance
# To quantify species abundance, convert cover band to approximate percentage values 
# simplified using the midpoint of each range.
cover_band_to_pct <- c(`1` = 0.05, `2` = 0.55, `3` = 1.75, `4` = 3.75, `5` = 7.5, 
                       `6` = 17.5, `7` = 37.5, `8` = 62.5, `9` = 87.5)
# Convert cover band to cover percentage and add to dataset
merged_data2$cover_percentage <- cover_band_to_pct[as.character(merged_data2$PERCENTAGE_COVER_BAND)]
write.csv(merged_data2, "merged_data2.csv", row.names = FALSE)


### 3.1 Individual Species Abundance
# # Calculate the average coverage rate of each species per year
species_avg_cover <- merged_data2 %>%
  filter(!is.na(cover_percentage)) %>%  #remove NaN
  group_by(YEAR, TAXON_NAME) %>%
  summarize(mean_cover_percentage = mean(cover_percentage, na.rm = TRUE))

# Check the range of years and data count per year
# noting that data is more abundant from 1994 onwards.
year_summary2 <- merged_data2 %>%
  group_by(YEAR) %>%
  summarize(count = n(), na_count = sum(is.na(cover_percentage)))
print(year_summary2, n = Inf)
# Define long-term and short-term year ranges
long_term_years <- c(1994, 2023)
short_term_years <- c(2019, 2020, 2021, 2022, 2023)
# Extract data for specific time periods
long_term_data <- species_avg_cover %>%
  filter(YEAR %in% long_term_years)
short_term_data <- species_avg_cover %>%
  filter(YEAR %in% short_term_years)

# Calculate long-term and short-term !!growth rate!!
long_term_changes <- long_term_data %>%
  spread(key = YEAR, value = mean_cover_percentage) %>%
  mutate(growth_rate = (get(as.character(long_term_years[2])) - get(as.character(long_term_years[1]))) / 30) %>%
  filter(!is.na(growth_rate)) # Remove Growth rate = NA

short_term_changes <- short_term_data %>%
  spread(key = YEAR, value = mean_cover_percentage) %>%
  mutate(growth_rate = (get(as.character(short_term_years[5])) - get(as.character(short_term_years[1]))) / 5) %>%
  filter(!is.na(growth_rate)) # Remove Growth rate = NA

# Determine long-term trends
long_term_trend <- long_term_changes %>%
  mutate(trend = case_when(
    growth_rate > 0 ~ "Increase",
    growth_rate == 0 ~ "No change",
    growth_rate < 0 ~ "Decrease"
  ))
# Determine short-term trends
short_term_trend <- short_term_changes %>%
  mutate(trend = case_when(
    growth_rate > 0 ~ "Increase",
    growth_rate == 0 ~ "No change",
    growth_rate < 0 ~ "Decrease"
  ))

# Add period labels
long_term_trend <- long_term_trend %>% mutate(period = "Long term")
short_term_trend <- short_term_trend %>% mutate(period = "Short term")
# Combine long-term and short-term trends
trend_data <- bind_rows(long_term_trend, short_term_trend)

# Set factor levels for trend types
trend_data$trend <- factor(trend_data$trend, levels = c("Increase","No change","Decrease"))
# Calculate percentage of each trend type for long-term and short-term
trend_summary <- trend_data %>%
  group_by(period, trend) %>%
  summarize(species_count = n(), .groups = 'drop') %>%
  group_by(period) %>%
  mutate(percentage = species_count / sum(species_count) * 100)

# Create stacked bar chart
ggplot(trend_summary, aes(x = period, y = percentage, fill = trend)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Increase" = "green",
                               "No change" = "grey",
                               "Decrease" = "red")) +
  labs(x = "Time period", 
       y = "Percentage of species", 
       fill = "Trend") +
  theme_minimal()


### 3.2 All Species Abundance in the UK
# Calculate the Average Coverage Rates of All Species Per Year 
# by averaging the average coverage rate of each species per year
avg_cover_year <- species_avg_cover %>%
  group_by(YEAR) %>%
  summarize(mean_cover_percentage_year = mean(mean_cover_percentage, na.rm = TRUE))

# Plot All Species Abundance trend
ggplot(avg_cover_year, aes(x = YEAR, y = mean_cover_percentage_year)) +
  geom_line(color = "blue", linewidth = 1) +  
  geom_point(color = "blue", size = 2) +  
  geom_smooth(method = "loess", se = TRUE, color = "red", linewidth = 1) +  
  labs(x = "Year", 
       y = "Mean Percentage Cover  (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) + 
  theme(plot.title = element_text(hjust = 0.5)) 

#### Filter data from 1994
avg_cover_year1 <- species_avg_cover %>%
  filter(YEAR >= 1994) %>%
  group_by(YEAR) %>%
  summarize(mean_cover_percentage_year = mean(mean_cover_percentage, na.rm = TRUE))

# Figure
ggplot(avg_cover_year1, aes(x = YEAR, y = mean_cover_percentage_year)) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_point(color = "blue", size = 2) +  
  geom_smooth(method = "loess", se = TRUE, color = "red", linewidth = 1) +  
  labs(x = "Year", 
       y = "Mean Percentage Cover  (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +  
  theme(plot.title = element_text(hjust = 0.5))  



###### 4.Alkalinity £¡affects£¡ Water Quality
# Convert date column to date format
site$BASE_DATA_DATE <- dmy(site$BASE_DATA_DATE)
# Extract year
site$YEAR <- year(site$BASE_DATA_DATE)

# Filter out clearly incorrect year values and ALKALINITY = NA
current_year <- year(Sys.Date())
site <- site %>%
  filter(!is.na(ALKALINITY)) %>%  #Remove NA
  filter(YEAR >= 1980 & YEAR <= current_year)
write.csv(site, "site.csv", row.names = FALSE)

# Plot alkalinity over time for each site
ggplot(site, aes(x = YEAR, y = ALKALINITY)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "red") +  
  labs(x = "Year",
       y = "Alkalinity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



###### 5. Relationship between Alkalinity and Species Abundance
## Merge all datasets
merged_data <- merged_data2 %>%
  inner_join(site, by = c("SITE_ID", "YEAR")) %>%
  filter(!is.na(cover_percentage))
# Only all rows with matching SITE_ID and YEAR column values are retained.
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

## Check the Uniqueness of alkalinity for each site Every Year
alkalinity_check <- merged_data %>%
  group_by(SITE_ID,YEAR) %>%
  summarize(unique_alkalinity_count = n_distinct(ALKALINITY))
# Check if all sites have a Unique alkalinity value Every Year (TRUE)
all_unique <- all(alkalinity_check$unique_alkalinity_count == 1)
print(all_unique)

## Calculate the average cover percentage for each site per year
yearly_site_avg_cover <- merged_data %>%
  group_by(SITE_ID,YEAR) %>%
  summarize(mean_cover_percentage = mean(cover_percentage, na.rm = TRUE),
            ALKALINITY = first(ALKALINITY)) # Because ALKALINITY is unique per SITE_ID


###!! ALKALINITY 
## 1.Correlation
correlation <- cor(yearly_site_avg_cover$ALKALINITY, yearly_site_avg_cover$mean_cover_percentage, use = "complete.obs")
print(paste("Correlation between Alkalinity and Mean Cover Percentage: ", correlation))

## 2.Fit a linear regression model
lm_model<- lm(mean_cover_percentage ~ ALKALINITY, data = yearly_site_avg_cover)
# Check the model results
summary(lm_model)

## 3.Residual Analysis
par(mfrow = c(2, 2))  
plot(lm_model) 
par(mfrow = c(1, 1))  

## 4.Plot the relationship between ALKALINITY and mean_cover_percentage, colored by YEAR
ggplot(yearly_site_avg_cover, aes(x = ALKALINITY, y = mean_cover_percentage, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Relationship between Alkalinity and Species Abundance",
       x = "Alkalinity",
       y = "Mean Cover Percentage",
       color = "Year") +
  theme_minimal()+
  theme(plot.title = element_text(size = 11)) +
  guides(color = guide_legend(ncol = 2)) # Set the legend to have two columns


###!! ALKALINITY + YAER
# Fit a linear regression model including YEAR as a predictor
lm_model_with_year <- lm(mean_cover_percentage ~ ALKALINITY + as.factor(YEAR), data = yearly_site_avg_cover)
summary(lm_model_with_year)

# Plot the relationship between ALKALINITY and mean_cover_percentage, colored by YEAR with regression lines
ggplot(yearly_site_avg_cover, aes(x = ALKALINITY, y = mean_cover_percentage, color = as.factor(YEAR))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, aes(group = as.factor(YEAR)), color = "red") + 
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  #
  labs(x = "Alkalinity",
       y = "Mean Percentage Cover ",
       color = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11)) +
  guides(color = guide_legend(ncol = 2)) # Set the legend to have two columns

# Use facet_wrap to generate separate figures for each year
ggplot(yearly_site_avg_cover, aes(x = ALKALINITY, y = mean_cover_percentage)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(x = "Alkalinity",
       y = "Mean Percentage Cover ") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9, hjust = 0.5),
        legend.position = "none",
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6)) +
  facet_wrap(~ YEAR, ncol = 6)  # Use facet_wrap to plot each year data






###### 6. Relationship between Taxon Sensitivity and Growth Rate (Long Term)
RMNI <- read.csv("RMNI.csv", stringsAsFactors = FALSE)
# Merge the RMNI values with growth rate data and remove NAs
long_term_trend_rmni <- long_term_trend %>%
  inner_join(RMNI, by = "TAXON_NAME")

### Linear Regression Analysis
## 1.correlation 
correlation <- cor(long_term_trend_rmni$RMNI, long_term_trend_rmni$growth_rate)
print(paste("Correlation between RMNI and growth rate:", correlation))

## 2.Fit a linear regression model
lm_model1<- lm(growth_rate ~ RMNI, data = long_term_trend_rmni)
summary(lm_model1)

## 3.Residual Analysis
par(mfrow = c(2, 2))  
plot(lm_model1) 
par(mfrow = c(1, 1))  

## 4.Visualize
ggplot(long_term_trend_rmni, aes(x = RMNI, y = growth_rate)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "RMNI",
       y = "Species Abundance Growth Rate (%)") +
  theme_minimal()+
  theme(plot.title = element_text(size = 12))



###### 7. Relationship between Species level Traits(Life Form) and Growth rate
GrowthRate <- long_term_trend %>%
  select(TAXON_NAME, growth_rate, trend)

life_form <- read.csv("life form - systema.csv", stringsAsFactors = FALSE)

# Use inner join, keep only records present in both datasets
species_level_traits <- merge(GrowthRate, life_form, by.x = "TAXON_NAME", by.y = "Taxon", all = FALSE)
# Remove rows where the life.form...systema column is empty
cleaned_data <- species_level_traits %>% filter(life.form...systema != "")

###### 1. Use three kinds of Growth Rate trend
# Calculate the percentage of different trends for each life form
life_form_trend <- cleaned_data %>%
  group_by(life.form...systema, trend) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Manually set the order of the y-axis
life_form_trend$trend <- factor(life_form_trend$trend, levels = c("Increase", "No change", "Decrease"))

# Create stacked bar chart
ggplot(life_form_trend, aes(x = life.form...systema, y = percentage, fill = trend)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "green",
                               "No change" = "grey",
                               "Decrease" = "red")) +
  labs(x = "Life Form",
       y = "Percentage",
       fill = "Trend") +
  theme(plot.title = element_text(size = 12))


###### !Check for relationship
# Create contingency table
contingency_table <- xtabs(count ~ life.form...systema + trend, data = life_form_trend)
print(contingency_table)

# 1. Chi-squared test for independence
# Chi-squared test is a statistical method used to test the independence of two categorical variables.
chisq_test <- chisq.test(contingency_table)
print(chisq_test)
# This warning message indicates that the Chi-squared test approximation may be inaccurate due to low expected frequencies in some cells.
# The Chi-squared test assumes that the expected frequency in each cell is at least greater than 5, otherwise, the test results may be inaccurate.


# 2. Fisher's Exact Test
# Suitable for small sample sizes or cases with low expected frequencies, 
# calculating exact probabilities to determine the association between categorical variables.
# This is an exact test method and does not rely on the size of the expected frequencies.
fisher_test <- fisher.test(contingency_table)
print(fisher_test)
# p > 0.05, indicating that the relationship between Life Form and Trend is not statistically significant, 
# suggesting that they may be independent.




###### 2. Use the specific value of Growth Rate
# Descriptive Statistics
summary_data <- cleaned_data %>%
  group_by(life.form...systema) %>%
  summarise(mean_growth_rate = mean(growth_rate, na.rm = TRUE),
            sd_growth_rate = sd(growth_rate, na.rm = TRUE))
print(summary_data)

# Plot Boxplot
ggplot(cleaned_data, aes(x = life.form...systema, y = growth_rate, fill = life.form...systema)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Life Form",
       y = "Species Abundance Growth Rate")

# One-Way ANOVA
library(car)
anova_model <- aov(growth_rate ~ life.form...systema, data = cleaned_data)
summary(anova_model)



# Calculate the number of species in each life form category
species_count <- table(cleaned_data$life.form...systema)

# Add species count to life form labels
cleaned_data$life.form...systema <- factor(cleaned_data$life.form...systema,
                                           levels = names(species_count),
                                           labels = paste0(names(species_count), " (n=", species_count, ")"))

# Plot with updated labels
ggplot(cleaned_data, aes(x = life.form...systema, y = growth_rate, fill = life.form...systema)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Life Form",
       y = "Species Abundance Growth Rate",
       fill = "Life Form (n=species count)") +
  theme(legend.position = "none")



