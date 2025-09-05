# Begin by loading the necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Set the file path and read the dataset
data_path <- "/home/adeoni/Pictures/assessment/cw_r.xlsx"
all_data <- readxl::read_excel(data_path)
sheets_in_file <- excel_sheets(data_path)
cat('Sheets in file:', sheets_in_file, '\n\n')

# Assign each table to a variable with a descriptive name
fraud_overview <- readxl::read_excel(data_path, sheet = "Table 3d")
crime_by_region <- readxl::read_excel(data_path, sheet = "Table 5", skip = 9)
personal_fraud_details <- readxl::read_excel(data_path, sheet = "Table 7")
region_fraud_profile <- readxl::read_excel(data_path, sheet = "Table 8")


# Define the column names to reflect the data they contain
col_names_fraud <- c("Type_of_Fraud", paste0("Year_", 2012:2021), "Change_in_Percentage")
fraud_trends <- read_excel(data_path, sheet = "Table 3d", skip = 8, col_names = col_names_fraud)
# Processing the data for fraud types
bank_fraud_data <- fraud_trends %>% filter(Type_of_Fraud == "Banking and credit industry fraud")
retail_fraud_data <- fraud_trends %>% filter(Type_of_Fraud == "Consumer and retail fraud [note 14]")

# Checking the structure of the datasets
head(bank_fraud_data)
str(bank_fraud_data)
head(retail_fraud_data)
str(retail_fraud_data)

# Converting relevant columns to numeric for plotting

bank_fraud_data <- bank_fraud_data %>% mutate(across(starts_with("Year"), as.numeric))
retail_fraud_data <- retail_fraud_data %>% mutate(across(starts_with("Year"), as.numeric))

# Reshaping the datasets for visualization
bank_fraud_long <- bank_fraud_data %>% pivot_longer(cols = starts_with("Year"), names_to = "Year", values_to = "Incidents")
retail_fraud_long <- retail_fraud_data %>% pivot_longer(cols = starts_with("Year"), names_to = "Year", values_to = "Incidents")
head(retail_fraud_long)




# Visualizing the fraud data with boxplots and jitter points
ggplot(bank_fraud_long, aes(x = Year, y = Incidents, group = 1)) +
  geom_line(color = 'palegreen3') + 
  geom_point(color = 'darkorange', size = 2) + 
  labs(title = "Banking Fraud Incidents Over Time", x = "Year", y = "Number of Incidents") +
  theme_light() 

# Revised plot for Retail Fraud using a line plot for a different visual representation
ggplot(retail_fraud_long, aes(x = Year, y = Incidents, group = 1)) +
  geom_line(color = 'dodgerblue3') + 
  geom_point(color = 'darkorange', size = 2) + # Changed point color and increased size
  labs(title = "Retail Fraud Incidents Over Time", x = "Year", y = "Number of Incidents") +
  theme_light() 


# Preprocessing data for regional crime statistics

# Clean the column names to remove backticks for easier reference
names(crime_by_region) <- c("Area_Code", "Area_Name", "Number_of_Offences", 
                            "Rate_per_1000_population", "Percent_Change_from_Previous_Year")

# We will first clean the data and ensure that all numerical columns are indeed numeric
data_clean <- crime_by_region %>%
  mutate(
    Number_of_Offences = as.numeric(gsub(",", "", Number_of_Offences)),
    Rate_per_1000_population = as.numeric(gsub("[^0-9.]", "", Rate_per_1000_population)),
    Percent_Change_from_Previous_Year = as.numeric(gsub("[^-0-9.]", "", Percent_Change_from_Previous_Year))
  ) %>%
  # Exclude the summary rows for England and Wales, focusing only on individual regions
  filter(!Area_Code %in% c("K04000001", "E92000001")) %>%
  # Remove rows with Area Codes that indicate they are not regions (e.g., counties)
  filter(!str_detect(Area_Code, "^E23"))


# Plot to show the regions with the highest total count of offences

ggplot(data_clean, aes(x = reorder(Area_Name, Number_of_Offences), y = Number_of_Offences)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Flip the coordinates for easier reading
  labs(title = "Total Count of Offences by Region", x = "Region", y = "Total Count of Offences")

# Plot to show the regions with the highest offence rate per 1000 population
ggplot(data_clean, aes(x = reorder(Area_Name, Rate_per_1000_population), y = Rate_per_1000_population)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() + # Flip the coordinates for easier reading
  labs(title = "Offence Rate per 1000 Population by Region", x = "Region", y = "Rate per 1000 Population")

# Identify the top three counties with the lowest total count of offences
county_data <- crime_data %>%
  filter(str_detect(Area_Code, "^(E23|W15)")) %>%
  mutate(Number_of_Offences = as.numeric(gsub(",", "", Number_of_Offences))) %>%
  arrange(Number_of_Offences)

# Get the top three counties with the lowest count of offences
top_three_counties_lowest_offences <- county_data %>%
  slice_head(n = 3)

# Print the results
print(top_three_counties_lowest_offences)

# Verify the selection
print(counties_with_lowest_offenses)

ggplot(top_three_counties_lowest_offences, aes(x = reorder(Area_Name, Number_of_Offences), y = Number_of_Offences, fill = Area_Name)) +
  geom_col() + # geom_col is used to create bar plots in ggplot2
  labs(title = "Top Three Counties with the Lowest Count of Offences",
       x = "County",
       y = "Count of Offences") +
  theme_minimal() + # Use a minimal theme for a cleaner look
  theme(legend.title = element_blank(), # Remove the legend title
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readabili









