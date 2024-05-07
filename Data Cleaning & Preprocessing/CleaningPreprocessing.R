# This file imports the raw data and performs all the cleaning and preprocessing 
# that our group deemed necessary for modeling and building member profiles
rm(list=ls())

# load the member and measure results table
df <- read.csv("MemberAndMeasureResults.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

# remove the extra member_id column created by the join
df$MEMBER_ID.1 <- NULL

# change the birth year category to age and remove the birth year column
library(lubridate)
df$age <- year(today()) - df$BIRTH_YEAR
df$BIRTH_YEAR <- NULL

# Create age classifications in case you want to use a categorical variable for age instead of continuous
df$age_Buckets <- ifelse(df$age < 65, 'Young', ifelse(df$age >= 65 & df$age <= 77, 'Average', 'Old'))

# load the claims line one data
df_claims_one_line <-  read.csv("claims_line_1.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")


# count the number of claims for each member 
library(dplyr)

Total_claims <- df_claims_one_line %>%
  filter(CLAIM_LINE_NUM == 1) %>%
  group_by(MEMBER_ID) %>%
  summarise(count = n())

Total_claims <- rename(Total_claims, "NUM_CLAIMS" = "count")

# join the new feature to the original dataframe, replacing NA with 0 claims
df <- left_join(df, Total_claims, by = "MEMBER_ID") %>% replace(is.na(.), 0)

# breaking down the total number of claims into category
# total medical claims per patient
# read in csv which is from a query in snowflake 
df_medical <- read.csv("MedicalCount.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

# merging the data frames
df <- merge(df, df_medical, by = "MEMBER_ID", all.x = TRUE, all.y = FALSE)

#total outpatient claims per patient
# read in csv which is from a query in snowflake 
df_outpatient <- read.csv("OutpatientCount.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

# merging the data frames
df <- merge(df, df_outpatient, by = "MEMBER_ID", all.x = TRUE, all.y = FALSE)

# total inpatient claims per patient
# read in csv which is from a query in snowflake 
df_Inpatient <- read.csv("InpatientCount.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

# merging the data frames
df <- merge(df, df_Inpatient, by = "MEMBER_ID", all.x = TRUE, all.y = FALSE)

# change NA values to 0 
df$MEDICAL_COUNT[is.na(df$MEDICAL_COUNT)] <- 0
df$OUTPATIENT_COUNT[is.na(df$OUTPATIENT_COUNT)] <- 0
df$INPATIENT_COUNT[is.na(df$INPATIENT_COUNT)] <- 0


# create three variables: the mean, median, and sum allowed amount for each member
# Calculate mean, median, sum of LINE_ALLOWED_AMT for each MEMBER_ID
df_claims <- read.csv("claims.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")
allowed_amount_stats <- df_claims %>%
  group_by(MEMBER_ID, CLAIM_NUM) %>%
  summarize(
    mean_allowed_amt = mean(LINE_ALLOWED_AMT),
    median_allowed_amt = median(LINE_ALLOWED_AMT),
    total_allowed_amt = sum(LINE_ALLOWED_AMT)
  )

#now run another group by to get the results for each member
stats_by_member <- allowed_amount_stats %>%
  group_by(MEMBER_ID) %>%
  summarize(
    mean_allowed_amt = mean(mean_allowed_amt, na.rm = TRUE),
    median_allowed_amt = median(median_allowed_amt, na.rm = TRUE),
    sum_allowed_amt = sum(total_allowed_amt, na.rm = TRUE)
  )

# join the new feature to the original dataframe
df <- left_join(df, stats_by_member, by = "MEMBER_ID") %>% replace(is.na(.), 0)


# Create a variable to identify the most common place of service for each member
# replace NA with unknown
df_claims_one_line$PLACE_OF_SVC_DESC <- ifelse(is.na(df_claims_one_line$PLACE_OF_SVC_DESC), "Unknown", df_claims_one_line$PLACE_OF_SVC_DESC)

# Group by MEMBER_ID and find the most common PLACE_OF_SVC_DESC
most_common_svc_location <- df_claims_one_line %>%
  group_by(MEMBER_ID) %>%
  summarize(most_common_svc_location = names(sort(table(PLACE_OF_SVC_DESC), decreasing = TRUE))[1])

# Replace "Unknown" with NA again
most_common_svc_location$most_common_svc_location[most_common_svc_location$most_common_svc_location == "Unknown"] <- NA

# join the new feature to the original dataframe
df <- left_join(df, most_common_svc_location, by = "MEMBER_ID") %>% replace(is.na(.), NA)


# create a feature that indicates the average amount of time in days between each claim for each member

# Convert LINE_FROM_DT to Date format if it's not already in Date format
df_claims_one_line$LINE_FROM_DT <- as.Date(df_claims_one_line$LINE_FROM_DT)

# Sort the data by MEMBER_ID and LINE_FROM_DT
df_claims_one_line <- df_claims_one_line %>% 
  arrange(MEMBER_ID, LINE_FROM_DT)

# Calculate time difference between consecutive claims
df_claims_one_line <- df_claims_one_line %>%
  group_by(MEMBER_ID) %>%
  mutate(time_diff = c(NA, diff(LINE_FROM_DT)))

# Calculate the average time difference between claims for each member in days
avg_time_diff <- df_claims_one_line %>%
  group_by(MEMBER_ID) %>%
  summarise(avg_days_between_claims = mean(time_diff, na.rm = TRUE))

# join the new feature to the original data frame
df <- left_join(df, avg_time_diff, by = "MEMBER_ID") %>% replace(is.na(.), NA)

# Create a new feature for the total number of chronic conditions
df$TOTAL_CONDITIONS <- rowSums(df[, c("ANXIETY_FLAG", "ASTHMA_FLAG", "DEPRESSION_FLAG", "DIABETES_FLAG")])


# Encoding the Sex variable, 1 if Male, 0 if female
df$sex <- ifelse(df$PAT_SEX == "M", 1, 0)
df$PAT_SEX <- NULL


# create an engagement percentage variable, this will be used as the target variable in regression problems
# this is done by dividing all compliance fields by eligibility fields
# if a patient is readmitted after 30 days it also decreases their engagement percentage accordingly
df$engagement_percentage <- round(((df$COLORECTAL_CANCER_SCR_COMPLIANCE + df$FLU_VAC_COMPLIANCE + df$BREAST_CANCER_SCR_COMPLIANCE + df$DIABETES_CARE_COMPLIANCE + df$READMISSION_ELIG)/(df$BREAST_CANCER_SCR_ELIG + df$COLORECTAL_CANCER_SCR_ELIG + df$FLU_VAC_ELIG + df$DIABETES_CARE_ELIG + df$READMISSION_FLAG)) * 100,2)


# create classification problem target variables
#making a variable to indicate a member having an engagement percentage of 0
df$no_engagement <- ifelse(df$engagement_percentage == 0, 1, 0)

# finding and average without 0 engagement percentage members to determine the cutoff point for medium and high engaged members
filtered_data <- df[df$engagement_percentage != 0, ]
average_without_zeros <- mean(filtered_data$engagement_percentage)

#make a column to indicate if a member has an engagement percentage greater than 0 and less the average without 0s
df$medium_engagment <- ifelse(df$engagement_percentage > 0 & df$engagement_percentage <= average_without_zeros, 1, 0)  

# creating column for high engagement
df$high_engagement <- ifelse(df$engagement_percentage > average_without_zeros, 1, 0)


# finding significant counties using a logistic regression model
df <- df %>% mutate(MBR_COUNTY = toupper(MBR_COUNTY))
#just using counties, determine the significance on the model for no engagement
county_no_engagement <- glm(no_engagement ~ MBR_COUNTY, data = df)
summary_data_low <- summary(county_no_engagement)


# Get the coefficients and corresponding p-values
coefficients_low <- summary_data_low$coefficients[, "Estimate"]
p_values_low <- summary_data_low$coefficients[, "Pr(>|t|)"]

# Combine coefficients and p-values into a data frame
coefficients_df_low <- data.frame(Coefficient = coefficients_low, P_Value = p_values_low)

# Sort the coefficients_df by p-values in ascending order
sorted_coefficients_low <- coefficients_df_low[order(coefficients_df_low$P_Value), ]

# Extract the top 5 counties with the lowest p-values
sorted_coefficients_low <- sorted_coefficients_low[-1, ]
top_5_counties_low <- head(sorted_coefficients_low, n = 5)
top_5_counties_low <- add_rownames(top_5_counties_low, var = "County")
top_5_counties_low$County <- gsub("^MBR_COUNTY", "", top_5_counties_low$County)


# Same process for the medium engaged counties
county_medium_engagement <- glm(medium_engagment ~ MBR_COUNTY, data = df)

#gives the p-value (significance) and the coefficient of the variable for medium engagement model 
summary_data_medium <- summary(county_medium_engagement)

# Get the coefficients and corresponding p-values
coefficients_medium <- summary_data_medium$coefficients[, "Estimate"]
p_values_medium <- summary_data_medium$coefficients[, "Pr(>|t|)"]

# Combine coefficients and p-values into a data frame
coefficients_df_medium <- data.frame(Coefficient = coefficients_medium, P_Value = p_values_medium)

# Sort the coefficients_df by p-values in ascending order
sorted_coefficients_medium <- coefficients_df_medium[order(coefficients_df_medium$P_Value), ]

# Extract the top 5 counties with the lowest p-values
sorted_coefficients_medium<- sorted_coefficients_medium[-1, ]
top_5_counties_medium <- head(sorted_coefficients_medium, n = 5)
top_5_counties_medium <- add_rownames(top_5_counties_medium, var = "County")
top_5_counties_medium$County <- gsub("^MBR_COUNTY", "", top_5_counties_medium$County)

# Same process for high engagement countie 
county_high_engagement <- glm(high_engagement ~ MBR_COUNTY, data = df)

#gives the p-value (significance) and the coefficient of the variable for high engagement model 
summary_data_high <- summary(county_high_engagement)

# Get the coefficients and corresponding p-values
coefficients_high <- summary_data_medium$coefficients[, "Estimate"]
p_values_high <- summary_data_high$coefficients[, "Pr(>|t|)"]

# Combine coefficients and p-values into a data frame
coefficients_df_high <- data.frame(Coefficient = coefficients_high, P_Value = p_values_high)

# Sort the coefficients_df by p-values in ascending order
sorted_coefficients_high <- coefficients_df_high[order(coefficients_df_high$P_Value), ]

# Extract the top 5 counties with the lowest p-values
sorted_coefficients_high <- sorted_coefficients_high[-1, ]
top_5_counties_high <- head(sorted_coefficients_high, n = 5)
top_5_counties_high <- add_rownames(top_5_counties_high, var = "County")
top_5_counties_high$County <- gsub("^MBR_COUNTY", "", top_5_counties_high$County)

#select the top 5 counties from each table and put them in a vector
top_5_counties_high <- as.vector(top_5_counties_high$County)
top_5_counties_medium <- as.vector(top_5_counties_medium$County)
top_5_counties_low <- as.vector(top_5_counties_low$County)

# Create a new binary feature column based on the condition
df$low_engagement_county <- ifelse(df$MBR_COUNTY %in% top_5_counties_low, 1, 0)
df$medium_engagement_county <- ifelse(df$MBR_COUNTY %in% top_5_counties_medium, 1, 0)
df$high_engagement_county <- ifelse(df$MBR_COUNTY %in% top_5_counties_high, 1, 0)


#making all column names uniform before writing csv
library(snakecase)
colnames(df) <- to_snake_case(colnames(df))

#removing the columns used to generate our target variables
df$breast_cancer_scr_elig <- NULL
df$breast_cancer_scr_compliance <- NULL
df$colorectal_cancer_scr_elig <- NULL
df$colorectal_cancer_scr_compliance <- NULL
df$flu_vac_elig <- NULL
df$flu_vac_compliance <- NULL
df$diabetes_care_elig <- NULL
df$diabetes_care_compliance <- NULL
df$readmission_elig <- NULL
df$readmission_flag <- NULL


#saving the cleaned and preprocessed dataframe as csv
write.csv(df, file = "DataForModeling.csv", row.names = FALSE)