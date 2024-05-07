# This file offers a bar chart of engagement for each age group
# The file is not required to run, but it is good to visualize
# a baseline of the distribution before modeling.

rm(list=ls())

#read in the data used for the regression problem
df <- read.csv("DataForModeling.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

#making a variable to indicate a member having an engagement percentage of 0
df$no_engagement <- ifelse(df$engagement_percentage == 0, 1, 0)

# finding and average without 0 engagement percentage members 
filtered_data <- df[df$engagement_percentage != 0, ]

average_without_zeros <- mean(filtered_data$engagement_percentage)

#make a column to indicate if a member has an engagement percentage greater than 0 and less the average without 0s
df$medium_engagment <- ifelse(df$engagement_percentage > 0 & df$engagement_percentage <= average_without_zeros, 1, 0)  


# creating column for high engagement
df$high_engagement <- ifelse(df$engagement_percentage > average_without_zeros, 1, 0)


#removing engagement percentage to make it a full classification problem
df$engagement_percentage <- NULL


#seeing the distribution and summary statistics of the age column 
distribution <- table(df$age)

print(distribution)

summary(df$age)

min(df$age)

df$age_Buckets <- ifelse(df$age < 65, 'Young', ifelse(df$age >= 65 & df$age <= 77, 'Average', 'Old'))

count_young <- sum(df$age_Buckets == 'Young')

count_average <- sum(df$age_Buckets == 'Average')

count_old <- sum(df$age_Buckets == 'Old')

write.csv(df, file = "clusteringdata.csv", row.names = FALSE)



library(ggplot2)


# Create a dataframe for plotting
age_counts <- data.frame(
  Age_Group = c('Young', 'Average', 'Old'),
  Frequency = c(count_young, count_average, count_old)
)

# Create a bar plot
ggplot(age_counts, aes(x = Age_Group, y = Frequency, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(x = "Age Group", y = "Frequency", title = "Frequency of Age Groups") +
  theme_minimal()


# what ages are most common along members with no medium and high engagement? 

# count of old for each level of engagement 

count_no_old <- sum(df$age_Buckets == 'Old' & df$no_engagement == 1)
Count_med_old <- sum(df$age_Buckets == 'Old' & df$medium_engagment == 1)
count_high_old <- sum(df$age_Buckets == 'Old' & df$high_engagement == 1)


# count of average for each level of engagement 

count_no_avg <- sum(df$age_Buckets == 'Average' & df$no_engagement == 1)
Count_med_avg <- sum(df$age_Buckets == 'Average' & df$medium_engagment == 1)
count_high_avg <- sum(df$age_Buckets == 'Average' & df$high_engagement == 1)

# count of young for each level of engagement 

count_no_young <- sum(df$age_Buckets == 'Young' & df$no_engagement == 1)
Count_med_young <- sum(df$age_Buckets == 'Young' & df$medium_engagment == 1)
count_high_young <- sum(df$age_Buckets == 'Young' & df$high_engagement == 1)


counts <- data.frame(
  Engagement_Level = rep(c("No", "Medium", "High"), each = 3),
  Age_Bucket = rep(c("Old", "Average", "Young"), times = 3),
  Count = c(count_no_old, count_no_avg, count_no_young,
            Count_med_old, Count_med_avg, Count_med_young,
            count_high_old, count_high_avg, count_high_young)
)


library(ggplot2)

ggplot(counts, aes(x = Age_Bucket, y = Count, fill = Engagement_Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Counts of Engagement Levels by Age Bucket",
       x = "Age Bucket",
       y = "Count") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF")) + # Customizing colors
  theme_minimal()


