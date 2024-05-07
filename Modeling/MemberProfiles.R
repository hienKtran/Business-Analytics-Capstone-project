# This file allows the user to investigate how members of the no, medium, and high
# engagement classes differ. The file will output statistics such as average age,
# sex ratio, flags, average claims, sdoh, and mental health calculations.

rm(list=ls())
data <- read.csv("DataForModeling.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", ""), sep = ",")

# Ensure that all binary variables are factor
str(data)
# If they are not, run the code block below
is_binary <- function(x) {
  x0 <- na.omit(x)
  is.numeric(x) && length(unique(x0)) %in% 1:2 && all(x0 %in% 0:1)
}
ok <- sapply(data, is_binary)
data <- replace(data, ok, lapply(data[ok], factor, levels = 0:1))

# First, generate baseline statistics
avg_age <- mean(data$age)
hist(data$age, 
     main = "Distribution of Age", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "skyblue", 
     border = "black")
percentage_male <- sum(data$sex == "1")/nrow(data)

# This will create a table that displays the raw amount and ratios for each condition in the data
flag_data <- data[, c("anxiety_flag", "asthma_flag", "depression_flag", "diabetes_flag")]
flag_counts <- apply(flag_data, 2, table)
ratios <- flag_counts[2, ] / flag_counts[1, ]
flag_counts <- rbind(flag_counts, ratios)

average_claims <- mean(data$num_claims)
percent_sdoh_flagged <- sum(data$sdoh_flag == 1)/nrow(data)
percent_mental_health <- sum(data$depression_flag == 1 | data$anxiety_flag == 1)/nrow(data)

# Next pull the same insights for no engagement members
no_engagement_data <- data[data$no_engagement == 1, ]
Lavg_age <- mean(no_engagement_data$age)
hist(no_engagement_data$age, 
     main = "Distribution of Age", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "skyblue", 
     border = "black")
Lpercentage_male <- sum(no_engagement_data$sex == "1")/nrow(no_engagement_data)
Lflag_data <- no_engagement_data[, c("anxiety_flag", "asthma_flag", "depression_flag", "diabetes_flag")]
Lflag_counts <- apply(Lflag_data, 2, table)
Lratios <- Lflag_counts[2, ] / Lflag_counts[1, ]
Lflag_counts <- rbind(Lflag_counts, Lratios)
Laverage_claims <- mean(no_engagement_data$num_claims)
hist(no_engagement_data$num_claims, 
     main = "Distribution of Number of Claims", 
     xlab = "Number of Claims", 
     ylab = "Frequency",
     col = "orange", 
     border = "black")
Lnum_sdoh_flagged <- sum(no_engagement_data$sdoh_flag == 1)/nrow(no_engagement_data)

# Do we see more SDOH flags in low engagement counties?
Lcounties <- no_engagement_data <- no_engagement_data[no_engagement_data$low_engagement_county == 1, ]
LCperc_sdoh_flagged <- sum(Lcounties$sdoh_flag == 1)/nrow(Lcounties)

# Then pull same insights for medium engaged members
med_engagement_data <- data[data$medium_engagement == 1, ]
Mavg_age <- mean(med_engagement_data$age)
hist(med_engagement_data$age, 
     main = "Distribution of Age", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "skyblue", 
     border = "black")
Mpercentage_male <- sum(med_engagement_data$sex == "1")/nrow(med_engagement_data)
Mflag_data <- med_engagement_data[, c("anxiety_flag", "asthma_flag", "depression_flag", "diabetes_flag")]
Mflag_counts <- apply(Mflag_data, 2, table)
Mratios <- Mflag_counts[2, ] / Mflag_counts[1, ]
Mflag_counts <- rbind(Mflag_counts, Mratios)
Maverage_claims <- mean(med_engagement_data$num_claims)
hist(med_engagement_data$num_claims, 
     main = "Distribution of Number of Claims", 
     xlab = "Number of Claims", 
     ylab = "Frequency",
     col = "orange", 
     border = "black")
Mnum_sdoh_flagged <- sum(med_engagement_data$sdoh_flag == 1)/nrow(med_engagement_data)
Mmental_health <- sum(med_engagement_data$depression_flag == 1 | med_engagement_data$anxiety_flag == 1)/nrow(med_engagement_data)




# Then pull same insights for high engaged members
high_engagement_data <- data[data$high_engagement == 1, ]
Havg_age <- mean(high_engagement_data$age)
hist(high_engagement_data$age, 
     main = "Distribution of Age", 
     xlab = "Age", 
     ylab = "Frequency",
     col = "skyblue", 
     border = "black")
Hpercentage_male <- sum(high_engagement_data$sex == "1")/nrow(high_engagement_data)
Hflag_data <- high_engagement_data[, c("anxiety_flag", "asthma_flag", "depression_flag", "diabetes_flag")]
Hflag_counts <- apply(Hflag_data, 2, table)
Hratios <- Hflag_counts[2, ] / Hflag_counts[1, ]
Hflag_counts <- rbind(Hflag_counts, Hratios)
Haverage_claims <- mean(high_engagement_data$num_claims)
hist(high_engagement_data$num_claims, 
     main = "Distribution of Number of Claims", 
     xlab = "Number of Claims", 
     ylab = "Frequency",
     col = "orange", 
     border = "black")
Hnum_sdoh_flagged <- sum(high_engagement_data$sdoh_flag == 1)/nrow(high_engagement_data)
Hcondition <- sum(high_engagement_data$depression_flag == 1 | high_engagement_data$anxiety_flag == 1 | high_engagement_data$diabetes_flag == 1 | high_engagement_data$asthma_flag == 1)/nrow(high_engagement_data)






#Testing age and engagement correlation
library(ggplot2)
ggplot(data, aes(x = age, y = engagement_percentage)) +
  geom_point() +
  labs(x = "Age", y = "Engagement Percentage") +
  ggtitle("Relationship between Age and Engagement Percentage")
correlation <- cor(data$age, data$engagement_percentage)


#How many members are from oregon?
oregon_data <- data[data$mbr_state_cd == "OR", ]
OR_percent <- 7055/7089




