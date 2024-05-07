# Business-Analytics-Capstone-project
I will not add dataset because of private information.
## AArete Consulting Enhancing Member Engagement
AArete Consulting has provided The University of Iowa's Business Analytics Capstone Section 5 Team 1 with data regarding a segment of Medicare members to gain a better understanding of what influences member engagement. AArete currently assists Medicare plans across the country to increase engagement and, in turn, increase the star rating of those plans. The Enhancing Member Engagement project purpose is to assit AArete Consulting with building a model to predict Medicare member engagement. 

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
1. Download Deliverables.zip and extract to desktop.
2. Set working directory to extracted folder\Data
3. Download RStudio and all packages listed in the prerequisites below
4. Download Orange for Data Mining 

## Prerequisites
What things you need to run the software
1. A SQL server with access to Medicare member data (ex: Snowflake)
2. R 4.3.1
3. RStudio 2023.06.1+524
4. Packages to install: ggplot2, dplyr, lubridate, snakecase
5. Orange Data Mining Software

## Break down into end to end tests
1. Script 1. InitialPull.txt utilizes the SQL database to join and download neccessary tables and produces outputs "MemberAndMeasureResults.csv", "Claims.csv", and "ClaimsOneLine.csv"
2. Script 2. ExploratoryDataAnalysis.txt utilizes the SQL database to pull various descriptive insights of the dataset
3. Script 3. CleaningPreprocessing.R uses inputs "MemberAndMeasureResults.csv", "Claims.csv", "claims_line_1.csv", "InpatientCount.csv", "OutpatientCount.csv", and "MedicalCount.csv" and produces output "DataForModeling.csv"
4. Script 4. EngagementCutoff.R uses inputs "DataForModeling.csv" and produces the output of a bar chart that displays the number of members for each level of engagement for each age group. This file is not required for modeling but is an interesting visualization to investigate before moving forward with modeling.
5. Script 5. MemberProfiles.R uses inputs "DataForModeling.csv" and produces the output of various descriptive statistics that can be used to compare each of the engagement classes, allowing the user to identify the typical member for each class.
6. Script 6. AArete_No.ows uses inputs "DataForModeling.csv" and produces the output of "NoResults.xlsx"
7. Script 7. AArete_Med.ows uses input "DataForModeling.csv" and produces the output of "MedResults.xlsx"
8. Script 8. AArete_high.ows uses input "DataForModeling.csv" and produces the output of "HighResults.xlsx"

## Deployment
This project is not built for deployment.

## Versioning
Version 1.0

## Authors
Grant Allen
Alexis Pomernackas
Aleksandra Zeglen
Hien Tran
Ryan Koshal

## Acknowledgments
Acknowledgements to Professor Michael Altemeier within The University of Iowa for assistance in the completion of this analysis. Additional acknowledgements to the AArete Consulting project team Sarah Altemeier, Justin Harris, Alex Behm for providing the data and overseeing the execution of the analysis.
