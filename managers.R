# Enter data into vectors before constructing the data frame
date_col <- c("2018-15-10", "2018-01-11", "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99) # 99 is one of the values in the age attribute - will require recoding
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")

managers <- data.frame(date_col, 
                       country_col, 
                       gender_col, 
                       age_col, 
                       q1_col, 
                       q2_col, 
                       q3_col,
                       q4_col,
                       q5_col)

colnames(managers) <- column_names

str(managers)

managers$Age[managers$Age == 99] <- NA

managers

managers$Age_cat[managers$Age >= 45] <- "Elder"
managers$Age_cat[managers$Age < 45 & managers$Age >= 26] <- "Middle age"
managers$Age_cat[managers$Age < 26] <- "Young"
managers$Age_cat[is.na(managers$Age)] <- "Elder"

managers
str(managers)

age_fact <- factor(managers$Age_cat, ordered = TRUE, levels = c("Young", "Middle age", "Elder") )

managers$Age_cat <- age_fact

str(managers)
managers
#Sum of columns
managers$summary_col <- managers$Q1 + managers$Q2 + managers$Q3 + managers$Q3 + managers$Q4 + managers$Q5

# Another method is 
#managers <- data.frame(managers,summary_col)
managers

str(managers)

# Get subset of data
sub_managers <- subset(managers,Age  >=35 | Age  < 25,select=c(Q1,Q2,Q3,Q4))
sub_managers

# Mean columns
managers$mean_col <- rowMeans(managers[5:9])
managers


#Changing column name in a data frame
names(managers)[11] <- "Summary"
names(managers)[12] <- "Average"

# Converting character date to Date type
managers$Date <- as.Date(as.character(managers$Date),"%Y-%d-%m")
managers
str(managers)
managers

complete_data <- complete.cases(managers)
complete_data
sum(complete_data)

#List rows that has missing data
complete_data <- managers[!complete.cases(managers),]
complete_data

#Find missing values
sum(is.na(managers$Age))

install.packages("mice")
library("mice")
md.pattern(managers)

install.packages("VIM")
library(VIM)
missing_values <- aggr(managers,
                       prop = FALSE,
                       numbers = TRUE)
