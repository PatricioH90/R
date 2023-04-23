#' Author: Alvaro Ronquillo
#' Date: Mar-10 2023
#' Purpose: OKCupid 
#' 

install.packages("modeest")
install.packages(c("tm", "slam", "wordcloud"))

# Libs
library(dplyr)
library(modeest)
library(ggplot2)
library(knitr)
library(wordcloud)
library(tidyr)

# Setting Working Directory
setwd("~/Hult_Visualizing-Analyzing-Data-with-R/PersonalFiles")

# Get the OkCupid data as `profiles`
profiles <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A1_OKCupid/profiles.csv')

# Looking for Missing Values
colSums(is.na(profiles))

missing_count <- data.frame(variable = names(profiles), count = colSums(is.na(profiles)))

# Plotting a Bar Chart for Counting Missing Values per Variables
ggplot(missing_count, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "navy") +
  ggtitle("Number of Missing Values per Variable") +
  xlab("Variables") +
  ylab("Count of Missing Values") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#1. Cleaning "body_type"
# Finding the Mode from the Categorical Variable "body_type"
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

body_type_mode <- Mode(profiles$body_type)
body_type_mode

# Filling NA values in the body type column with "average"
profiles$body_type <- ifelse(is.na(profiles$body_type), "average", profiles$body_type)

#2. Cleaning "diet"
# Cleaning diet column and narrowing down the options
unique(profiles$diet)

profiles$diet <- ifelse(grepl("^\\w+$", profiles$diet), paste("mostly", profiles$diet, sep = " "), profiles$diet)
profiles$diet <- gsub("strictly anything", "mostly anything", profiles$diet)
profiles$diet <- gsub("mostly other", "other", profiles$diet)
profiles$diet <- gsub("strictly other", "other", profiles$diet)
profiles$diet <- ifelse(is.na(profiles$diet), "mostly anything", profiles$diet)

#3. Cleaning "drinks"
# Finding the Mode from the Categorical Variable "drinks"
unique(profiles$drinks)

drinks_mode <- Mode(profiles$drinks)
drinks_mode

# Filling NA values in the drinks column with "socially"
profiles$drinks <- ifelse(is.na(profiles$drinks), "socially", profiles$drinks)

#4. Cleaning "drugs"
# Finding the Mode from the Categorical Variable "drugs"
unique(profiles$drugs)

drugs_mode <- Mode(profiles$drugs)
drugs_mode

# Filling NA values in the drugs column with "never"
profiles$drugs <- ifelse(is.na(profiles$drugs), "never", profiles$drugs)

#5. Cleaning "education"
# Finding the Mode from the Categorical Variable "education"
unique(profiles$education)

education_mode <- Mode(profiles$education)
education_mode

# Filling NA values in the drinks column with "graduated from college/university"
profiles$education <- ifelse(is.na(profiles$education), "graduated from college/university", profiles$education)

#6. Removing "essay0"
# Remove Column "essay0" from the DataFrame
profiles <- subset(profiles, select = -essay0)

#7. Cleaning "ethnicity"
# Finding the Mode from the Categorical Variable "Ethnicity"
unique(profiles$ethnicity)

# Filling NA values in the ethnicity column with "other"
profiles$ethnicity <- ifelse(is.na(profiles$ethnicity), "other", profiles$ethnicity)

#8. Removing "income"
# Remove Column "income" from the DataFrame <- Almost 100% of Missing Values
profiles <- subset(profiles, select = -income)

#9. Cleaning "job"
# Filling NA values in the job column with "Not Specified"
profiles$job <- ifelse(is.na(profiles$job), "Not Specified", profiles$job)

#10. Cleaning "offspring"
# Finding the Mode from the Categorical Variable "Offspring"
unique(profiles$offspring)

# Creating a new column for Offspring
profiles$offspring_new <- ifelse(grepl("has", profiles$offspring, ignore.case = TRUE), "have kids", "don't have kids")

# Remove Column "offspring" from the DataFrame 
profiles <- subset(profiles, select = -offspring)

#11. Cleaning "pets"
# Create new columns for dogs and cats
profiles <- separate(profiles, col = pets, into = c("dogs", "cats"), sep = " and ")

# Find the empty cells in the "cats" column
empty_cats <- which(is.na(profiles$cats))

# Copy the values from dogs to empty cells in cats
profiles$cats[empty_cats] <- profiles$dogs[empty_cats]

# Clean the dog column, remove all cats and replace them with Not Specified
profiles$dogs <- ifelse(profiles$dogs %in% c("dislikes cats", "has cats", "likes cats"), "Not Specified", profiles$dogs)
profiles$cats <- ifelse(profiles$cats %in% c("dislikes dogs", "has dogs", "likes dogs"), "Not Specified", profiles$cats)

# Filling NA values in the cat column with "Not Specified"
profiles$dogs <- ifelse(is.na(profiles$dogs), "Not Specified", profiles$dogs)
profiles$cats <- ifelse(is.na(profiles$cats), "Not Specified", profiles$cats)


#12. Cleaning "religion"
# Create new columns for religion and religion status
profiles$religion_new <- sub("^(\\w+).*", "\\1", profiles$religion)
profiles$religion_status <- sub("^\\w+\\s+(.*)", "\\1", profiles$religion)

# Remove "and" and "but" from the religion_status column
profiles$religion_status <- gsub("\\b(and|but)\\b", "", profiles$religion_status, ignore.case = TRUE)

# Trim leading and trailing whitespace from religion_status column
profiles$religion_status <- trimws(profiles$religion_status)

profiles$religion_status[profiles$religion_new == profiles$religion_status] <- "Not Specific"

# Replace "NA" with "Not Specified"
profiles$religion_new[is.na(profiles$religion_new)] <- "Not Specific"
profiles$religion_status[is.na(profiles$religion_status)] <- "Not Specific"

# Remove Column "religion" from the DataFrame 
profiles <- subset(profiles, select = -religion)

#13. Cleaning "sex"
# Finding the Mode from the Categorical Variable "sex"
unique(profiles$sex)

#14. Cleaning "sign"
# Create new columns for sign and its importance
profiles$sign_new <- sub("^(\\w+).*", "\\1", profiles$sign)
profiles$sign_status <- sub("^\\w+\\s+(.*)", "\\1", profiles$sign)

# Remove "and" and "but" from the sign_status column
profiles$sign_status <- gsub("\\b(and|but)\\b", "", profiles$sign_status, ignore.case = TRUE)

# Trim leading and trailing whitespace from sign_status column
profiles$sign_status <- trimws(profiles$sign_status)

profiles$sign_status[profiles$sign_new == profiles$sign_status] <- "Not Specific"

# Replace "NA" with "Not Specified"
profiles$sign_new[is.na(profiles$sign_new)] <- "Not Specified"
profiles$sign_status[is.na(profiles$sign_status)] <- "Not Specified"


# Remove Column "sign" from the DataFrame 
profiles <- subset(profiles, select = -sign)

#14. Cleaning "smokes"
# Finding the Mode from the Categorical Variable "smokes"
unique(profiles$smokes)

profiles$smokes <- gsub("sometimes", "yes", profiles$smokes)
profiles$smokes <- gsub("when drinking", "yes", profiles$smokes)
profiles$smokes <- gsub("trying to quit", "yes", profiles$smokes)

smokes_mode <- Mode(profiles$smokes)
smokes_mode

# Filling NA values in the smokes column with "no"
profiles$smokes <- ifelse(is.na(profiles$smokes), "no", profiles$smokes)


#14. Cleaning "speaks"
# Filling NA values in the speaks column with "english"
profiles$speaks <- ifelse(is.na(profiles$speaks), "english", profiles$speaks)

#15. Looking for outliers in "Age"
# Create a boxplot for the age column 
ggplot(profiles, aes(x = "Age", y = age)) +
  geom_boxplot() +
  ggtitle("Boxplot of Age") +
  xlab("") +
  ylab("Age") +
  theme(plot.title = element_text(size = 16, face = "bold"))
# Drop rows where age equals 110 or 109
profiles <- profiles[!(profiles$age %in% c(109, 110)), ]

#16. Looking for outliers in "Height"
# Create a boxplot for the heigh column 
ggplot(profiles, aes(x = "Height", y = height)) +
  geom_boxplot() +
  ggtitle("Boxplot of Height") +
  xlab("") +
  ylab("Height") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Drop rows where height less than 50
profiles <- profiles[profiles$height >= 50, ]

profiles <- profiles[!is.na(profiles$height), ]

#17. Creating new columns for City and State
profiles <- separate(profiles, location, into = c("city", "state"), sep = ", ")

# Trim leading and trailing whitespace from religion_status column
profiles$state <- trimws(profiles$state)


# Final Check for Missing Values
missing_count <- data.frame(variable = names(profiles), count = colSums(is.na(profiles)))

ggplot(missing_count, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "navy") +
  ggtitle("Number of Missing Values per Variable") +
  xlab("Variables") +
  ylab("Count of Missing Values") +
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Replacing the Values of Gender
profiles$sex <- profiles$sex %>% gsub("m", "male", .) %>% gsub("f", "female", .)



# Graphs for Insights


# Counts of Profiles by Gender
ggplot(data = profiles, aes(x = sex, fill = sex)) + 
  geom_bar(position = "dodge", stat = "count")  +
  ggtitle("Counts by Gender") +
  scale_fill_manual(values = c("darkgreen", "navy")) + 
  ylab("Count") + 
  xlab("Gender")

# Distribution of Age per Status and Gender
ggplot(data = profiles, aes(x = age)) +
  geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
  xlab("Age") + ylab("Count") +
  ggtitle("Age Distribution by Gender and Status") +
  facet_grid(sex ~ status, scales = "free_y") +
  theme_bw()

# Distribution of Status
ggplot(data = profiles, aes(x = status)) + 
  geom_bar() + 
  labs(title = "Distribution of Users per Status", x = "status", y = "Count") +
  theme(legend.position = "right")

# Age info per Gender
age_summary <- aggregate(age ~ sex, data = profiles, FUN = function(x) {
  c(mean = mean(x), median = median(x), mode = mlv(x))
})
age_summary

# To see the Distribution of Male and Female that are Single
# Create separate dataframes for male and female ages
male_age <- profiles %>% filter(sex == "male" & status == "single") %>% group_by(age) %>% summarize(count = n())
female_age <- profiles %>% filter(sex == "female" & status == "single") %>% group_by(age) %>% summarize(count = n())


# Graph Distribution by Gender
ggplot() +
  geom_bar(data = profiles, aes(x = age), fill = "lightblue", alpha = 0.5) +
  geom_line(data = male_age, aes(x = age, y = count), color = "blue", size = 1.5) +
  geom_line(data = female_age, aes(x = age, y = count), color = "pink", size = 1.5) +
  scale_x_continuous(name = "Age", limits = c(18, 80)) +
  scale_y_continuous(name = "Count", limits = c(0, 3800)) +
  labs(title = "Age Distribution") +
  theme_bw()


# New column Cluster Ages
profiles$age_group <- cut(profiles$age,
                          breaks = c(17, 25, 35, 45, 55, max(profiles$age)),
                          labels = c("18-25", "26-35", "36-45", "46-55", "56+"))


# Age Group by gender
ggplot(data = profiles, aes(x = sex, fill = age_group)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Age Group by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

table(profiles$sex, profiles$age_group)

# Body Type by gender
ggplot(data = profiles, aes(x = sex, fill = body_type)) + 
  geom_bar() + 
  labs(title = "Body Type by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Diet by gender
ggplot(data = profiles, aes(x = sex, fill = diet)) + 
  geom_bar() + 
  labs(title = "Diet by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Religion by Genre
ggplot(data = subset(profiles, religion_new != "Not Specific" & religion_new != "other"), aes(x = sex, fill = religion_new)) + 
  geom_bar() + 
  labs(title = "Religion by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")


# Religion by Age Group
ggplot(data = subset(profiles, religion_new != "Not Specific" & religion_new != "other"), aes(x = age_group, fill = religion_new)) + 
  geom_bar() + 
  labs(title = "Religion by Age Group", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Drinking Habits by Age Group
ggplot(data = profiles, aes(x = age_group, fill = drinks)) + 
  geom_bar() + 
  labs(title = "Drinking Habits by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ sex)

# Drinking Habits by Religion
ggplot(data = profiles, aes(x = religion_new, fill = drinks)) + 
  geom_bar() + 
  labs(title = "Drinking Habits by Religion", x = "Religion", y = "Count") +
  theme(legend.position = "right")

# Drugs Usage by Age Group
ggplot(data = profiles, aes(x = age_group, fill = drugs)) + 
  geom_bar() + 
  labs(title = "Drugs Usage by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ sex)

# Drugs Usage by Religion
ggplot(data = profiles, aes(x = religion_new, fill = drugs)) + 
  geom_bar() + 
  labs(title = "Drugs Usage by Religion", x = "Religion", y = "Count") +
  theme(legend.position = "right")

# Orientation by gender
ggplot(data = profiles, aes(x = sex, fill = orientation)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Orientation by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Pets by gender (Dogs)
ggplot(data = profiles, aes(x = sex, fill = dogs)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Feeling about Dogs by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Pets by Age Group (Dogs)
ggplot(data = profiles, aes(x = age_group, fill = cats)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Feeling about Dogs by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right")

# Pets by Orientation (Dogs)
ggplot(data = profiles, aes(x = orientation, fill = dogs)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Feeling about Dogs by Orientation", x = "Orientation", y = "Count") +
  theme(legend.position = "right")

# Pets by gender (Cats)
ggplot(data = profiles, aes(x = sex, fill = cats)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Feeling about Cats by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Pets by Orientation (Cats)
ggplot(data = profiles, aes(x = orientation, fill = cats)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Feeling about Cats by Orientation", x = "Orientation", y = "Count") +
  theme(legend.position = "right")


# Smoking Habits by Gender
ggplot(data = profiles, aes(x = sex, fill = smokes)) + 
  geom_bar() + 
  labs(title = "Smokes by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Smoking Habits by Age Group
ggplot(data = profiles, aes(x = age_group, fill = smokes)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smokes by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right")

# Smoking Habits by Age Group for Both Genres
ggplot(data = profiles, aes(x = age_group, fill = smokes)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smokes by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ sex)

# Smoking Habits relationship with Drinks
ggplot(data = profiles, aes(x = drinks, fill = smokes)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smokes by Drinking Habits", x = "Drinking Habits", y = "Count") +
  theme(legend.position = "right")

# Smoking Habits relationship with Drugs
ggplot(data = profiles, aes(x = drugs, fill = smokes)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smokes by Drugs Usage", x = "Drugs Usage", y = "Count") +
  theme(legend.position = "right")

# Smoking Habits relationship with Drugs Usage Often
ggplot(data = subset(profiles, drugs == "often"), aes(x = drugs, fill = smokes)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Smokes by Drugs Usage (Often)", x = "Drugs Usage", y = "Count") +
  theme(legend.position = "right")


# Offpsring by Gender
ggplot(data = profiles, aes(x = sex, fill = offspring_new)) + 
  geom_bar() + 
  labs(title = "Offspring Preference by Gender", x = "Gender", y = "Count") +
  theme(legend.position = "right")

# Offpsring by Orientation
ggplot(data = profiles, aes(x = orientation, fill = offspring_new)) + 
  geom_bar() + 
  labs(title = "Offspring Preference by Orientation", x = "Orientation", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ sex)

# Offpsring by Orientation
ggplot(data = profiles, aes(x = orientation, fill = offspring_new)) + 
  geom_bar() + 
  labs(title = "Offspring Preference by Orientation", x = "Orientation", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ status)

# Offpsring by Age Group
ggplot(data = profiles, aes(x = age_group, fill = offspring_new)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Offspring Preference by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right")

# Offpsring Have Kids by Age Group
ggplot(data = subset(profiles, offspring_new == "have kids"), aes(x = age_group, fill = offspring_new)) + 
  geom_bar() + 
  labs(title = "Offspring Preference by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right")

# Offpsring Have Kids by Age Group and Genre
ggplot(data = subset(profiles, offspring_new == "have kids"), aes(x = age_group, fill = offspring_new)) + 
  geom_bar() + 
  labs(title = "Offspring Preference by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right") + 
  facet_wrap(~ sex)

# Offpsring by Age Group for Both Genres
ggplot(data = profiles, aes(x = age_group, fill = offspring_new)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Offspring Preference by Age Group", x = "Age Group", y = "Count") +
  theme(legend.position = "right") +
  facet_wrap(~ sex)

# Offpsring by religion
ggplot(data = profiles, aes(x = religion_new, fill = offspring_new)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Offspring Preference by Religion", x = "Religion", y = "Count") +
  theme(legend.position = "right")


# Jobs
# Filter Data
male_profiles <- subset(profiles, sex == "male" & job != "Not Specified")
female_profiles <- subset(profiles, sex == "female" & job != "Not Specified")

# Generate Word Clouds
par(mfrow=c(1,2)) # Set plot layout to display two Word Clouds side by side
wordcloud(male_profiles$job, scale=c(2,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"), main="Male Profiles")
wordcloud(female_profiles$job, scale=c(2,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"), main="Female Profiles")

# Education
par(mfrow=c(1,1))
wordcloud(profiles$education, scale=c(7,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#Languages
wordcloud(profiles$speaks, scale=c(7,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#Location
wordcloud(profiles$state, scale=c(5,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(profiles$city, scale=c(5,0.5), min.freq=5, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

