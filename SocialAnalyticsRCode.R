####### Imports ######
#install.packages("data.table")           
library("data.table")
library(tibble)
library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)
library(tidyr)

####### Basic Data Exploration and Preparation ####### 
social_data <- read.csv("cdf_train.csv")
View(social_data)

#Get class types of columns 
class_type <- as.data.frame(sapply(social_data, class))

#Make copy of dataframe
social_data_copy <- data.frame(social_data)

missing_values <- as.data.frame(colSums(is.na(social_data)))
missing_values$missing_freq <- colSums(is.na(social_data))/nrow(social_data)*100

###### Cleaning dataset ####### 

#Total.Interactions = Likes + Comments + Shares + Love + Wow + Haha + Sad + Angry + Care
#Use to fill missing values in total.interactions

#First need to drop rows with missing values in the RHS columns
#Use setDT to convert dataframe to data.table
social_data <- na.omit(setDT(social_data), 
                            cols=c("Likes","Comments","Shares","Love","Wow","Haha","Sad","Angry","Care"))

#Class_type shows "Haha" is of class 'character', cast to int
social_data$Haha <- as.integer(social_data$Haha)

#rowSums
social_data$Total.Interactions <- rowSums(social_data[,c("Likes","Comments","Shares","Love","Wow","Haha","Sad","Angry","Care")])

### Create engagement column: Engagement = Likes + Shares + Comments
social_data <- add_column(social_data, Engagement = c(rep(0, nrow(social_data))), .after = 13)
social_data$Engagement <- rowSums(social_data[,c("Likes","Comments","Shares")])

### Check missing values again
missing_values <- as.data.frame(colSums(is.na(social_data)))
missing_values$missing_freq <- colSums(is.na(social_data))/nrow(social_data)*100

#Followers.at.Posting : 30% missing
#Video.Share.Status : 60% missing
#Video.Length: 99% missing
#Final.Link: 73% missing
#Image.Text: 97% missing
#Link.Text: 17% missing
#Description: 52% missing
#Sponsor.Id: 41% missing
#Sponsor.Name: 42% missing
#Sponsor.Category: 42% missing

#Drop Followers.at.Posting, Video.Length, Final.Link, Image.Text, Link.Text, Description
#Either too many missing or too text heavy: NLP outside scope of project
social_data <- social_data[,-c("Followers.at.Posting","Video.Length","Final.Link","Image.Text",
                               "Image.Text","Link.Text","Description")]

#Value counts for Video.Share.Status
#In addition, visual inspection shows Is.Video.Owner = "-" when Video.Share.Status = NA
table(social_data$Video.Share.Status, useNA = "always")
table(social_data$Is.Video.Owner., useNA = "always")
#Can't seem to find 1:1 relation anywhere, drop both columns
social_data <- social_data[,-c("Video.Share.Status","Is.Video.Owner.")]

###Handle Sponsor columns: try to find relationship to avoid dropping columns
#SponsorID is NA when no sponsor: change NA to 0
social_data$Sponsor.Id[is.na(social_data$Sponsor.Id)] <- 0
#Create dummy hasSponsor for future interaction effect
social_data <- add_column(social_data, hasSponsor = c(rep(0, nrow(social_data))), .after = 28)
social_data$hasSponsor[social_data$Sponsor.Id != 0] <- 1

#Sponsor.Name & Sponsor.Category = 0 when Sponsor.Id = 0
social_data$Sponsor.Name[social_data$Sponsor.Id == 0] <- 0
social_data$Sponsor.Category[social_data$Sponsor.Id == 0] <- 0

### Check missing values again
missing_values <- as.data.frame(colSums(is.na(social_data)))
missing_values$missing_freq <- colSums(is.na(social_data))/nrow(social_data)*100

#Drop rows with missing values
social_data <- na.omit(social_data)




#Cast dataset back to dataframe 
social_data <- data.frame(social_data)
#Make 2nd copy of dataset
social_data_copy_v2 <- data.frame(social_data)

#Get class types of columns 
class_type <- as.data.frame(sapply(social_data, class))

#Make sure class type makes sense
#Post.Views, Total.Views, Likes.at.Posting are characters when should be int: convert
social_data$Post.Views <- as.integer(social_data$Post.Views)
social_data$Total.Views <- as.integer(social_data$Total.Views)
social_data$Likes.at.Posting <- as.integer(social_data$Likes.at.Posting)

#WC, Dash is character when should be numeric: convert
social_data$WC <- as.numeric(social_data$WC)
social_data$Dash <- as.numeric(social_data$Dash)

#3 NAs introduced by coercion when casting: drop them
social_data <- na.omit(social_data)


#Get dataset with only important variables
nrow(table(social_data$Page.Name)) #Cardinality: 256, keep column
nrow(table(social_data$Page.Category)) #Cardinality: 21, keep column
nrow(table(social_data$User.Name)) #Cardinality: 258, keep column
nrow(table(social_data$Page.Admin.Top.Country)) #Cardinality: 1, drop column
nrow(table(social_data$Type)) #Cardinality: 9, keep column
nrow(table(social_data$Sponsor.Name)) #Cardinality: 5398, keep column
nrow(table(social_data$train_test)) #Cardinality: 1, drop column

#Keep Page.Created, Post.Created and Post.Created.Time: can perhaps make a feature out of these two columns

drop_cols <- c("Facebook.Id","Page.Admin.Top.Country","Page.Description","URL",
               "Message","Link","Sponsor.Id","train_test")
social_data <- social_data[,!(names(social_data) %in% drop_cols)]


#Get class types of columns 
class_type <- as.data.frame(sapply(social_data, class))

###Convert character variables to factor
social_data = social_data %>% 
  mutate_at(vars(Page.Name,User.Name,Page.Category,Type,Sponsor.Name,Sponsor.Category),
            as.factor)

##### Get Page.Created into two columns: Page.Created.Date and Page.Created.Time
social_data = social_data %>%
  separate(Page.Created, c("Page.Created.Date", "Page.Created.Time"), " ", remove=FALSE)

### Separate main dataframe column index into vectors 
colnames(social_data)
#CrowdTangle: variables 1 through 39
crowdtangle_vars <- c(1:28)
#LIWC2015: variables 40 through 122
liwc_vars <- c(29:121)

#Create dataframes for crowdtangle data and liwc data
crowdtangle_data <- social_data[,crowdtangle_vars]
liwc_data <- social_data[,liwc_vars]

#Numeric variables for main dataframe
num_vars <- unlist(lapply(social_data, is.numeric))  
#Factor variables for main dataframe
factor_vars <- unlist(lapply(social_data, is.factor))

#Numeric variables for crowdtangle dataframe
crowdtangle_num_vars <- unlist(lapply(crowdtangle_data, is.numeric))  
#Factor variables for crowdtangle dataframe
crowdtangle_factor_vars <- unlist(lapply(crowdtangle_data, is.factor))  

#Numeric variables for liwc dataframe
liwc_num_vars <- unlist(lapply(liwc_data, is.numeric))  
liwc_factor_vars <- unlist(lapply(liwc_data, is.factor))  


## Save the dataset as .csv
write.csv(social_data, "cleaned_social_data.csv")


###### Data Visualization ########

#### Histograms #### 
#Function to create a list of histograms
histplot = function (data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(fill = "blue") +
    xlab(column) 
}

#List of histograms for CrowdTangle numeric variables
crowdtangle_numhist <- lapply(colnames(crowdtangle_data[,crowdtangle_num_vars]), histplot, data = crowdtangle_data[,crowdtangle_num_vars])
names(crowdtangle_numhist) <- colnames(crowdtangle_data[,crowdtangle_num_vars])
#Some SO magic to get a grid arranged
n <- length(crowdtangle_numhist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(crowdtangle_numhist, ncol=nCol))

#Histograms aren't very useful: are all 1 bars. Use summary to check min/max
summary(crowdtangle_data[,crowdtangle_num_vars])

#List of histograms for log of CrowdTangle numeric variables
crowdtangle_numhist.log <- lapply(colnames(log(crowdtangle_data[,crowdtangle_num_vars])), histplot, data = log(crowdtangle_data[,crowdtangle_num_vars]))
names(crowdtangle_numhist.log) <- colnames(log(crowdtangle_data[,crowdtangle_num_vars]))
#Some SO magic to get a grid arranged
n <- length(crowdtangle_numhist.log)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(crowdtangle_numhist.log, ncol=nCol))


#List of histograms for LIWC numeric variables
liwc_numhist <- lapply(colnames(liwc_data[,liwc_num_vars]), histplot, data = liwc_data[,liwc_num_vars])
names(liwc_numhist) <- colnames(liwc_data[,liwc_num_vars])
#Some SO magic to get a grid arranged
n <- length(liwc_numhist)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(liwc_numhist, ncol=nCol))

