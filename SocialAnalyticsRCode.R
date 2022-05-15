####### Imports ######
#install.packages("glmnet")           
library("data.table")
library(tibble)
library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)
library(tidyr)
library(mlbench)
library(caret)
library(cattonum)
library(chron)
library(h2o)
library(glmnet)  
library(psych)
library(Hmisc)
library(stargazer)

####### Basic Data Exploration and Preparation ####### 
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

#Sponsor.Name & Sponsor.Category = 'No Sponsor when Sponsor.Id = 0
social_data$Sponsor.Name[social_data$Sponsor.Id == 0] <- 'No Sponsor'
social_data$Sponsor.Category[social_data$Sponsor.Id == 0] <- 'No Sponsor'

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



###### Feature Engineering ######

###### Data Visualization ########

#Create factor variable with day of the week for post created 
social_data$Post.Created.Day <- weekdays(as.Date(social_data$Post.Created.Date))
#Create binary column for weekend: 1 if 'Saturday' or 'Sunday', 0 else
social_data$Post.Created.Weekend <- ifelse((social_data$Post.Created.Day == 'Saturday' |
                                              social_data$Post.Created.Day == 'Sunday' ), 1, 0)

# Create dummy variables based on time intervals

#First cast Post.Created.Time to chron time 
social_data$Post.Created.Time <- chron(times = social_data$Post.Created.Time)

#Then: define bins and dummies for those bins
#Bin 1: from 22:00:01 to 06:00:00 -> Post.Created.TimeNight
social_data$Post.Created.TimeNight <- ifelse((social_data$Post.Created.Time >= '22:00:01' |
                                                social_data$Post.Created.Time <= '06:00:00'), 1, 0)

#Bin 2: from 06:00:01 to 10:00:00 -> Post.Created.TimeMorning
social_data$Post.Created.TimeMorning <- ifelse((social_data$Post.Created.Time >= '06:00:01' &
                                                  social_data$Post.Created.Time <= '10:00:00'), 1, 0)

#Bin 3: from 10:00:01 to 14:00:00 -> Post.Created.TimeMidday
social_data$Post.Created.TimeMidday <- ifelse((social_data$Post.Created.Time >= '10:00:01' &
                                                 social_data$Post.Created.Time <= '14:00:00'), 1, 0)

#Bin 4: from 14:00:01 to 18:00:00 -> Post.Created.TimeAfternoon
social_data$Post.Created.TimeAfternoon <- ifelse((social_data$Post.Created.Time >= '14:00:01' &
                                                    social_data$Post.Created.Time <= '18:00:00'), 1, 0)

#Bin 5: from 18:00:01 to 22:00:00 -> Post.Created.TimeEvening
social_data$Post.Created.TimeEvening <- ifelse((social_data$Post.Created.Time >= '18:00:01' &
                                                  social_data$Post.Created.Time <= '22:00:00'), 1, 0)


#Encode high cardinality features using mean encoding

nrow(table(social_data$Page.Name)) #Cardinality: 256, encode
nrow(table(social_data$Page.Category)) #Cardinality: 21, encode
nrow(table(social_data$User.Name)) #Cardinality: 258, encode
nrow(table(social_data$Type)) #Cardinality: 9, keep as Factor variable
nrow(table(social_data$Sponsor.Name)) #Cardinality: 5398, encode
nrow(table(social_data$Sponsor.Category)) #Cardinality: 682, encode

#Get idea of distribution for those features

sort(table(social_data$Page.Name)) 
table(table(social_data$Page.Name) <= 15)
#Rename pages with less than 15 rows to 'Other': 75 pages, 516 rows
levels(social_data$Page.Name)[table(social_data$Page.Name) <= 15] <- 'Other'

sort(table(social_data$Page.Category)) 
table(table(social_data$Page.Category) <= 10)
#Rename categories with less than 10 rows to 'Other': 4 categories, 12 rows
levels(social_data$Page.Category)[table(social_data$Page.Category) <= 10] <- 'Other'

sort(table(social_data$User.Name)) 
table(table(social_data$User.Name) <= 9)
#Rename users with less than 9 rows to 'Other': 52 users, 210 rows
levels(social_data$User.Name)[table(social_data$User.Name) <= 9] <- 'Other'

SponsorNameFreq <- sort(table(social_data$Sponsor.Name), decreasing = TRUE) 
table(table(social_data$Sponsor.Name) <= 3)
#Rename sponsors with less than 3 rows to 'Other': 1877 sponsors, 3797 rows
levels(social_data$Sponsor.Name)[table(social_data$Sponsor.Name) <= 3] <- 'Other'

sort(table(social_data$Sponsor.Category)) 
table(table(social_data$Sponsor.Category) <= 5)
#Rename categories with less than 5 rows to 'Other': 134 categories, 386 rows
levels(social_data$Sponsor.Category)[table(social_data$Sponsor.Category) <= 5] <- 'Other'

#Reposition columns to keep definite split between CrowdTangle and LIWC data
colnames(social_data)
social_data <- social_data[,c(1:28, 122:128, 29:121)]


#H2o does not recognize times format from chron: cast back to string
social_data$Post.Created.Time <- as.character(social_data$Post.Created.Time )

#Save dataframe as csv
#write.csv(social_data, "cleaned_data_noencoding.csv")

### Split dataset into train and test in order to perform leave-one-out encoding
#First cast dataframe to h2o frame
h2o.init()
h2o.social_data <- as.h2o(social_data)

#Define seed
seed <- 123

#Split into train and test
splits <- h2o.splitFrame(h2o.social_data, seed = seed, ratios = c(0.75))

train_data <- splits[[1]]
test_data <- splits[[2]]

### Leave one out encoding for categorical features with high cardinality
#Specify columns to encode
cat_cols <- c('Page.Name','Page.Category','User.Name','Sponsor.Name','Sponsor.Category')

target_encoder <- h2o.targetencoder(training_frame = train_data,
                                    x = cat_cols, y = "Engagement",
                                    data_leakage_handling = 'LeaveOneOut',
                                    noise = 0.1,
                                    seed = seed)

transformed_train <- h2o.transform(target_encoder, train_data, as_training = TRUE)
transformed_test <- h2o.transform(target_encoder, test_data, noise = 0)

## Save the train and test h2o frames as .csv
#h2o.exportFile(transformed_train, "training_data.csv")
#h2o.exportFile(transformed_test, "test_data.csv")

########### Finished Data Cleaning ################
#Open cleaned dataset
#train_data <- read.csv("training_data.csv")
#test_data <- read.csv("test_data.csv")


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

##########################################################################################
#Feature Engineering and model building
set.seed(143)
train_data_all <- read.csv("training_data.csv")
train_data = train_data[sample(nrow(train_data), 10000), ]
test_data_all <- read.csv("test_data.csv")
test_data = test_data[sample(nrow(test_data), 10000), ]

# Removing irrelevant features
model_data = train_data[, -c(1,11,12,13,15,16,17,19,20,21,22,23,24,25,26,27,28)]
model_test = test_data[, -c(1,11,12,13,15,16,17,19,20,21,22,23,24,25,26,27,28)]

# Removing rows with NA if any
sum(is.na(model_data))
model_data<-na.omit(model_data)

# Adjusting columns
model_data <- model_data %>%
  select(Page.Name,User.Name,Page.Category,Sponsor.Name,Sponsor.Category,Engagement,Type,Post.Created.Day, everything())

model_test <- model_test %>%
  select(Page.Name,User.Name,Page.Category,Sponsor.Name,Sponsor.Category,Engagement,Type,Post.Created.Day, everything())

#################################################################
# Removing useless features using correlation matrix
# ensure the results are repeatable
set.seed(143)
# calculate correlation matrix
correlationMatrix <- cor(model_data[,9:123])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.50)
# print indexes of highly correlated attributes
print(highlyCorrelated)


############ Don't run on whole data set #########################
# Feature importance generalized linear model
set.seed(143)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Engagement~., data=model_data[,6:116], method="glm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model)
# summarize importance
print(importance)

##################################################################
# Step forward and backward elimination
# Step 1: Define base intercept only model
base.mod <- lm(Engagement ~ 1 , data=model_data[,6:116])  

# Step 2: Full model with all predictors
all.mod <- lm(Engagement ~ . , data= model_data[,6:116]) 

# Step 3: Perform step-wise algorithm. direction='both' implies both forward and backward stepwise
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 100)  

# Step 4: Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

# Show
print(shortlistedVars)

###############################################################################
model_final <- lm(Engagement ~ Post.Views 
                  +Total.Views.For.All.Crossposts 
                  +Total.Views 
                  +User.Name_te 
                  +male 
                  +female 
                  , data = model_data)

summary(model_final)
stargazer(model_final, type="text", title="Regression Results",
          dep.var.labels=c("Effects on Engangement"),
          covariate.labels=c("Post views", "Total views for all crossposts","Total views", "User Name encoded","Male",
                             "Female"),
          omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE, digits.extra = 2, digits = 2)

str(model_data)
str(model_test)

# Accuracy check on test dataset
sapply(model_test, class)
predictions <- predict(model, model_test)
data.frame(R2 = R2(predictions, model_test$Engagement), 
           RMSE = RMSE(predictions, model_test$Engagement), 
           MAE = MAE(predictions, model_test$Engagement))

# Ridge regression
y <- model_data %>% select(Engagement) %>% 
  scale(center = TRUE, scale = FALSE) %>% 
  as.matrix()

X <- model_data %>% select(c(Post.Views
                             ,Total.Views.For.All.Crossposts
                             ,Total.Views
                             ,User.Name_te
                             ,male
                             ,female)) %>% as.matrix()

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

ridge_cv <- cv.glmnet(X, y, alpha = 0, 
                      lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
plot(ridge_cv)


################################################################################
# Additional graphical analysis
graph = rbind(train_data_all, test_data_all)

# interactions vs weekdays
dev.off()
df = graph %>% group_by(Post.Created.Day)  %>%
  summarise(No_interactions = sum(Total.Interactions)/1000000,
            .groups = 'drop')

ggplot(df, aes(y=No_interactions, x=Post.Created.Day)) + 
  geom_bar(position="dodge",stat="identity")+
  ggtitle("Interaction(in million) vs weekdays") +
  ylab("No of Interaction (in million)")+
  xlab("Post created day")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#
dev.off()
df2 = graph %>% group_by(Type)  %>%
  summarise(sponsor = sum(hasSponsor),
            .groups = 'drop')

ggplot(df2, aes(y=sponsor, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Type vs No of sponsors") +
  ylab("No of sponsors")+
  xlab("Types")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

