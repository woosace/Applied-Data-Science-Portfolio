ds <- file.choose()
ds <- read.csv(ds
                   , header = TRUE
                   , stringsAsFactors = FALSE)

str(ds)
colSums(is.na(ds)) # NO null values :)

# Number of admissions categorized by patient age

library(ggplot2)
library(RColorBrewer)
theme_set(theme_minimal())
blues_palette <- rev(brewer.pal(n = 6, name = "Blues"))
ds$age <- factor(ds$age, levels = c('[40-50)', '[50-60)', '[60-70)', '[70-80)', '[80-90)', '[90-100)'), ordered = TRUE)
ggplot(ds, aes(x = age, fill = age)) +
  geom_bar() +
  scale_fill_manual(values = blues_palette) +
  labs(title = "Number of admissions categorized by patient age.")

# Most common primary diagnoses

library(dplyr)
ds$diag_1[ds$diag_1 == "Missing"] <- "Other"
primary_diag <- table(ds$diag_1)
ds$diag_1 <- factor(ds$diag_1, levels = names(sort(primary_diag, increasing = TRUE)))

ggplot(data = ds, aes(x = diag_1, fill = diag_1)) +
  geom_bar() +
  scale_fill_brewer(palette = "BuGn") +
  labs(title = "Primary diagnosis of patient admissions.",
       x = "Count",
       y = "Primary diagnosis") +
  coord_flip()

# Readmission by age & medical specailty

cols <- c('Diabetes_ind' = 'Diabetes Diagnosis',
          'diabetes_med' = 'Prescribed Diabetes Med Indicator',
          'change' = 'Change in Diabetes Meds',
          'glucose_test' = 'Glucose Test Result',
          'A1Ctest' = 'A1C Test Result',
          'medical_specialty' = 'Medical Specialty of Admitting Physician')

# Select columns for clustering
quantCols <- names(ds)[sapply(ds, function(x) is.numeric(x))]

# Create new DataFrame with clustering variables
df_features <- ds[quantCols]

df_features

# Correlation Matrix

library(reshape2)

# Define the correlation matrix function with labels
corr_matrix <- function(dataframe, x, y, title) {
  # Create a correlation matrix
  cor_matrix <- cor(dataframe)
  
  # Melt the correlation matrix for ggplot
  cor_matrix_melted <- melt(cor_matrix)
  
  # Set up the plot
  p <- ggplot(cor_matrix_melted, aes(x=Var1, y=Var2, fill=value, label=sprintf("%.2f", value))) +
    geom_tile() +
    geom_text(aes(fill = value), vjust = 1, size = 3) +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limit=c(-1,1), space="Lab") +
    theme_minimal() +
    labs(x="Features", y="Features", title=title) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Set plot size and legend position
  p <- p + theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(hjust = 0.5),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position = "bottom",
    legend.key.width=unit(2, "cm"),
    legend.key.height=unit(0.5, "cm")
  )
  
  # Display the plot
  print(p)
}

# Call the correlation matrix function
corr_matrix(df_features, 15, 15, 'Correlation Matrix of Model Features')

# Data Transformation

# Define the character columns
textCols <- c('glucose_test', 'A1Ctest', 'change', 'diabetes_med', 'readmitted')

# Loop through the character columns
for (i in textCols) {
  # Print column name
  print(i)
  # Print unique values in the column
  print(unique(ds[, i]))
}

binaryCols <- c('change', 'diabetes_med', 'readmitted')

# Loop through the binary columns
for (i in binaryCols) {
  # Convert values to 0 if 'no', otherwise 1
  ds[, i] <- ifelse(ds[, i] == 'no', 0, 1)
}

# Define the columns
noNormalHighCols <- c('glucose_test', 'A1Ctest')

# Define a function to encode values
encode_function <- function(x) {
  ifelse(x == 'no', 0, ifelse(x == 'normal', 1, 2))
}

# Loop through the columns
for (i in noNormalHighCols) {
  # Create a new indicator column
  ds <- ds %>% mutate(!!paste0(i, '_ind') := encode_function(.data[[i]]))
}

# Encode the 'age' column
ds <- ds %>% mutate(age_t = as.integer(factor(age)))
View(ds)

#Readmission prediction (Clustering?)

library(rpart)
library(rpart.plot)
library(caret)

# Load required libraries
library(rpart)
library(rpart.plot)

set.seed(123)

# Split the data into training (80%) and testing (20%) sets
train_index <- createDataPartition(ds$readmitted, p = 0.8, list = FALSE)

X_train <- ds[train_index, -which(names(ds) == "readmitted")]  # Exclude "readmitted" from predictors
Y_train <- ds[train_index, "readmitted"]

X_test <- ds[-train_index, -which(names(ds) == "readmitted")]  # Exclude "readmitted" from predictors
Y_test <- ds[-train_index, "readmitted"]


# Specify the decision tree model
tree_model <- rpart(Y_train ~ ., data = cbind(Y_train, X_train), method = "class")

# Predict on the test set
Y_predict_test <- predict(tree_model, newdata = X_test, type = "class")

# Calculate confusion matrix
matrix <- table(Y_test, Y_predict_test)

# Calculate accuracy
accuracy <- sum(diag(matrix)) / sum(matrix)

# Print accuracy score and confusion matrix
cat('Accuracy score:', accuracy, '\n')
print('Confusion matrix')
print(matrix)

# New input data
new_input <- data.frame(age = 65, time_in_hospital = 5, n_lab_procedures = 5, n_procedures = 1, n_medications = 10, n_outpatient = 0
                        , n_inpatient = 0, n_emergency = 0, medical_specialty = "Cardiology",
                        diag_1 = 1, diag_2 = 0, diag_3 = 0, glucose_test = "no", change = 1, A1Ctest = "no", diabetes_med = 1, predicted_readmitted = "yes", A1Cresult = 1)

# Predict readmission for the new input
Y_predict_real <- predict(tree_model, newdata = new_input, type = "class")

# Print prediction



# Proj Cont.

library(dplyr)
library(skimr)
library(stringr)
install.packages('psych')
library(psych)
install.packages('ROSE')
library(ROSE)
library(ggplot2)
library(caret)

hospData <- read.csv("/Users/woosace/Downloads/IST 707 Final Project/Dataset/hospital_readmissions.csv")
dim(hospData)

# Transform Data Type (s)
hospData$admission_type_id <- as.factor(hospData$admission_type_id)
hospData$discharge_disposition_id <- as.factor(hospData$discharge_disposition_id)
hospData$admission_source_id <- as.factor(hospData$admission_source_id)


# Deal with the missing values (s)
count <- 0
for(i in 1:ncol(hospData)){
  if(is.factor(hospData[,i])){
    for(j in 1:nrow(hospData)){
      if(hospData[j,i]== "?" | hospData[j,i]== "Unknown/Invalid" ){
        count <- count + 1
        hospData[j,i] <- NA  #replace "?" and "Unknown/Invalid" values with NA
      }
    }
    if(count > 0){
      print(c(colnames(hospData)[i],count))
    }
  }
  count <- 0
}

write.csv(hospData, file = "hospData_NA.csv")

hospD <- read.csv("./hospData_NA.csv")
hospD$X <- NULL

hospD$weight <- NULL
hospD$payer_code <- NULL
hospD$medical_specialty <- NULL
dim(hospD)

# Two medications of Citoglipton and Examide deleted because all of those records have the same value (s)
hospD$examide <- NULL
hospD$citoglipton <- NULL
dim(hospD)

# remove missing values of race, gender, and prim diag
hospD <- na.omit(hospD)
dim(hospD)

# Because of the primary mission is to predict readmissions, the patients who died during this hospital readmission were not included. The encounters of "discharge disposition" values of 11, 13, 14, 19, 20, and 21 are correlated to death or hospice meaning that the patients cannot be readmitted (s)

par(mfrow = c(1,2))
barplot(table(hospD$discharge_disposition_id), main = "Before", col = 'lightblue')
#"discharge__disposition_id" tells us where the patient went after the hospitalization.
#11,13,14,19,20,21 related to death or hospice, which cannot be readmitted
#remove (s)
hospD <- hospD[!hospD$discharge_disposition_id %in% c(11,13,14,19,20,21), ]
barplot(table(hospD$discharge_disposition_id), main = "After", col = 'darkblue')

#changing the column name of admission type id to 'admission_type'
colnames(hospD)[5] <- "admission_type"
barplot(table(hospD$admission_type))

# collapse the variables
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 2, 1)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 7, 1)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 6, 5)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 8, 5)

barplot(table(hospD$admission_type), main = "After collapsing")

# Changing the name of some other variables
hospD$admission_type <- str_replace(hospD$admission_type,"1","Emergency")
hospD$admission_type <- str_replace(hospD$admission_type,"5","Other")
hospD$admission_type <- str_replace(hospD$admission_type,"3","Elective")
hospD$admission_type <- str_replace(hospD$admission_type,"4","Newborn")

hospD$admission_type <- as.factor(hospD$admission_type)
barplot(table(hospD$admission_type))

# Changing the name of column admission source id to 'admission source' (s)
colnames(hospD)[7] <- "admission_source"
barplot(table(hospD$admission_source))

hospD$admission_source <- case_when(hospD$admission_source %in% c("1","2","3") ~ "Physician   Referral",
                                    hospD$admission_source %in% c("4","5","6","8","9","10","11","12","13","14","15","17","18","19","20","21","22","23","24","25","26") ~   "Other",  
                                    TRUE ~ "Emergency Room")                                          

hospD$admission_source <- as.factor(hospD$admission_source)
barplot(table(hospD$admission_source), main = "After collapsing and changing the type")

# Discharge disposition has 29 values, for example, discharged to home, expired, and not avail (s)
# changing the name of column 'discharge disposition id to 'discharge_dispotion' (s)
colnames(hospD)[6] <- "discharge_disposition"
barplot(table(hospD$discharge_disposition))

#collapsing some other variable and change the name of variables (s)
hospD$discharge_disposition <- case_when(hospD$discharge_disposition %in% "1" ~ "Home",
                                         TRUE ~ "Other")

hospD$discharge_disposition <- as.factor(hospD$discharge_disposition)
barplot(table(hospD$discharge_disposition), main = "After collapsing and changing the type")

# CATEGORIZATION

# The 'diag_1' contains three diagnoses for any given patient as a primary, secondary, and additional diagnosis.(s)
# However, each of these had 700-900 unique ICD codes making it very difficult to include them in the model and run them reasonably (s)
# These diagnosis codes were collapsed into 9 types of disease categories, which include Circulatory, repiratory, digestive, etc. (s)

hospD$diag_1 <- as.character(hospD$diag_1)

hospD <- mutate(hospD, primary_diagnosis =
                 ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Other", 
                        # disease codes starting with V or E are in “other” category;
                        ifelse(str_detect(diag_1, "250"), "Diabetes",
                               ifelse((as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatory",
                                      ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratory", 
                                             ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestive", 
                                                    ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genitourinary",
                                                           ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasms",  
                                                                  ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Musculoskeletal",          
                                                                         ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injury",                    
                                                                                "Other"))))))))))
hospD$primary_diagnosis <- as.factor(hospD$primary_diagnosis)
table(hospD$primary_diagnosis)

#removing diag_1
hospD$diag_1 <- NULL

#regrouping the ages (s)
barplot(table(hospD$age))

hospD$age <- case_when(hospD$age %in% c("[0-10)","[10-20)","[20-30)","[30-40)") ~ "[0-40]",
                       hospD$age %in% c("[80-90)","[90-100)") ~ "[80-100]",
                       hospD$age %in% "[40-50)" ~ "[40-50]",
                       hospD$age %in% "[50-60)" ~ "[50-60]",
                       hospD$age %in% "[60-70)" ~ "[60-70]",
                       TRUE ~ "[70-80]")
barplot(table(hospD$age), main = "Regroup Age")

hospD$age <- as.factor(hospD$age)

hospD$repaglinide <- NULL
hospD$nateglinide <- NULL
hospD$chlorpropamide <-NULL
hospD$acetohexamide <- NULL
hospD$tolbutamide <- NULL
hospD$acarbose <- NULL
hospD$miglitol <- NULL
hospD$troglitazone <- NULL
hospD$tolazamide <- NULL
hospD$glyburide.metformin <- NULL
hospD$glipizide.metformin <- NULL
hospD$glimepiride.pioglitazone <- NULL
hospD$metformin.rosiglitazone <- NULL
hospD$metformin.pioglitazone <- NULL

dim(hospD)

#Categorizing 'readmitted' to boolean values 1 = patient readmitted w/n 30 days, 0 = readmission after 30 days and no readmission (s)
hospD$readmitted <- case_when(hospD$readmitted %in% c(">30","NO") ~ "0",
                              TRUE ~ "1")
hospD$readmitted <- as.factor(hospD$readmitted)
levels(hospD$readmitted)

# archiving the converted data first for convenience
write.csv(hospD, file = "hospD_bef_outlier.csv")

# Removing the outliers (s)

par(mfrow = c(2,4))
boxplot(hospD$time_in_hospital, main = "time_in_hospital")
boxplot(hospD$num_lab_procedures, main = "num_lab_procedures")
boxplot(hospD$num_procedures, main = "num_procedures")
boxplot(hospD$num_medications, main = "num_medications")
boxplot(hospD$number_outpatient, main = "number_outpatient")
boxplot(hospD$number_emergency, main = "number_emergency")
boxplot(hospD$number_inpatient, main = "number_inpatient")
boxplot(hospD$number_diagnoses, main = "number_diagnoses")

hospD$number_emergency <- NULL
hospD$number_inpatient <- NULL
hospD$number_outpatient <- NULL
dim(hospD)

# removing the outliers
outliers_remover <- function(a){
  df <- a
  aa <- c()
  count <- 1
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
      Q3 <- quantile(df[,i], 0.75, na.rm = TRUE)
      Q1 <- quantile(df[,i], 0.25, na.rm = TRUE) 
      IQR <- Q3 - Q1  #IQR(df[,i])
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(is.na(df[j,i]) == TRUE){
          next
        }
        else if(df[j,i] > upper | df[j,i] < lower){
          aa[count] <- j
          count <- count+1                  
        }
      }
    }
  }
  df <- df[-aa,]
}
hospD <- outliers_remover(hospD)

dim(hospD)
table(hospD$readmitted)

#This final dataset contains 74,866 obserations and (24 features) (s)
#'Readmitted' is the dependent variable which represents whether the patient gets readmitted to the hospital within 30 days or not (s)
#So this has 3 categories of less than 30, more than 30, and 'NO'. This has been combined and converted to binary classification


#Feature selection (s)
#There area good few general classes of feature selection algorithms or methods, and for this project wrapper methods were used which a subset of different features of combinations
#are selected, prepared, evaluated, and compared to other number of combinations (s)

#This will be one of the best ways to establish feature selection that finds only the relevant/necessary features by comparing the original values importance achievable at random
#ensure results are repeatable
set.seed(100)
install.packages('Boruta')
library(Boruta)

boruta <- Boruta(readmitted ~., data = hospD, doTrace = 2)
plot(boruta, las = 2, cex.axis = 0.5)
plotImpHistory(boruta)
attStats(boruta)
boruta


# ANALYTICAL METHODS
# Logistic Regression, Decisions Trees, Naive Bayes, and Random Forest
# K-Fold Cross Validation

#Splitting the dataset into training and testing data (s)
#set random seed (s)
set.seed(100)
train <- createDataPartition(hospD$readmitted, p = 0.8, list = FALSE)
training <- hospD[train, ]
testing <- hospD[-train, ]
#check dependent variable(training set)
table(training$readmitted)

# Balance the dataset with ROSE (Random Oversampling Example) (s)
data_rose <- ROSE(readmitted ~., data = training)$data
table(data_rose$readmitted)


# K-Fold Cross Validation (s)
trCntl <- trainControl(method = "CV",number = 10)

#Logistic Regression Model with 10-Folds Cross Validation
logitMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                       time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                       max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                     data = data_rose, trControl = trCntl, method = "glm", family = "binomial")


DTMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_rose, trControl = trCntl, method = "rpart")

DT_pred_CV <- predict(DTMod_CV, testing)
confusionMatrix(DT_pred_CV, testing$readmitted)

RFMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                  time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                 max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_rose, trControl = trCntl, method = "rf")

RF_pred_CV <- predict(RFMod_CV, testing)
confusionMatrix(RF_pred_CV, testing$readmitted)

varImp(RFMod_CV)
ggplot(varImp(RFMod_CV))

ggplot(hospD,aes(x=num_procedures,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1)+theme_bw()

ggplot(hospD,aes(x=time_in_hospital,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1)+theme_bw()

ggplot(hospD,aes(number_diagnoses,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1)+theme_bw()

ggplot(hospD,aes(num_lab_procedures,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1)+theme_bw()

ggplot(hospD,aes(num_medications,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=0.8,binwidth=1)+theme_bw()














