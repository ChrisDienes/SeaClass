#' SeaClass Output Code Creator
#'
#' SeaClass function which outputs analysis code from the output tab based on users input and actions.
#' @details SeaClass helper function; not intended for general use.
#' @param methodvar current application method variable. Options include: "Bias Correction", "Cost Models", "Decision Tree", "Mahalanobis Distance", "One Class SVM", "Prediction Models", "Single Variable: All", "Single Variable: Categorical", "SMOTE", "Train/Test Split", "Thresholds: Accuracy", "Thresholds: True Positive", "Variable Selection: PCA", and "Variable Selection: Supervised".
#' @param dfvar current application data frame variable. Defaults to NULL.
#' @param responsevar current application response variable. Defaults to NULL.
#' @param tool1 current application input variable from shiny \code{input$tool1}. Defaults to NULL.
#' @param tool2 current application input variable from shiny \code{input$tool2}. Defaults to NULL.
#' @return Returns R code in text format for current application analysis.
#' @seealso \code{\link{SeaClass}}
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(1000,0,1),ncol=10,nrow=100)
#' X[1:10,] <- X[1:10,] + 3
#' Y <- c(rep(1,10), rep(0,990))
#' DF <- data.frame(Y = Y , X)
#' my_text <- code_output(methodvar = "Mahalanobis Distance", dfvar = "DF", responsevar = "Y", tool1 = 20, tool2 = NULL)
#' cat(my_text)}
#' @export

code_output = function(methodvar, dfvar = NULL, responsevar = NULL, tool1 = NULL, tool2 = NULL){
  if(methodvar == "Mahalanobis Distance"){
    return_output <- paste0(
"
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Load Packages:
library(SeaClass)
library(ggplot2)
library(robustbase)
# Final settings from application
response_var_name <- \"",responsevar,"\"",
"\nuser_cutpoint <- ",tool1,
"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- df_reduced[, keep_cols]
# Remove zero variance and highly correlated variables:
my_check <- cov_check(cov = cov(df_reduced), cut_point = 0.9999)
if(my_check$remove_length > 0){df_reduced <- df_reduced[,-my_check$remove_vars]}
# compute mahalanobis distances:
my_mahal <- mahalanobis(df_reduced, center = colMeans(df_reduced), cov = cov(df_reduced), inverted = FALSE)
# theoretical cut point under gaussian data:
n <- nrow(df_reduced)
p <- ncol(df_reduced)
informed_prob <- sum(response_var01)/n
chisq_cut <- qchisq(1-informed_prob, p)
scored_chisq <- score_threshold(x = my_mahal, group = response_var01, pos_class = 1, cut = chisq_cut, type = \"upper\")
# boxplot methods:
boxplot_cuts <- boxplot_cutpoints(x = my_mahal, iqr_mult = 3)
scored_boxplot <- score_threshold(x = my_mahal, group = response_var01, pos_class = 1, cut = boxplot_cuts$standard_rule[[2]], type = \"upper\")
scored_adjusted <- score_threshold(x = my_mahal, group = response_var01, pos_class = 1, cut = boxplot_cuts$adjusted_rule[[2]], type = \"upper\")
# accuracy threshold:
accuracy_cut <- accuracy_threshold(x = my_mahal, group = response_var01, pos_class = 1)
if(!is.na(accuracy_cut$cut)){
  scored_accuracy <- score_threshold(x = my_mahal, group = response_var01, pos_class = 1, cut=accuracy_cut$cut, type=accuracy_cut$direction)
} else {
  scored_accuracy <- list(FP=0, TP=0, Misclass=mean(response_var01), Direction = \"Flag None\")
}
# user choice:
scored_user <- score_threshold(x = my_mahal, group = response_var01,  pos_class = 1, cut = user_cutpoint, type = \"upper\")
# combine results into table:
my_table <- data.frame(Method = c(\"Chi-Square\", \"Boxplot\", \"Adjusted Boxplot\", \"Accuracy Threshold\", \"User Specified\"),
    Cutpoint = round(c(chisq_cut,boxplot_cuts$standard_rule[[2]],boxplot_cuts$adjusted_rule[[2]],accuracy_cut$cut,user_cutpoint),digits=3),
    FP  = round(c(scored_chisq[[1]],scored_boxplot[[1]],scored_adjusted[[1]],scored_accuracy[[1]],scored_user[[1]]),digits=3),
    TP  = round(c(scored_chisq[[2]],scored_boxplot[[2]],scored_adjusted[[2]],scored_accuracy[[2]],scored_user[[2]]),digits=3))
# View table:
my_table
# Make histogram:
X <- data.frame(y = my_mahal, x = seq(1, length(my_mahal)))
p <- ggplot(X, aes(x=y)) +
    geom_histogram(bins=30,colour=\"blue\",fill = \"#1E90FF\") +
    ggtitle(\"Mahalanobis Distance\") +
    xlab(\"Distance\") +
    ylab(\"Frequency\") +
    geom_vline(xintercept = user_cutpoint, colour = \"red\", size = 2)
p
")}

  if(methodvar == "One Class SVM"){
    return_output <- paste0(
"
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Load Packages:
library(e1071)
# Final settings from application
response_var_name <- \"",responsevar,"\"",
"\nnu_param <- ",tool1,
"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- df_reduced[, keep_cols]
# Remove zero variance variables:
my_check <- which(apply(df_reduced,2,sd) == 0)
if(length(my_check) > 0){df_reduced <- df_reduced[,-my_check]}
# Fit one class SVM:
svm_model <- svm(x=df_reduced,y=NULL, type='one-classification',scale=TRUE,kernel=\"radial\", nu = nu_param/100)
# Score model:
svm_pred <- ifelse(svm_model$fitted, majority_class, minority_class)
table_svm <- table(Predicted=svm_pred,Truth=response_var)
# confusion matrix:
table_svm
# performance metrics:
pos_index <- which(response_var==minority_class)
cat(paste0(\"True Positive %: \",round(100*mean(response_var[pos_index] == svm_pred[pos_index]),digits=4)))
cat(paste0(\"False Positive %: \",round(100*mean(response_var[-pos_index] != svm_pred[-pos_index]),digits=4)))
cat(paste0(\"Misclassification %: \",round(100*mean(response_var != svm_pred),digits=4)))
")}

  if(methodvar == "Decision Tree"){
    return_output <- paste0(
"
library(rpart)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nuser_tree_depth <- ", tool1,
"\nuser_prior <- ", tool2,
"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
Y <- factor(response_var, levels = c(minority_class, majority_class), ordered=TRUE)
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(df_reduced,is.numeric)))
if(length(categorical_vars) > 0){
  remove_cat = NULL
  for(cv in categorical_vars){if(length(unique(df_reduced[,cv])) > max_num_of_levels){remove_cat = c(remove_cat, cv)}}
  if(!is.null(remove_cat)){df_reduced <- df_reduced[,-remove_cat]}
}
df_reduced <- data.frame(YYY = Y, df_reduced)
tree_fit <- rpart(YYY ~., data = df_reduced, method = \"class\",
    parms = list(prior = c(user_prior,1-user_prior)),
    control = list(minsplit=20, minbucket=5, maxdepth = user_tree_depth))
Yhat <- as.character(predict(tree_fit, type = \"class\"))
par(mfrow = c(1,1), xpd = NA)
plot(tree_fit, uniform=TRUE, main=\"Decision Tree\")
text(tree_fit, use.n=TRUE, cex=.8)
cat(paste0(\"Total Cases: \", length(Y),\"\\n\",
    \"Total Fail:  \", sum(Y == minority_class), \"\\n\",
    \"Total Pass:  \", sum(Y != minority_class), \"\\n \\n\",
    \"True Positive Rate:      \", round(100*mean(Yhat[Y == minority_class] == minority_class), digits = 2), \"\\n\",
    \"False Positive Rate:     \", round(100*mean(Yhat[Y != minority_class] == minority_class), digits = 2), \"\\n\",
    \"Misclassification Rate:  \", round(100*mean(Yhat != Y), digits = 2)))
")}


  if(methodvar == "Single Variable: All"){
    return_output <- paste0(
"
# Load libraries:
library(PRROC)
library(randomForest)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
response_var <- factor(response_var, levels=c(majority_class,minority_class))
# Removing any constant variables:
unique_check <- apply(df_reduced, 2, function(x)length(unique(x)))
constant_check <- which(unique_check == 1)
if(length(constant_check) > 0){
  df_reduced <- df_reduced[,-constant_check]
  unique_check <- unique_check[-constant_check]
}
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(df_reduced,is.numeric)))
if(length(categorical_vars) > 0){
  level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
  if(length(level_check) > 0){df_reduced <- df_reduced[,-level_check]}
}
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(df_reduced,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    df_reduced[,ct] <- as.factor(df_reduced[,ct])
  }
}
# Performing univariate logistic regression analysis:
NC <- ncol(df_reduced)
my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=6))
names(my_output_table) <- c(\"Variable\", \"Logistic P-Val\", \"Logistic Misclass %\", \"Logistic AIC\", \"Logistic AUC\", \"RF Importance\")
my_output_table$Variable <- names(df_reduced)
for(nc in 1:NC){
  logistic_model <- glm(response_var ~ df_reduced[,nc], family=binomial(link='logit'))
  my_output_table[nc,2] <- anova(logistic_model, test=\"Chisq\")$`Pr(>Chi)`[2]
  my_output_table[nc,3] <- 100*mean(ifelse(logistic_model$fitted.values > 0.5,minority_class,majority_class) != response_var)
  my_output_table[nc,4] <- logistic_model$aic
  my_output_table[nc,5] <- roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = response_var01)$auc
}
# Random forest analysis:
rf_model <- randomForest(x = df_reduced, y = response_var, ntree=100)
my_output_table[match(row.names(rf_model$importance), names(df_reduced)),6] <- rf_model$importance
# Preparing table:
my_output_table <- my_output_table[order(my_output_table[,5], decreasing = TRUE),]
# View table:
my_output_table
")}

  if(methodvar == "Single Variable: Categorical"){
    return_output <- paste0(
"
# Load libraries:
library(PRROC)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\n# partition data:
response_var  <- df[,response_var_name]
df_reduced    <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
response_var <- factor(response_var, levels=c(majority_class,minority_class))
# Removing any numeric variables, and limiting categorical variables to a predefined number of levels:
categorical_vars <- which(!unlist(lapply(df_reduced,is.numeric)))
level_test <- rep(0,length(categorical_vars))
for(cv in 1:length(categorical_vars)){level_test[cv] <- length(unique(df_reduced[,categorical_vars[cv]]))}
max_num_of_levels <- 10
min_num_of_levels <- 2
keep_vars <- categorical_vars[which(level_test <= max_num_of_levels & level_test >= min_num_of_levels)]
tmp_names <- names(df_reduced)[keep_vars]
df_reduced <- as.data.frame(df_reduced[,keep_vars],stringsAsFactors=TRUE)
names(df_reduced) <- tmp_names
# Prepare output table:
NC <- ncol(df_reduced)
my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=6))
names(my_output_table) <- c(\"Variable\", \"Logistic P-Val\", \"Logistic Misclass %\", \"Logistic AIC\", \"Logistic AUC\", \"Chi-Square Test P-Val\")
my_output_table$Variable <- names(df_reduced)
# Performing univariate logistic regression analysis:
for(nc in 1:NC){
  logistic_model <- glm(response_var ~ df_reduced[,nc], family=binomial(link='logit'))
  my_output_table[nc,2] <- anova(logistic_model, test=\"Chisq\")$`Pr(>Chi)`[2]
  my_output_table[nc,3] <- 100*mean(ifelse(logistic_model$fitted.values > 0.5,minority_class,majority_class) != response_var)
  my_output_table[nc,4] <- logistic_model$aic
  my_output_table[nc,5] <- roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = response_var01)$auc
  tmp_TAB <- table(df_reduced[,nc],response_var)
  my_output_table[nc,6] <- chisq.test(tmp_TAB, simulate.p.value = TRUE, B = 100000)$p.value
}
# Preparing table:
my_output_table <- my_output_table[order(my_output_table[,6], decreasing = FALSE),]
# View table:
my_output_table
")}

  if(methodvar == "Thresholds: Accuracy"){
    return_output <- paste0(
"
library(SeaClass)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\n# partition data:
response_var  <- df[,response_var_name]
df_reduced    <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- df_reduced[, keep_cols]
Nc <- ncol(df_reduced)
user_matrix <- as.data.frame(matrix(0,ncol=5,nrow=Nc))
names(user_matrix) <- c(\"Variable\",\"Rule\",\"FP %\", \"TP %\", \"Misclass %\")
user_matrix$Variable <- names(df_reduced)
for(nc in 1:Nc){
  my_thresh <- accuracy_threshold(x=df_reduced[,nc], group=response_var01, pos_class=1)
  if(!is.na(my_thresh$cut)){
    scored_thresh <- score_threshold(x=df_reduced[,nc], group=response_var01, pos_class=1, cut=my_thresh$cut, type=my_thresh$direction)
    user_matrix[nc,c(3,4,5,2)] <- unlist(scored_thresh)
  } else {
    user_matrix[nc,2:5] <- c(\"Flag None\",0,0,100*mean(response_var01 == 1))
  }
}
user_matrix <- user_matrix[order(as.numeric(user_matrix[,5])),]
user_matrix[,3:5] <- apply(user_matrix[,3:5],2,as.numeric)
print(user_matrix, row.names = FALSE, digits=4)
")}

  if(methodvar == "Thresholds: True Positive"){
    return_output <- paste0(
"
library(SeaClass)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nmax_fp <- ", tool1,
"\n# partition data:
response_var  <- df[,response_var_name]
df_reduced    <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- df_reduced[, keep_cols]
Nc <- ncol(df_reduced)
user_matrix <- as.data.frame(matrix(0,ncol=5,nrow=Nc))
names(user_matrix) <- c(\"Variable\",\"Rule\",\"FP %\", \"TP %\", \"Misclass %\")
user_matrix$Variable <- names(df_reduced)
for(nc in 1:Nc){
  user_matrix[nc,c(3,4,5,2)] = unlist(max_fp_threshold(x=df_reduced[,nc], group=response_var01, pos_class=1, max_fp = max_fp/100))
}
user_matrix <- user_matrix[order(user_matrix[,4],decreasing = TRUE),]
user_matrix[,3:5] <- apply(user_matrix[,3:5],2,as.numeric)
print(user_matrix, row.names = FALSE, digits=4)
")}

  if(methodvar == "SMOTE"){
    return_output <- paste0(
"
library(DMwR)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nminorX <- ", tool1,
"\nmajorX <- ", tool2,
"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- data.frame(YYY = response_var, df_reduced[,keep_cols])
df_reduced[,1] <- as.factor(df_reduced[,1])
names(df_reduced)[1] <- response_var_name
myFormula <- as.formula(paste0(response_var_name, \"~.\"))
SMOTE_df <- SMOTE(myFormula, data=df_reduced ,perc.over=(minorX - 1)*100, perc.under=(majorX)*(minorX/(minorX-1))*100, k=5)
cat(\"Original data set: \")
print(table(df_reduced[,response_var_name]))
cat(\"\\nSMOTE'd data set:\")
print(table(SMOTE_df[,response_var_name]))
")}

  if(methodvar == "Train/Test Split"){
    return_output <- paste0(
"
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nmajorp <- ", tool1,
"\nminorp <- ", tool2,
"\n# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(df[,response_var_name])
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
class_index <- which(df[,response_var_name] == minority_class)
n1 <- length(class_index)
n0 <- nrow(df) - n1
index_major <- sample.int(n=n0, size=ceiling(n0*majorp/100), replace=FALSE)
index_minor <- sample.int(n=n1, size=ceiling(n1*minorp/100), replace=FALSE)
TRAIN_df <- rbind(df[-class_index,][index_major,],df[class_index,][index_minor,])
TEST_df  <- rbind(df[-class_index,][-index_major,],df[class_index,][-index_minor,])
TAB_train <- table(TRAIN_df[,response_var_name])
TAB_test  <- table(TEST_df[,response_var_name])
cat(paste0(\"Training data set (\", round(100*min(TAB_train)/sum(TAB_train),digits=4) , \"% minority class):\"))
print(TAB_train)
cat(paste0(\"Testing data set (\", round(100*min(TAB_test)/sum(TAB_test),digits=4) , \"% minority class):\"))
print(TAB_test)
")}

  if(methodvar == "Variable Selection: Supervised"){
    return_output <- paste0(
"
# Load libraries:
library(PRROC)
library(randomForest)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nselection_method <-\"", tool1,
"\"\nselection_count <-", tool2,
"\n# partition data:
response_var <- df[,response_var_name]
df_reduced   <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
response_var <- factor(response_var, levels=c(majority_class,minority_class))
# Removing any constant variables:
unique_check <- apply(df_reduced, 2, function(x)length(unique(x)))
constant_check <- which(unique_check == 1)
if(length(constant_check) > 0){
  df_reduced <- df_reduced[,-constant_check]
  unique_check <- unique_check[-constant_check]
}
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(df_reduced,is.numeric)))
if(length(categorical_vars) > 0){
  level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
  if(length(level_check) > 0){df_reduced <- df_reduced[,-level_check]}
}
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(df_reduced,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    df_reduced[,ct] <- as.factor(df_reduced[,ct])
  }
}
# Performing univariate logistic regression analysis:
NC <- ncol(df_reduced)
my_output_table <- data.frame(matrix(NA, nrow=NC, ncol=4))
names(my_output_table) <- c(\"Variable\", \"Logistic AUC\",\"Logistic AIC\", \"RF Importance\")
my_output_table$Variable <- names(df_reduced)
for(nc in 1:NC){
  logistic_model <- glm(response_var ~ df_reduced[,nc], family=binomial(link='logit'))
  my_output_table[nc,2] <- roc.curve(scores.class0 = logistic_model$fitted.values,weights.class0 = response_var01)$auc
  my_output_table[nc,3] <- logistic_model$aic
}
# Random forest analysis:
rf_model <- randomForest(x = df_reduced, y = response_var, ntree=100)
my_output_table[match(row.names(rf_model$importance), names(df_reduced)),4] <- rf_model$importance
# Preparing table:
order_by_loc <- which(selection_method == names(my_output_table))
order_direction <- ifelse(selection_method == \"Logistic AIC\", FALSE, TRUE)
my_output_table <- my_output_table[order(my_output_table[,order_by_loc], decreasing = order_direction),]
# View table:
my_output_table
# Subset data:
subset_variables <-  my_output_table$Variable[1:selection_count]
SUBSET_df <- data.frame(YYY = response_var, df_reduced[,subset_variables])
names(SUBSET_df) <- c(response_var_name, subset_variables)
# View data:
head(SUBSET_df)
")}

  if(methodvar == "Variable Selection: PCA"){
    return_output <- paste0(
"
# Load packages:
library(SeaClass)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nnum_of_pcs <-", tool1,
"\n# partition data:
response_var  <- df[,response_var_name]
df_reduced    <- df[,which(names(df) != response_var_name)]
# Remove categorical variables:
keep_cols <- which(unlist(lapply(df_reduced,is.numeric)))
df_reduced <- df_reduced[, keep_cols]
# Remove zero variance and highly correlated variables:
my_check <- cov_check(cov = cov(df_reduced), cut_point = 0.9999)
if(my_check$remove_length > 0){df_reduced <- df_reduced[,-my_check$remove_vars]}
# Perform PCA:
df_pca <- prcomp(df_reduced, center = TRUE, scale. = TRUE)
# Examine variance explained
cum_var  <- 100*cumsum(df_pca$sdev^2)/sum(df_pca$sdev^2)
plot(0:length(cum_var),c(0,cum_var),xlab=\"Number of PCs\", ylab=\"Variance Explained (%)\",ylim=c(0,100),type=\"l\",
    main=paste0(round(cum_var[num_of_pcs],digits=2),\"% of Variance Explained by \",num_of_pcs, \" PCs\"))
lines(c(num_of_pcs,num_of_pcs),c(0,100),lty=\"solid\",col=\"red\")
# Subset data:
PCA_df <- data.frame(YYY = response_var, df_pca$x[,1:num_of_pcs])
names(PCA_df) <- c(response_var_name, paste0(\"PC\",1:num_of_pcs))
# View data:
head(PCA_df)
")}

  if(methodvar == "Prediction Models"){
    tmp1  <- ""
    if(!startsWith(tool1,"None")){
      tmp1 <- paste0(
"
#################################
### Score New Data
#################################
test_df <- ", tool1,
"\ntest_response <- test_df[,response_var_name]
test_response01 <- ifelse(test_response == minority_class, 1, 0)
pos_index <- which(test_response01 == 1)
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(test_df,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    test_df[,ct] <- as.factor(test_df[,ct])
  }
}
# Training Output Prep:
my_models <- c(\"Baseline\",\"Random Forest\",\"LDA\",\"Stepwise Logistic\",\"Penalized Logistic\",\"XGBoost\",\"SVM\",\"KNN (k=5)\")
my_test_table <- data.frame(Model = my_models, matrix(0,ncol=5,nrow=length(my_models)))
names(my_test_table)[2:6] <- c(\"Misclass %\",\"AUC-ROC\",\"AUC-PR\", \"TP %\", \"FP %\")
### Baseline Stupid Model:
my_test_table[1,2:6] <- c(100*mean(test_response01), 0.5, 0.5, 0, 0)
### Random Forest Model:
rf_prob <- predict(rf_model, test_df, type=\"prob\")
rf_pred <- predict(rf_model, test_df, type=\"response\")
minority_index <- which(attributes(rf_prob)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(rf_prob)$dimnames[[2]] == majority_class)
my_test_table[2,2:6] <- c(100*mean(rf_pred != test_response),
    roc.curve(scores.class0 = rf_prob[,minority_index],weights.class0 = test_response01)$auc,
    pr.curve(rf_prob[,majority_index],rf_prob[,minority_index])$auc.integral,
    100*mean(rf_pred[pos_index] == test_response[pos_index]),
    100*mean(rf_pred[-pos_index] != test_response[-pos_index]))
### LDA model:
lda_pred  <- predict(lda_model, newdata = test_df[,colnames(lda_model$means)])
minority_index <- which(attributes(lda_pred$posterior)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(lda_pred$posterior)$dimnames[[2]] == majority_class)
my_test_table[3,2:6] <- c(100*mean(lda_pred$class != test_response),
    roc.curve(scores.class0 = lda_pred$posterior[,minority_index],weights.class0 = test_response01)$auc,
    pr.curve(lda_pred$posterior[,majority_index],lda_pred$posterior[,minority_index])$auc.integral,
    100*mean(lda_pred$class[pos_index] == test_response[pos_index]),
    100*mean(lda_pred$class[-pos_index] != test_response[-pos_index]))
### Stepwise Logistic Regression:
logistic_step_prob <- predict(logistic_step_model, newdata = test_df, type=\"response\")
logistic_step_pred <- ifelse(logistic_step_prob >= 0.5, minority_class, majority_class)
my_test_table[4,2:6] <- c(100*mean(logistic_step_pred  != test_response),
    roc.curve(scores.class0 = logistic_step_prob,weights.class0 = test_response01)$auc,
    pr.curve(1-logistic_step_prob,logistic_step_prob)$auc.integral,
    100*mean(logistic_step_pred[pos_index] == test_response[pos_index]),
    100*mean(logistic_step_pred[-pos_index] != test_response[-pos_index]))
### Penalized (Ridge) Logistic Regression:
penalized_cv_prob  <- predict(penalized_cv_model,newx=as.matrix(test_df[,rownames(coef(penalized_cv_model))[-1]]),type=\"response\",s=\"lambda.min\")
penalized_cv_pred <- ifelse(penalized_cv_prob>=0.5,minority_class,majority_class)
my_test_table[5,2:6] <- c(100*mean(penalized_cv_pred  != test_response),
    roc.curve(scores.class0 = penalized_cv_prob,weights.class0 = test_response01)$auc,
    pr.curve(1-penalized_cv_prob,penalized_cv_prob)$auc.integral,
    100*mean(penalized_cv_pred[pos_index] == test_response[pos_index]),
    100*mean(penalized_cv_pred[-pos_index] != test_response[-pos_index]))
### XGBoost Model:
df_xgb <- apply(test_df[,names(train_df_numeric)],2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
xgb_prob <- predict(xgb_model, df_xgb)
xgb_pred <- ifelse(xgb_prob>=0.5,minority_class,majority_class)
my_test_table[6,2:6] <- c(100*mean(xgb_pred  != test_response),
    roc.curve(scores.class0 = xgb_prob,weights.class0 = test_response01)$auc,
    pr.curve(1-xgb_prob,xgb_prob)$auc.integral,
    100*mean(xgb_pred[pos_index] == test_response[pos_index]),
    100*mean(xgb_pred[-pos_index] != test_response[-pos_index]))
### SVM scoring:
svm_pred <- predict(svm_model, test_df[,names(svm_model$x.scale$`scaled:center`)], probability = TRUE)
svm_prob <- attr(svm_pred, \"probabilities\")
minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(svm_prob)$dimnames[[2]] == majority_class)
my_test_table[7,2:6] <- c(100*mean(svm_pred != test_response),
    roc.curve(scores.class0 = svm_prob[,minority_index],weights.class0 = test_response01)$auc,
    pr.curve(svm_prob[,majority_index],svm_prob[,minority_index])$auc.integral,
    100*mean(svm_pred[pos_index] == test_response[pos_index]),
    100*mean(svm_pred[-pos_index] != test_response[-pos_index]))
### KNN scoring:
knn_test <-  knn(train=train_df_numeric, test=test_df[,names(train_df_numeric)], cl=response_var,prob=TRUE,k=5)
knn_pred  <-  as.vector(knn_test)
knn_prob  <- attributes(knn_test)$prob
knn_prob[knn_pred == majority_class] <- 1-knn_prob[knn_pred == majority_class]
my_test_table[8,2:6] <- c(100*mean(knn_pred  != test_response),
    roc.curve(scores.class0 = knn_prob,weights.class0 = test_response01)$auc,
    pr.curve(1-knn_prob,knn_prob)$auc.integral,
    100*mean(knn_pred[pos_index] == test_response[pos_index]),
    100*mean(knn_pred[-pos_index] != test_response[-pos_index]))

my_test_table[order(my_test_table[,2]),]
"
    )
    }
    return_output <- paste0(
"
library(SeaClass)
library(PRROC)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\n# partition data:
response_var <- df[,response_var_name]
train_df     <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
# Data Prep:
if(is.numeric(response_var)){response_var01 <- ifelse(response_var == as.numeric(minority_class),1,0)}
if(!is.numeric(response_var)){response_var01 <- ifelse(response_var == minority_class,1,0)}
response_var <- factor(response_var, levels=c(majority_class,minority_class))
pos_index <- which(response_var == minority_class)
# Removing any constant variables:
unique_check <- apply(train_df, 2, function(x)length(unique(x)))
constant_check <- which(unique_check == 1)
if(length(constant_check) > 0){
  train_df <- train_df[,-constant_check]
  unique_check <- unique_check[-constant_check]
}
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(train_df,is.numeric)))
if(length(categorical_vars) > 0){
  level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
  if(length(level_check) > 0){train_df <- train_df[,-level_check]}
}
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(train_df,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    train_df[,ct] <- as.factor(train_df[,ct])
  }
}
# Prepare numeric only data set:
keep_cols <- which(unlist(lapply(train_df,is.numeric)))
train_df_numeric <- train_df[, keep_cols]
my_check <- cov_check(cov = cov(train_df_numeric), cut_point = 0.9999)
if(my_check$remove_length > 0){train_df_numeric <- train_df_numeric[,-my_check$remove_vars]}
# Training Output Prep:
my_models <- c(\"Baseline\",\"Random Forest\",\"LDA\",\"Stepwise Logistic\",\"Penalized Logistic\",\"XGBoost\",\"SVM\",\"KNN (k=5)\")
my_train_table <- data.frame(Model = my_models, matrix(0,ncol=5,nrow=length(my_models)))
names(my_train_table)[2:6] <- c(\"Misclass %\",\"AUC-ROC\",\"AUC-PR\",\"TP %\", \"FP %\")
### Baseline Stupid Model:
my_train_table[1,2:6] <- c(100*mean(response_var01), 0.5, 0.5, 0, 0)
### Random Forest Model:
library(randomForest)
rf_model <- randomForest(x = train_df, y = response_var, ntree=100)
minority_index <- which(attributes(rf_model$votes)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(rf_model$votes)$dimnames[[2]] == majority_class)
my_train_table[2,2:6] <- c(100*mean(rf_model$predicted != response_var),
  roc.curve(scores.class0 = rf_model$votes[,minority_index],weights.class0 = response_var01)$auc,
  pr.curve(rf_model$votes[,majority_index],rf_model$votes[,minority_index])$auc.integral,
  100*mean(rf_model$predicted[pos_index] == response_var[pos_index]),
  100*mean(rf_model$predicted[-pos_index] != response_var[-pos_index]))
### LDA Model:
library(MASS)
lda_model <- lda(x=train_df_numeric,grouping=response_var)
lda_pred  <- predict(lda_model, newdata = train_df_numeric)
minority_index <- which(attributes(lda_pred$posterior)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(lda_pred$posterior)$dimnames[[2]] == majority_class)
my_train_table[3,2:6] <- c(100*mean(lda_pred$class != response_var),
    roc.curve(scores.class0 = lda_pred$posterior[,minority_index],weights.class0 = response_var01)$auc,
    pr.curve(lda_pred$posterior[,majority_index],lda_pred$posterior[,minority_index])$auc.integral,
    100*mean(lda_pred$class[pos_index] == response_var[pos_index]),
    100*mean(lda_pred$class[-pos_index] != response_var[-pos_index]))
### Stepwise Logistic Regression:
my_stepwise_logistic_df <- cbind(YYY = response_var, train_df)
intercept_only <- glm(Y ~ 1, data=my_stepwise_logistic_df, family=binomial)
upper_formula <- formula(paste0(\"YYY ~ \", paste(names(my_stepwise_logistic_df)[-1],collapse = \" + \")))
logistic_step_model <- step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),direction=\"forward\", trace = FALSE, k = log(nrow(my_stepwise_logistic_df)))
logistic_step_prob <- logistic_step_model$fitted.values
logistic_step_pred <- ifelse(logistic_step_prob >= 0.5, minority_class, majority_class)
my_train_table[4,2:6] <- c(100*mean(logistic_step_pred  != response_var),
    roc.curve(scores.class0 = logistic_step_prob,weights.class0 = response_var01)$auc,
    pr.curve(1-logistic_step_prob,logistic_step_prob)$auc.integral,
    100*mean(logistic_step_pred[pos_index] == response_var[pos_index]),
    100*mean(logistic_step_pred[-pos_index] != response_var[-pos_index]))
### Penalized (Ridge) Logistic Regression:
library(glmnet)
penalized_cv_model <- cv.glmnet(alpha=0,x=as.matrix(train_df_numeric),y=response_var01,nfolds=10,family=\"binomial\",type.measure=\"class\")
penalized_cv_prob  <- predict(penalized_cv_model,newx=as.matrix(train_df_numeric),type=\"response\",s=\"lambda.min\")
penalized_cv_pred <- ifelse(penalized_cv_prob>=0.5,minority_class,majority_class)
my_train_table[5,2:6] <- c(100*mean(penalized_cv_pred  != response_var),
    roc.curve(scores.class0 = penalized_cv_prob,weights.class0 = response_var01)$auc,
    pr.curve(1-penalized_cv_prob,penalized_cv_prob)$auc.integral,
    100*mean(penalized_cv_pred[pos_index] == response_var[pos_index]),
    100*mean(penalized_cv_pred[-pos_index] != response_var[-pos_index]))
### XGBoost Model:
library(xgboost)
df_xgb <- apply(train_df_numeric,2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
xgb_model <- xgboost(data = df_xgb,label = response_var01,objective = \"binary:logistic\",nrounds = 100,verbose=0)
xgb_prob <- predict(xgb_model, df_xgb)
xgb_pred <- ifelse(xgb_prob>=0.5,minority_class,majority_class)
my_train_table[6,2:6] <- c(100*mean(xgb_pred  != response_var),
    roc.curve(scores.class0 = xgb_prob,weights.class0 = response_var01)$auc,
    pr.curve(1-xgb_prob,xgb_prob)$auc.integral,
    100*mean(xgb_pred[pos_index] == response_var[pos_index]),
    100*mean(xgb_pred[-pos_index] != response_var[-pos_index]))
### SVM model:
library(e1071)
svm_model <- svm(x=train_df_numeric,y=response_var,scale=TRUE,kernel=\"radial\",probability=TRUE)
svm_pred <- predict(svm_model, train_df_numeric, probability = TRUE)
svm_prob <- attr(svm_pred, \"probabilities\")
minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
majority_index <- which(attributes(svm_prob)$dimnames[[2]] == majority_class)
my_train_table[7,2:6] <- c(100*mean(svm_pred != response_var),
    roc.curve(scores.class0 = svm_prob[,minority_index],weights.class0 = response_var01)$auc,
    pr.curve(svm_prob[,majority_index],svm_prob[,minority_index])$auc.integral,
    100*mean(svm_pred[pos_index] == response_var[pos_index]),
    100*mean(svm_pred[-pos_index] != response_var[-pos_index]))
### KNN Model:
library(class)
knn_model <-  knn.cv(train=train_df_numeric,cl=response_var,prob=TRUE,k=5)
knn_pred  <-  as.vector(knn_model)
knn_prob  <- attributes(knn_model)$prob
knn_prob[knn_pred == majority_class] <- 1-knn_prob[knn_pred == majority_class]
my_train_table[8,2:6] <- c(100*mean(knn_pred  != response_var),
    roc.curve(scores.class0 = knn_prob,weights.class0 = response_var01)$auc,
    pr.curve(1-knn_prob,knn_prob)$auc.integral,
    100*mean(knn_pred[pos_index] == response_var[pos_index]),
    100*mean(knn_pred[-pos_index] != response_var[-pos_index]))\n
my_train_table[order(my_train_table[,2]),]\n",
      tmp1)
    }

  if(methodvar == "Cost Models"){
    return_output <- paste0(
"
library(SeaClass)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nfp_cost <-  ", tool1,
"\nfn_cost <-  ", tool2,
"\n# partition data:
response_var <- df[,response_var_name]
df_input     <- df[,which(names(df) != response_var_name)]
# Identifying minority class. If balanced, then return alphabetically:
TAB <- table(response_var)
if(sum(TAB == min(TAB)) == 1){
  minority_class <- names(which.min(TAB))
  majority_class <- names(which.max(TAB))
}else{
  minority_class <- sort(names(TAB))[2]
  majority_class <- sort(names(TAB))[1]
}
# Data Prep:
response_var <- factor(response_var, levels=c(minority_class, majority_class), ordered=TRUE)
pos_index <- which(response_var == minority_class)
neg_index <- (1:length(response_var))[-pos_index]
# Store data for later:
n0 <- length(neg_index)
n1 <- length(pos_index)
test_size_proportion <- 0.20
test_index <- c(neg_index[sample.int(n=n0, size=ceiling(n0*test_size_proportion), replace=FALSE)],
    pos_index[sample.int(n=n1, size=ceiling(n1*test_size_proportion), replace=FALSE)])
test_index <- sort(test_index)
y_train <- response_var[-test_index]
y_test  <- response_var[test_index]
train_pos <- (y_train == minority_class)
test_pos <- (y_test == minority_class)
new_decision_rule <- fp_cost/(fp_cost + fn_cost)
# Removing any constant variables based on training:
unique_check <- apply(df_input[-test_index,], 2, function(x)length(unique(x)))
constant_check <- which(unique_check == 1)
if(length(constant_check) > 0){
  df_input <- df_input[,-constant_check]
  unique_check <- unique_check[-constant_check]
}
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(df_input,is.numeric)))
if(length(categorical_vars) > 0){
  level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
  if(length(level_check) > 0){df_input <- df_input[,-level_check]}
}
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(df_input,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    df_input[,ct] <- as.factor(df_input[,ct])
  }
}
# Prepare numeric only data set:
keep_cols <- which(unlist(lapply(df_input,is.numeric)))
df_input_numeric <- df_input[, keep_cols]
my_check <- cov_check(cov = cov(df_input_numeric[-test_index,]), cut_point = 0.9999)
if(my_check$remove_length > 0){df_input_numeric <- df_input_numeric[,-my_check$remove_vars]}
# Output Prep:
my_models <- c(\"Baseline\",\"Decision Tree\",\"LDA\",\"Stepwise Logistic\",\"Penalized Logistic\",
               \"Random Forest\",\"XGBoost\",\"SVM\")
my_table <- data.frame(Model = my_models, matrix(0,ncol=4,nrow=length(my_models)))
names(my_table)[2:5] <- c(\"Train Misclass %\", \"Test Misclass %\",\"Train Cost\",\"Test Cost\")
# Baseline Stupid Model:
my_table[1,2:5] <- c(100*mean(response_var[-test_index] == minority_class),
    100*mean(response_var[test_index] == minority_class),
    fn_cost*sum(response_var[-test_index] == minority_class),
    fn_cost*sum(response_var[test_index] == minority_class))
# Decision Tree Model:
library(rpart)
model_df <- data.frame(YYY = response_var, df_input)
tree_fit <- rpart(YYY ~., data = model_df[-test_index,], method = \"class\",
    parms = list(loss = matrix(c(0,1,1,0),ncol=2)),
    control = list(minsplit=20, minbucket=5, maxdepth = 10))
tree_prob <- predict(tree_fit, type = \"prob\", newdata = model_df)
minority_index <- which(attributes(tree_prob)$dimnames[[2]] == minority_class)
Yhat <- ifelse(tree_prob[,minority_index] >= new_decision_rule,minority_class,majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[2,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# LDA Model:
library(MASS)
lda_model <- lda(x=df_input_numeric[-test_index,],grouping=y_train)
lda_prob <- predict(lda_model, newdata = df_input_numeric)
minority_index <- which(attributes(lda_prob$posterior)$dimnames[[2]] == minority_class)
Yhat <- ifelse(lda_prob$posterior[,minority_index] >= new_decision_rule,minority_class,majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[3,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# Logistic Stepwise Regression Model:
model_df$YYY <- ifelse(response_var== minority_class, 1, 0)
intercept_only <- glm(YYY ~ 1, data=model_df[-test_index,], family=binomial)
upper_formula <- formula(paste0(\"YYY ~ \", paste(names(model_df)[-1],collapse = \" + \")))
logistic_step_model <- step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),direction=\"forward\", trace = FALSE, k = log(nrow(model_df[-test_index,])))
logistic_prob <- predict(logistic_step_model, newdata=df_input, type=\"response\")
Yhat <- ifelse(logistic_prob >= new_decision_rule, minority_class, majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[4,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# Penalized (Ridge) Logistic Regression:
library(glmnet)
penalized_cv_model <- cv.glmnet(alpha=0,x=as.matrix(df_input_numeric[-test_index,]),y=model_df$YYY[-test_index],nfolds=10,family=\"binomial\",type.measure=\"class\")
penalized_cv_prob  <- predict(penalized_cv_model,newx=as.matrix(df_input_numeric),type=\"response\",s=\"lambda.min\")
Yhat <- ifelse(penalized_cv_prob >= new_decision_rule, minority_class, majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[5,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# Random Forest Model:
library(randomForest)
rf_model <- randomForest(x = df_input[-test_index,], y = y_train, ntree=100)
minority_index <- which(attributes(rf_model$votes)$dimnames[[2]] == minority_class)
rf_prob <- rep(0,length(response_var))
rf_prob[(1:length(response_var))[-test_index]] <- rf_model$votes[,minority_index]
rf_prob_test <- predict(rf_model, df_input[test_index,], type=\"prob\")
minority_index <- which(attributes(rf_prob_test)$dimnames[[2]] == minority_class)
rf_prob[test_index] <- rf_prob_test[,minority_index]
Yhat <- ifelse(rf_prob >= new_decision_rule, minority_class, majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[6,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# XGBoost Model:
library(xgboost)
df_xgb <- apply(df_input_numeric,2,function(x) if(is.numeric(x)){return(as.numeric(x))}else{return(x)})
response_var01 <- ifelse(response_var == minority_class,1,0)
xgb_model <- xgboost(data = df_xgb[-test_index,],label = response_var01[-test_index],objective = \"binary:logistic\",nrounds = 100,verbose=0)
xgb_prob <- predict(xgb_model, df_xgb)
Yhat <- ifelse(xgb_prob>=new_decision_rule,minority_class,majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[7,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# SVM model:
library(e1071)
svm_model <- svm(x=df_input_numeric[-test_index,],y=response_var[-test_index],scale=TRUE,kernel=\"radial\",probability=TRUE)
svm_prob <- attr(predict(svm_model, df_input_numeric, probability = TRUE), \"probabilities\")
minority_index <- which(attributes(svm_prob)$dimnames[[2]] == minority_class)
svm_prob <- svm_prob[,minority_index]
Yhat <- ifelse(svm_prob>=new_decision_rule,minority_class,majority_class)
Yhat_train <- Yhat[-test_index]
Yhat_test <- Yhat[test_index]
my_table[8,2:5] <- c(100*mean(y_train != Yhat_train),
    100*mean(y_test != Yhat_test),
    fn_cost*sum(y_train[train_pos] != Yhat_train[train_pos]) + fp_cost*sum(y_train[!train_pos] != Yhat_train[!train_pos]),
    fn_cost*sum(y_test[test_pos] != Yhat_test[test_pos]) + fp_cost*sum(y_test[!test_pos] != Yhat_test[!test_pos]))
# View table:
my_table[order(my_table[,5]),]
"
      )
  }

  if(methodvar == "Bias Correction"){
    return_output <- paste0(
"
library(Zelig)
# Load your data and asign to data fame object df:
df <- ",dfvar,
"\n# Final settings from application
response_var_name <- \"", responsevar,
"\"\nminority_class <- \"", tool1,
"\"\n# True minority class percentage:
tau <- ", tool2,
"\n# partition data:
response_var <- df[,response_var_name]
df_input     <- df[,which(names(df) != response_var_name)]
# Data Prep:
tmp_classes <- unique(response_var)
response_var <- ifelse(response_var== minority_class, 1, 0)
# Removing any constant variables based on training:
unique_check <- apply(df_input, 2, function(x)length(unique(x)))
constant_check <- which(unique_check == 1)
if(length(constant_check) > 0){
  df_input <- df_input[,-constant_check]
  unique_check <- unique_check[-constant_check]
}
# Limiting any categorical variables to a predefined number of levels:
max_num_of_levels <- 10
categorical_vars <- which(!unlist(lapply(df_input,is.numeric)))
if(length(categorical_vars) > 0){
  level_check <- categorical_vars[which(unique_check[categorical_vars] > max_num_of_levels)]
  if(length(level_check) > 0){df_input <- df_input[,-level_check]}
}
# Converting any character columns to factors:
character_test <- which(!unlist(lapply(df_input,is.numeric)))
if(length(character_test) > 0){
  for(ct in character_test){
    df_input[,ct] <- as.factor(df_input[,ct])
  }
}
# Logistic Stepwise Regression Model:
model_df <- data.frame(YYY = response_var, df_input)
intercept_only <- glm(YYY ~ 1, data=model_df, family=binomial)
upper_formula <- formula(paste0(\"YYY ~ \", paste(names(model_df)[-1],collapse = \" + \")))
logistic_step_model <- step(intercept_only,scope=list(lower=formula(intercept_only),upper=upper_formula),direction=\"forward\", trace = FALSE, k = log(nrow(model_df)))
logistic_prob <- predict(logistic_step_model, newdata=model_df, type=\"response\")
logistic_pred <- ifelse(logistic_prob >= 0.50, 1, 0)
# Rare Events Logistic: Bias Correction:
# See: King G and Zeng L (2001). Logistic Regression in Rare Events Data. Political Analysis, 9 (2), pp. 137-163.
# Using best model from stepwise fit:
bias_adjusted <- zelig(logistic_step_model$formula, data = model_df,
    case.control = \"prior\", bias.correct = TRUE,
    model = \"relogit\", tau = tau/100, cite=FALSE)
corrected_coef <- bias_adjusted$get_coef()[[1]]
pred_link <- rowSums(scale(model_df[,names(corrected_coef)[-1]], center=FALSE, scale=1/corrected_coef[-1])) + corrected_coef[1]
adjusted_prob <- 1/(1 + exp(-1*pred_link))
adjusted_pred <- ifelse(adjusted_prob >= 0.50, 1, 0)
# Plot probability differences:
plot(logistic_prob,adjusted_prob, xlab=\"Unadjusted Probabilities\", ylab=\"Bias Corrected Probabilites\", xlim=c(0,1), ylim=c(0,1))
lines(c(0,1),c(0,1), lty=\"dashed\", lwd=2, col=\"red\")
# Classsification differences:
table(logistic_pred)
table(adjusted_pred)
"
      )
  }

  return(return_output)
  }
