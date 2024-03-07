# Load required library
library(caret)
library(palmerpenguins)
library(xgboost)
library(tibble)
library(ggforce)



# +
# Note: The functions shap.score.rank, shap_long_hd and plot.shap.summary were 
# originally published at https://liuyanguu.github.io/post/2018/10/14/shap-visualization-for-xgboost/
# All the credits to the author.


## functions for plot
# return matrix of shap score and mean ranked score list
shap.score.rank <- function(xgb_model = xgb_mod, shap_approx = TRUE, 
                            X_train = mydata$train_mm){
  require(xgboost)
  require(data.table)
  shap_contrib <- predict(xgb_model, newdata=X_train,
                          predcontrib = TRUE, approxcontrib = shap_approx)
  shap_contrib <- as.data.table(shap_contrib)
  shap_contrib[,BIAS:=NULL]
  cat('make SHAP score by decreasing order\n\n')
  mean_shap_score <- colMeans(abs(shap_contrib))[order(colMeans(abs(shap_contrib)), decreasing = T)]
  return(list(shap_score = shap_contrib,
              mean_shap_score = (mean_shap_score)))
}

# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}


# prep shap data
shap.prep <- function(shap  = shap_result, X_train = mydata$train_mm, top_n){
  require(ggforce)
  # descending order
  if (missing(top_n)) top_n <- dim(X_train)[2] # by default, use all features
  if (!top_n%in%c(1:dim(X_train)[2])) stop('supply correct top_n')
  require(data.table)
  shap_score_sub <- as.data.table(shap$shap_score)
  shap_score_sub <- shap_score_sub[, names(shap$mean_shap_score)[1:top_n], with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  
  # feature values: the values in the original dataset
  fv_sub <- as.data.table(X_train)[, names(shap$mean_shap_score)[1:top_n], with = F]
  # standardize feature values
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, stdfvalue := std1(value), by = "variable"]
  # SHAP value: value
  # raw feature value: rfvalue; 
  # standarized: stdfvalue
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue" )
  shap_long2 <- cbind(shap_score_long, fv_sub_long[,c('rfvalue','stdfvalue')])
  shap_long2[, mean_value := mean(abs(value)), by = variable]
  setkey(shap_long2, variable)
  return(shap_long2) 
}

plot.shap.summary <- function(data_long, gglabs){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") + gglabs +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5) )

    
  return(plot1)
}






var_importance <- function(shap_result, top_n=10)
{
  var_importance=tibble(var=names(shap_result$mean_shap_score), importance=shap_result$mean_shap_score)
  
  var_importance=var_importance[1:top_n,]
  
  ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
}


# +
# install.packages("ggforce")

# +
# install.packages("xgboost")
# -

# Load data
data("penguins")

Mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

impute_missing_with_median <- function(trainData, testData) {
  colnames = colnames(trainData)
  for (i in 2:ncol(trainData)) {
    col = colnames[i]
    if (anyNA(trainData[[col]])) {
      pointer = trainData[[col]]
      if (is.numeric(trainData[[col]])) {
        median_val <- median(trainData[[col]], na.rm = TRUE)
        trainData[is.na(trainData[, i]), i] <- median_val
        testData[is.na(testData[, i]), i] <- median_val
      } else {
        mode_val <- Mode(trainData[[col]])
        trainData[is.na(trainData[, i]), i] <- mode_val
        testData[is.na(testData[, i]), i] <- mode_val
      }
    }
  }
  return(list(trainData = trainData, testData = testData))
}


# +
# Set seed for reproducibility
set.seed(45)

# Split data into train and test sets
trainIndex <- createDataPartition(penguins$species, p = 0.8, list = FALSE)
trainIndex <- sample(trainIndex)
trainData <- penguins[trainIndex, ]
testData <- penguins[-trainIndex, ]

# -

imputed_data = impute_missing_with_median(trainData, testData)

# Convert species to a factor
trainData$species <- as.factor(trainData$species)
testData$species <- as.factor(testData$species)

# +
encode_species <- function(species) {
  encoded_species <- ifelse(species == "Adelie", 1, 
                             ifelse(species == "Chinstrap", 2, 
                                    ifelse(species == "Gentoo", 3, NA)))
  return(encoded_species)
}

encode_species_onehot <- function(species, value) {
  encoded_species <- ifelse(species == value, 1, 0)
  return(encoded_species)
}
encode_island <- function(island) {
  encoded_island <- ifelse(island == "Biscoe", 1, 
                            ifelse(island == "Dream", 2, 
                                   ifelse(island == "Torgersen", 3, NA)))
  return(encoded_island)
}

encode_sex <- function(sex) {
  encoded_sex <- ifelse(sex == "female", 1, 
                         ifelse(sex == "male", 2, NA))
  return(encoded_sex)
}


# -

one_hot_encode <- function(factor_column) {
  encoded_column <- model.matrix(~ factor_column - 1)
  return(encoded_column)
}


imputed_data$trainData$island = one_hot_encode(imputed_data$trainData$island)
imputed_data$testData$island = one_hot_encode(imputed_data$testData$island)

imputed_data$trainData$sex = one_hot_encode(imputed_data$trainData$sex)
imputed_data$testData$sex = one_hot_encode(imputed_data$testData$sex)

# ## SHAP Model for every Label

# +
imputed_data$trainData$species_a = encode_species_onehot(imputed_data$trainData$species, "Adelie") #Chinstrap, Adelie, Gentoo
imputed_data$trainData$species_c = encode_species_onehot(imputed_data$trainData$species, "Chinstrap") #Chinstrap, Adelie, Gentoo
imputed_data$trainData$species_g = encode_species_onehot(imputed_data$trainData$species, "Gentoo") #Chinstrap, Adelie, Gentoo

imputed_data$testData$species_a = encode_species_onehot(imputed_data$testData$species, "Adelie") #Chinstrap, Adelie, Gentoo
imputed_data$testData$species_c = encode_species_onehot(imputed_data$testData$species, "Chinstrap") #Chinstrap, Adelie, Gentoo
imputed_data$testData$species_g = encode_species_onehot(imputed_data$testData$species, "Gentoo") #Chinstrap, Adelie, Gentoo

# +
# Define the model
xgb_model_a <- xgboost(data = as.matrix(imputed_data$trainData[,c(-1,-9, -10, -11)]), # Exclude species column
                     label = imputed_data$trainData$species_a,
                     nrounds = 10,
                     objective = "binary:logistic",
                     verbose = 0) # Suppress verbose output

xgb_model_c <- xgboost(data = as.matrix(imputed_data$trainData[,c(-1,-9, -10, -11)]), # Exclude species column
                     label = imputed_data$trainData$species_c,
                     nrounds = 10,
                     objective = "binary:logistic",
                     verbose = 0) # Suppress verbose output

xgb_model_g <- xgboost(data = as.matrix(imputed_data$trainData[,c(-1,-9, -10, -11)]), # Exclude species column
                     label = imputed_data$trainData$species_g,
                     nrounds = 10,
                     objective = "binary:logistic",
                     verbose = 0) # Suppress verbose output
# -

get_binary_acc <- function(predictions, labels) {
    # Convert predictions to binary (0 or 1)
    predictions <- ifelse(predictions > 0.5, 1, 0)
    
    # Create confusion matrix
    confusion_matrix <- table(predictions, labels)
    
    # Compute accuracy
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    return(accuracy)
}


# +
a.y_pred_train <- predict(xgb_model_a, as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]))
a.y_pred_test <- predict(xgb_model_a, as.matrix(imputed_data$testData[, c(-1,-9, -10, -11)]))
a.y_true_train = imputed_data$trainData[['species_a']]
a.y_true_test = imputed_data$testData[['species_a']]

c.y_pred_train <- predict(xgb_model_c, as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]))
c.y_pred_test <- predict(xgb_model_c, as.matrix(imputed_data$testData[, c(-1,-9, -10, -11)]))
c.y_true_train = imputed_data$trainData[['species_c']]
c.y_true_test = imputed_data$testData[['species_c']]

g.y_pred_train <- predict(xgb_model_g, as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]))
g.y_pred_test <- predict(xgb_model_g, as.matrix(imputed_data$testData[, c(-1,-9, -10, -11)]))
g.y_true_train = imputed_data$trainData[['species_g']]
g.y_true_test = imputed_data$testData[['species_g']]
# -

get_binary_acc(c.y_pred_train, c.y_true_train)
get_binary_acc(c.y_pred_test, c.y_true_test) #0.985074626865672


get_binary_acc(a.y_pred_train, a.y_true_train)
get_binary_acc(a.y_pred_test, a.y_true_test) #0.985074626865672


get_binary_acc(g.y_pred_train, g.y_true_train)
get_binary_acc(g.y_pred_test, g.y_true_test) #0.985074626865672


# +
## Calculate shap values
a.shap_result_species = shap.score.rank(xgb_model = xgb_model_a, 
                              X_train = as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]),
                              shap_approx = F
                              )

## Plot var importance based on SHAP
var_importance(a.shap_result_species, top_n=10)

# +
## Calculate shap values
c.shap_result_species = shap.score.rank(xgb_model_c, 
                              X_train = as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]),
                              shap_approx = F
                              )

## Plot var importance based on SHAP
var_importance(c.shap_result_species, top_n=10)

# +
## Calculate shap values
g.shap_result_species = shap.score.rank(xgb_model_g, 
                              X_train = as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]),
                              shap_approx = F
                              )

## Plot var importance based on SHAP
var_importance(g.shap_result_species, top_n=10)
# -

source("shap.R")

plot.shap.summary <- function(data_long, gglabs){
  x_bound <- max(abs(data_long$value))
  require('ggforce') # for `geom_sina`
  plot1 <- ggplot(data = data_long)+
    coord_flip() + 
    # sina plot: 
    geom_sina(aes(x = variable, y = value, color = stdfvalue)) +
    # print the mean absolute value: 
    geom_text(data = unique(data_long[, c("variable", "mean_value"), with = F]),
              aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_hline(yintercept = 0) + # the vertical line
    scale_y_continuous(limits = c(-x_bound, x_bound)) +
    # reverse the order of features
    scale_x_discrete(limits = rev(levels(data_long$variable)) 
    ) + 
    labs(y = "SHAP value (impact on model output)", x = "", color = "Feature value") + gglabs +
    # theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text=element_text(size=12) )
    
    # +theme(axis.text=element_text(size=12),
    #     axis.title=element_text(size=14,face="bold"))
    
  return(plot1)
}


a.ggtitle = labs(title = "Top 4 Important Features",
              subtitle = "species: Adelie",
              caption = "Data source: Penguin Dataset")
c.ggtitle = labs(title = "Top 4 Important Features",
              subtitle = "species: Chinstrap",
              caption = "Data source: Penguin Dataset")
g.ggtitle = labs(title = "Top 4 Important Features",
              subtitle = "species: Gentoo",
              caption = "Data source: Penguin Dataset")

colnames(a.shap_result_species$shap_score)

options(repr.plot.width = 8, repr.plot.height = 3) 

# +
## Prepare data for top N variables
a.shap_long_species = shap.prep(shap = a.shap_result_species,
                           X_train = imputed_data$trainData[, c(-1,-9, -10, -11)] , 
                           top_n = 4
                           )

## Plot shap overall metrics
plot.shap.summary(data_long = a.shap_long_species, gglabs = a.ggtitle)
# -


ggsave(
  "shap_adelie.jpg",
  plot = plot.shap.summary(data_long = a.shap_long_species, gglabs = a.ggtitle),
    width = 8, height = 3, dpi = 300,
)


# +
## Prepare data for top N variables
c.shap_long_species = shap.prep(shap = c.shap_result_species,
                           X_train = imputed_data$trainData[, c(-1,-9, -10, -11)] , 
                           top_n = 4
                           )

## Plot shap overall metrics
plot.shap.summary(data_long = c.shap_long_species, gglabs = c.ggtitle)
# -


ggsave(
  "shap_chinstrap.jpg",
  plot = plot.shap.summary(data_long = c.shap_long_species, gglabs = c.ggtitle),
    width = 8, height = 3, dpi = 300,
)


# +
## Prepare data for top N variables
g.shap_long_species = shap.prep(shap = g.shap_result_species,
                           X_train = imputed_data$trainData[, c(-1,-9, -10, -11)] , 
                           top_n = 4
                           )

## Plot shap overall metrics
plot.shap.summary(data_long = g.shap_long_species, gglabs = g.ggtitle)


# +
ggsave(
  "shap_gentoo.jpg",
  plot = plot.shap.summary(data_long = g.shap_long_species, gglabs = g.ggtitle),
    width = 8, height = 3, dpi = 300,

)

# -

# # SHAP on Multi-Class

# +
# # Define the model
# xgb_model <- xgboost(data = as.matrix(imputed_data$trainData[, -1]), # Exclude species column
#                      label = imputed_data$trainData$species,
#                      nrounds = 10,
#                      objective = "multi:softmax",
#                      num_class = 4, # Number of classes
#                      verbose = 0) # Suppress verbose output

# y_pred_train <- predict(xgb_model, as.matrix(imputed_data$trainData[, -1]))
# y_pred_test <- predict(xgb_model, as.matrix(imputed_data$testData[, -1]))

# y_true_train = encode_species(imputed_data$trainData[['species']])

# y_true_test = encode_species(imputed_data$testData[['species']])

# # Evaluate the model
# get_acc <- function(predictions, label){
#     confusion_matrix <- table(predictions, label)
#     accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)    
#     return(accuracy)
# }



# get_acc(y_pred_train, y_true_train)
# get_acc(y_pred_test, y_true_test) #0.985074626865672


# ## Calculate shap values
# shap_result_species = shap.score.rank(xgb_model = xgb_model, 
#                               X_train = as.matrix(imputed_data$trainData[, c(-1,-9, -10, -11)]),
#                               shap_approx = F
#                               )

# ## Plot var importance based on SHAP
# var_importance(shap_result_species, top_n=20)

# ## Prepare data for top N variables
# shap_long_species = shap.prep(shap = shap_result_species,
#                            X_train = imputed_data$trainData[, -1] , 
#                            top_n = 2
#                            )

# ## Plot shap overall metrics
# plot.shap.summary(data_long = shap_long_species)


# ## 

# xgb.plot.shap(data = as.matrix(imputed_data$trainData[, -1]), # input data
#               model = xgb_model, # xgboost model
#               features = names(shap_result_species$mean_shap_score[1:9]), # only top 10 var
#               n_col = 3, # layout option
#               plot_loess = T # add red line to plot
#               )


# -


