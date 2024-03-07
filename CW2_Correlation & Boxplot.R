# install.packages("palmerpenguins")
# install.packages("VIM")
# install.packages("Amelia")
# install.packages("mice")
# install.packages("GGally")
# install.packages("gridExtra")
# install.packages("tidyverse")
# install.packages("recipes")

install.packages("palmerpenguins")
install.packages("VIM")
install.packages("Amelia")
install.packages("mice")
install.packages("GGally")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("recipes")

library(palmerpenguins)
library(gridExtra)
library(dplyr)
library(GGally)
library(ggplot2)
library(tidyr)
library(VIM)
library(mice)
library(Amelia)
library(tidyverse)
library(recipes)

## see the penguins data
glimpse(penguins)
penguins <- palmerpenguins::penguins

## find the missing value
md.pattern(penguins)

# The results indicate that features species, island, and year have 
# no missing values, bill_length_mm, bill_depth_mm, flipper_length_mm and body_mass_g
# has two missing values respectively, while sex has eleven missing values.
# Overall, there are 19 missing values.

## simulate the data
# bill_length
bill_length_miss <- which(is.na(penguins$bill_length_mm) == TRUE)

indbill_lenth1 <- which(is.na(penguins$bill_length_mm) == FALSE & 
                         penguins$species == "Adelie" & penguins$island == "Torgersen"
                       & penguins$year == "2007")
prebill_length1 <- sample(indbill_lenth1, 1, replace = TRUE)

indbill_lenth2 <- which(is.na(penguins$bill_length_mm) == FALSE & 
                          penguins$species == "Gentoo" & penguins$island == "Biscoe"
                        & penguins$year == "2009")
prebill_length2 <- sample(indbill_lenth2, 1, replace = TRUE)

# bill_depth
bill_depth_miss <- which(is.na(penguins$bill_depth_mm) == TRUE)

indbill_depth1 <- which(is.na(penguins$bill_depth_mm) == FALSE & 
                          penguins$species == "Adelie" & penguins$island == "Torgersen"
                        & penguins$year == "2007")
prebill_depth1 <- sample(indbill_depth1, 1, replace = TRUE)

indbill_depth2 <- which(is.na(penguins$bill_depth_mm) == FALSE & 
                          penguins$species == "Gentoo" & penguins$island == "Biscoe"
                        & penguins$year == "2009")
prebill_depth2 <- sample(indbill_depth2, 1, replace = TRUE)

# flipper_length
flipper_length_miss <- which(is.na(penguins$flipper_length_mm) == TRUE)

indflipper_length1 <- which(is.na(penguins$flipper_length_mm) == FALSE & 
                          penguins$species == "Adelie" & penguins$island == "Torgersen"
                        & penguins$year == "2007")
preflipper_length1 <- sample(indflipper_length1, 1, replace = TRUE)

indflipper_length2 <- which(is.na(penguins$flipper_length_mm) == FALSE & 
                          penguins$species == "Gentoo" & penguins$island == "Biscoe"
                        & penguins$year == "2009")
preflipper_length2 <- sample(indflipper_length2, 1, replace = TRUE)

# body_mass
body_mass_miss <- which(is.na(penguins$body_mass_g) == TRUE)

indbody_mass1 <- which(is.na(penguins$body_mass_g) == FALSE & 
                              penguins$species == "Adelie" & penguins$island == "Torgersen"
                            & penguins$year == "2007")
prebody_mass1 <- sample(indbody_mass1, 1, replace = TRUE)

indbody_mass2 <- which(is.na(penguins$body_mass_g) == FALSE & 
                              penguins$species == "Gentoo" & penguins$island == "Biscoe"
                            & penguins$year == "2009")
prebody_mass2 <- sample(indbody_mass2, 1, replace = TRUE)

# sex
sex_miss <- which(is.na(penguins$sex) == TRUE)
penguins[c(4,9,10,11,12,48,179,219,257,269,272),]
indsex0 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Adelie" & penguins$island == "Torgersen"
                 & penguins$year == "2007" )
presex0 <- sample(indsex0, 1, replace = TRUE)

indsex1 <- which(is.na(penguins$sex) == FALSE & 
                  penguins$species == "Adelie" & penguins$island == "Torgersen"
                 & penguins$year == "2007" & penguins$bill_length_mm < 38 
                 & penguins$bill_depth_mm > 17 & penguins$flipper_length_mm < 200
                 & penguins$body_mass_g < 4000)
presex1 <- sample(indsex1, 3, replace = TRUE)

indsex2 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Adelie" & penguins$island == "Torgersen"
                 & penguins$year == "2007" & penguins$bill_length_mm > 38 
                 & penguins$bill_depth_mm > 17 & penguins$flipper_length_mm < 200
                 & penguins$body_mass_g > 4000)
presex2 <- sample(indsex2, 1, replace = TRUE)

indsex3 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Adelie" & penguins$island == "Dream"
                 & penguins$year == "2007" & penguins$bill_length_mm < 38 
                 & penguins$bill_depth_mm > 17 & penguins$flipper_length_mm < 200
                 & penguins$body_mass_g < 4000)
presex3 <- sample(indsex3, 1, replace = TRUE)

indsex4 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Gentoo" & penguins$island == "Biscoe"
                 & penguins$year == "2007" & penguins$bill_length_mm > 38 
                 & penguins$bill_depth_mm < 17 & penguins$flipper_length_mm > 200
                 & penguins$body_mass_g > 4000)
presex4 <- sample(indsex4, 1, replace = TRUE)

indsex5 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Gentoo" & penguins$island == "Biscoe"
                 & penguins$year == "2008" & penguins$bill_length_mm > 38 
                 & penguins$bill_depth_mm < 17 & penguins$flipper_length_mm > 200
                 & penguins$body_mass_g > 4000)
presex5 <- sample(indsex5, 1, replace = TRUE)

indsex6 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Gentoo" & penguins$island == "Biscoe"
                 & penguins$year == "2009" & penguins$bill_length_mm > 38 
                 & penguins$bill_depth_mm < 17 & penguins$flipper_length_mm > 200
                 & penguins$body_mass_g > 4000)
presex6 <- sample(indsex6, 2, replace = TRUE)

indsex7 <- which(is.na(penguins$sex) == FALSE & 
                   penguins$species == "Gentoo" & penguins$island == "Biscoe"
                 & penguins$year == "2009")
presex7 <- sample(indsex7, 1, replace = TRUE)

complete_penguins <- penguins
complete_penguins$bill_length_mm[c(4,272)] <- 
  complete_penguins$bill_length_mm[c(prebill_length1,prebill_length2)]
complete_penguins$bill_depth_mm[c(4,272)] <- 
  complete_penguins$bill_depth_mm[c(prebill_depth1,prebill_depth2)]
complete_penguins$flipper_length_mm[c(4,272)] <- 
  complete_penguins$flipper_length_mm[c(preflipper_length1,preflipper_length2)]
complete_penguins$body_mass_g[c(4,272)] <- 
  complete_penguins$body_mass_g[c(prebody_mass1,prebody_mass2)]
complete_penguins$sex[4] <- complete_penguins$sex[presex0]
complete_penguins$sex[c(9,11,12)] <- complete_penguins$sex[presex1]
complete_penguins$sex[10] <- complete_penguins$sex[presex2]
complete_penguins$sex[48] <- complete_penguins$sex[presex3]
complete_penguins$sex[179] <- complete_penguins$sex[presex4]
complete_penguins$sex[219] <- complete_penguins$sex[presex5]
complete_penguins$sex[c(257,269)] <- complete_penguins$sex[presex6]
complete_penguins$sex[272] <- complete_penguins$sex[presex7]

# Using the  hot deck imputation method to simulate the data. 
# Then analyse the complete data.

# Count penguins for each species / island
count(complete_penguins,species, island, .drop = FALSE)
# visualize
ggplot(complete_penguins, aes(x = island, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# Count penguins for each species / sex
count(complete_penguins,species, sex, .drop = FALSE)
# visualize
ggplot(complete_penguins, aes(x = sex, fill = species)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"), 
                    guide = FALSE) +
  theme_minimal() +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

# correlation between different body features for the whole data

cor_matrix <- cor(complete_penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")],use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "cyan4", mid = "white", high = "orange",
                       midpoint = 0, limits = c(-1,1),
                       name = "Correlation",
                       guide = guide_colorbar(direction = "vertical")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 4) +
  labs(title = "Correlation Heatmap of Penguin Body Features",
       x = "Body Features", y = "Body Features")

# If we just use the whole data to calculate correlations between body features we could see some
# negative relationship between them, for example, bill_depth vs all other features.
# But is it reflect the true relationships if we divide the whole data considering speices or other 
# classification variables.

# relationship between species and body features
species_feature=c('species',"bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")
ggpairs(complete_penguins[species_feature], 
        lower = list(continuous = "smooth", combo = "box"), 
        upper = list(continuous = "cor", combo = "box"), 
        diag = list(continuous = "densityDiag", discrete = "barDiag"), 
        mapping = aes(color = species))

# relationship between island and body features
island_feature=c('island',"bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")
ggpairs(complete_penguins[island_feature], 
        lower = list(continuous = "smooth", combo = "box"), 
        upper = list(continuous = "cor", combo = "box"), 
        diag = list(continuous = "densityDiag", discrete = "barDiag"), 
        mapping = aes(color = island))


# focus on species, select all Adelie penguins (island change body features?) 
data_adelie=complete_penguins[complete_penguins$species == 'Adelie',]
penguins_long <- pivot_longer(data_adelie, cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g), names_to = "body_feature", values_to = "measurement")
ggplot(penguins_long, aes(x = island, y = measurement, fill = island)) +
  geom_boxplot() +
  facet_wrap(~body_feature, scales = "free_y") +
  labs(title = "Boxplot of Adelie penguin body features by island")
# From plot we could see in different islands, the body features of the same species penguins didn't show
# the obvious differences, so maybe the environments of islands didn't influence those body features much,
# which means we can remove island in the following regression model or classification model.


# focus on sex(sex influence body features?)
ggplot(complete_penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"), 
                     na.translate = FALSE) +
  facet_wrap(~species)

ggplot(complete_penguins, aes(x = bill_length_mm,
                     y = bill_depth_mm)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"), 
                     na.translate = FALSE) +
  facet_wrap(~species)


p1=ggplot(complete_penguins, aes(x = species, y = flipper_length_mm, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of flipper_length by species and sex")

p2=ggplot(complete_penguins, aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of bill_length by species and sex")

p3=ggplot(complete_penguins, aes(x = species, y = bill_depth_mm, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of bill_depth by species and sex")

p4=ggplot(complete_penguins, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of body_mass by species and sex")

grid.arrange(p1, p2, p3, p4,ncol = 2)
# From this plot we could see, all body features perform differently on different sex,
# generally male penguins have larger values than female penguins in all three species.
# And normally different species performs differently in all body features. Therefore,
# sex could be a informative variables when do classifying. Because male penguins and female penguins should
# have different standards because of their original nature body differences.


p1=ggplot(complete_penguins, aes(x = species, y = flipper_length_mm, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Boxplot of flipper_length by species and year")

p2=ggplot(complete_penguins, aes(x = species, y = bill_length_mm, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Boxplot of bill_length by species and year")

p3=ggplot(complete_penguins, aes(x = species, y = bill_depth_mm, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Boxplot of bill_depth by species and year")

p4=ggplot(complete_penguins, aes(x = species, y = body_mass_g, fill = as.factor(year))) +
  geom_boxplot() +
  labs(title = "Boxplot of body_mass by species and year")

grid.arrange(p1, p2, p3, p4,ncol = 2)

# In different species, body features have some slight differences if the penguins egg coming from
# different years. Whether this characteristic should be consider when modelling depends on 
# the model fit results.


######PCA
penguins_pcadata <- select(complete_penguins, -c(species, island, sex, year))
pca_result <- prcomp(penguins_pcadata, scale = TRUE)
summary(pca_result)

pca_result_df <- as.data.frame(pca_result$x)

rotation_matrix <- pca_result$rotation

loadings_df <- data.frame(
  variable = rownames(rotation_matrix),
  PC1 = rotation_matrix[, 1],
  PC2 = rotation_matrix[, 2],
  PC3 = rotation_matrix[, 3],
  PC4 = rotation_matrix[, 4]
)

# biplot graph
pca_result_df$species <- complete_penguins$species

ggplot(data = pca_result_df, aes(x = PC1, y = PC2,color = species)) +
  geom_point() +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.1, "inches")), color = "red") + 
  geom_text(data = loadings_df, aes(label = variable, x = PC1, y = PC2), 
            hjust = -0.2, vjust = -0.2, size = 3, color = "red") +  
  labs(x = "PC1", y = "PC2") +  
  ggtitle("Biplot") +  
  theme_minimal()

# From the graph, we can see the flipper and the body mass have high contribution to PC1.


# # XGBoost

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
# -


