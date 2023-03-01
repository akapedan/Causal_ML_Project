# Course: Methods for Data-Driven Optimal Policy
# University of St. Gallen.
# Last tested: 23/06/2022
# Run time on Apple M1: 40s
# R Version: 4.0.3
#########################################################################

#########################################################################
## Setup 
#########################################################################
# Memory cleaning
rm(list=ls()) 

# Load packages
library(dplyr)       # Data manipulation (0.8.0.1)
library(fBasics)     # Summary statistics (3042.89)
library(corrplot)    # Correlations (0.84)
library(psych)       # Correlation p-values (1.8.12)
library(grf)         # Generalized random forests (0.10.2)
library(rpart)       # Classification and regression trees, or CART (4.1-13)
library(rpart.plot)  # Plotting trees (3.0.6)
library(treeClust)   # Predicting leaf position for causal trees (1.1-7)
library(car)         # linear hypothesis testing for causal tree (3.0-2)
library(remotes)     # Install packages from github (2.0.1)
library(readr)       # Reading csv files (1.3.1)
library(tidyr)       # Database operations (0.8.3)
library(tibble)      # Modern alternative to data frames (2.1.1)
library(knitr)       # RMarkdown (1.21) if you decide to create a Markdown
library(kableExtra)  # Prettier RMarkdown (1.0.1)
library(ggplot2)     # general plotting tool (3.1.0)
library(haven)       # read stata files (2.0.0)
library(aod)         # hypothesis testing (1.3.1)
library(evtree)      # evolutionary learning of globally optimal trees (1.0-7)
library(estimatr)    # simple interface for OLS estimation w/ robust std errors ()
library(stargazer)   # LaTeX output
library(plyr)        # Tools for Splitting, Applying and Combining Data
library(formattable) # Nice tables 
library(gridExtra)   # Grid graphic objects
library(factoextra)  # Extract and Visualize the Results of Multivariate Data Analyses

# Install from Github
#remotes::install_github('susanathey/causalTree') # Uncomment to install
library(causalTree)
#remotes::install_github('grf-labs/sufrep') # Uncomment to install
library(sufrep)

# Working directory
setwd("YOUR WORKING DIRECTORY")

# Load data
load("softwaremarketing.Rdata")

# Set seed
set.seed(18062022)


#########################################################################
## Data Preparation
#########################################################################
data_treated_tech=df[df$Tech.Support==1,]
data_control_tech=df[df$Tech.Support==0,]

data_treated_disc=df[df$Discount==1,]
data_control_disc=df[df$Discount==0,]

df$Tech.Support.and.Discount <- ifelse((df$Tech.Support %in% 1 & df$Discount %in% 1), 1, 0)

data_treated_both <- df[(df$Tech.Support %in% 1 & df$Discount %in% 1),]
data_treated_both <- data_treated_both[,-10]
data_treated_both %>% 
  rename(
    Tech.Support.and.Discount = Tech.Support)
data_control_both <- df[!(df$Tech.Support %in% 1 & df$Discount %in% 1),]
data_control_both <- data_control_both[,-(9:10)]
data_control_both$Tech.Support.and.Discount <- rep(0, nrow(data_control_both))

df = df[df$Revenue > 0,] # remove implausible value

df=na.omit(df)

# Filter important variables
colnames(df)

covariate_names1 <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size",           
                      "Discount") 
covariate_names2 <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size",           
                      "Tech.Support") 
covariate_names3 <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size")

# robustness test by removing IT.spend
#covariate_names2 <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","Employee.Count","PC.Count","Size","Tech.Support")    

# Train and test set
train_fraction <- 0.75  # Use train_fraction % of the dataset to train our models
n <- dim(df)[1]
train_idx <- sample.int(n, replace=F, size=floor(n*train_fraction))
df_train <- df[train_idx,]
df_test <- df[-train_idx,]

# 1) Divide the data into splitting sample (to build the tree) and estimation
# sample (to estimate the TE) = HONESTY PRINCIPLE
split_size <- floor(nrow(df_train)*0.5)
split_idx <- sample.int(nrow(df_train), replace=F, size=split_size)

df_split <- df_train[split_idx,]
df_est <- df_train[-split_idx,]

#########################################################################
## Descriptive Statistics
#########################################################################

# Make a data.frame containing summary statistics of interest
summ_stats <- fBasics::basicStats(df)
summ_stats <- as.data.frame(t(summ_stats))

# Rename some of the columns for convenience
summ_stats <- summ_stats[c("Mean", "Stdev", "Minimum", "1. Quartile", "Median",  "3. Quartile", "Maximum")] %>% 
  rename("Lower quartile" = '1. Quartile', "Upper quartile"= "3. Quartile")

summ_stats

stargazer(df)

# Inspect structure of df
str(df)

# Scatterplots
options(scipen=10000)

colNames <- names(df)[5:8]
plot_lst <- list()
for(i in colNames){
  plt <- ggplot(df, aes_string(x=i, y = "Revenue")) +
    geom_point(size = 1) +
    xlab(i) + ylab("Revenue") +
    theme_minimal()
  plot_lst[[i]] <- plt
}
sc <- cowplot::plot_grid(plotlist = plot_lst, nrow = 2, ncol = 2)

title <- cowplot::ggdraw() + cowplot::draw_label("Scatterplots", fontface='bold', size = 18)

cowplot::plot_grid(title, sc, ncol=1, rel_heights=c(0.1, 1))

# Histograms
colNames <- names(df)[c(5:8,11)]
plot_lst <- list()
for(i in colNames){
  plt <- ggplot(df, aes_string(x=i)) +
    geom_histogram(aes(y=..count../sum(..count..))) + 
    scale_y_continuous(labels = scales::percent_format()) +
    xlab(i) + ylab("Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Histogram of ",i)) +
    theme_minimal()
  plot_lst[[i]] <- plt
}
cowplot::plot_grid(plotlist = plot_lst, nrow = 2, ncol = 3)

# Correlation matrix
sds <- df
sds[] <- lapply(sds,as.numeric)


cormatweek <- round(cor(sds, method = "spearman"),2)

# Get upper triangle of the correlation matrix
get_upper_tri_week <- function(cormatweek){
  cormatweek[lower.tri(cormatweek)]<- NA
  return(cormatweek)
}

upper_tri_week <- get_upper_tri_week(cormatweek)
upper_tri_week
melted_cormat_week <- melt(upper_tri_week, na.rm = TRUE)

ggheatmap <- ggplot(data = melted_cormat_week, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  theme(axis.text.y = element_text(vjust = 1, 
                                   size = 8, hjust = 1))
# add numbers
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (18), hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.75),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  ggtitle("Correlation Matrix")

# rapid check for ATE given randomization
mean(data_treated_tech$Revenue)- mean(data_control_tech$Revenue)
mean(data_treated_disc$Revenue)- mean(data_control_disc$Revenue)
mean(data_treated_both$Revenue)- mean(data_control_both$Revenue)

# Note: Checking split based on descriptive statistics should be done
summary(df_split)
stargazer(df_split)
summary(df_est)
stargazer(df_est)

# Histograms
colNames <- names(df)[c(5:8,11)]
plot_lst <- list()
for(i in colNames){
  plt <- ggplot(df, aes_string(x=i)) +
    geom_histogram(data=df_split, aes(y=..count../sum(..count..)), fill="#F8766D", alpha = 0.3) +
    geom_histogram(data=df_est, aes(y=..count../sum(..count..)), fill="#00BFC4", alpha = 0.3) + 
    xlab(i) + ylab("Frequency") +
    ggtitle(paste0("Histogram of ",i)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot_lst[[i]] <- plt
}

# Get Legend
legend <- ggplot(df, aes_string(x="Size")) +
  geom_histogram(data=df_split, aes(y=..count../sum(..count..), fill="#F8766D"), alpha = 0.3) +
  geom_histogram(data=df_est, aes(y=..count../sum(..count..), fill="#00BFC4"), alpha = 0.3) + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_identity(name = 'Legend', 
                      guide = 'legend',
                      labels = c('Split','Estimation')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

legend <- cowplot::get_legend(legend)

cowplot::plot_grid(plotlist = plot_lst, legend, nrow = 2, ncol = 3)

#########################################################################
## Causal tree
#########################################################################

# 2.1) Build the tree for treatment = Tech.Support
# Create a formula linking y with the covariates
fmla_ct1 <- paste("Revenue ~", paste(covariate_names1, collapse = " + "))

set.seed(18062022)
# Call the tree command
ct_unpruned <- honest.causalTree(
  formula=fmla_ct1,            # Define the model
  data=df_split,              # Subset used to create tree structure
  est_data=df_est,            # Which data set to use to estimate effects
  
  treatment=df_split$Tech.Support,       # Splitting sample treatment variable
  est_treatment=df_est$Tech.Support,     # Estimation sample treatment variable
  
  split.Rule="CT",            # Define the splitting option
  cv.option="TOT",            # Cross validation options
  cp=0,                       # Complexity parameter
  
  split.Honest=TRUE,          # Use honesty when splitting
  cv.Honest=TRUE,             # Use honesty when performing cross-validation
  
  minsize=25,                 # Min. number of treatment and control cases in each leaf
  HonestSampleSize=nrow(df_est)) # Num obs used in estimation after building the tree

## plot it
rpart.plot(
  x=ct_unpruned,
  type = 4,
  digits=-1,
  box.palette = "GnYlRd",
  main = "Causal Tree for Tech Support as Treatment (unpruned)")

# 3) CV to prune the tree
# Table with the CV results
ct_cptable <- as.data.frame(ct_unpruned$cptable)

# Obtain the optimal cp
selected_cp <- which.min(ct_cptable$xerror)
optim_cp_ct <- ct_cptable[selected_cp,"CP"]

# Prune tree
ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)

## plot it
rpart.plot(
  x=ct_pruned,
  type = 4,
  digits=-1,
  box.palette = "GnYlRd",
  main = "Causal Tree for Tech Support as Treatment (pruned)")

#printing rules
rpart.rules(ct_pruned, cover = TRUE)

# 4) Predict point estimates (on the estimation sample)
tauhat_ct_est <- predict(ct_pruned, newdata = df_est)

# 5) Compute the std. errors
# Create a factor column 'leaf' indicating leaf assignment
num_leaves <- length(unique(tauhat_ct_est))
num_leaves <- ct_cptable[selected_cp, "nsplit"] + 1
df_est$leaf <- factor(tauhat_ct_est, labels = seq(num_leaves))

# Run regression
ols_ct <- lm_robust(Revenue ~ 0 + leaf + Tech.Support:leaf, data = df_est)
ols_ct_summary <- summary(ols_ct)

te_summary <- coef(ols_ct_summary)[(num_leaves+1):(2*num_leaves),
                                   c("Estimate", "Std. Error")]

# 6) Predict point estimates for (test set)
tauhat_ct_test <- predict(ct_pruned, newdata = df_test)

# 7) Covariate heterogeneity
# Regress each covariate on leaf assignment to means p
cov_names <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size", "Discount") 
cov_means <- lapply(cov_names, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + leaf')), data = df_est)
})

# Extract the mean and standard deviation of each covariate per leaf
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df <- round(cov_table_df, digits = 2)

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Discount", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1")

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))
#########################################################################
# 2.2) Build the tree for treatment= Discount
#########################################################################
# Create a formula linking y with the covariates
fmla_ct2 <- paste("Revenue ~", paste(covariate_names2, collapse = " + "))
set.seed(18062022)
# Call the tree command
ct_unpruned <- honest.causalTree(
  formula=fmla_ct2,            # Define the model
  data=df_split,              # Subset used to create tree structure
  est_data=df_est,            # Which data set to use to estimate effects
  
  treatment=df_split$Discount,       # Splitting sample treatment variable
  est_treatment=df_est$Discount,     # Estimation sample treatment variable
  
  split.Rule="CT",            # Define the splitting option
  cv.option="TOT",            # Cross validation options
  cp=0,                       # Complexity parameter
  
  split.Honest=TRUE,          # Use honesty when splitting
  cv.Honest=TRUE,             # Use honesty when performing cross-validation
  
  minsize=25,                 # Min. number of treatment and control cases in each leaf
  HonestSampleSize=nrow(df_est)) # Num obs used in estimation after building the tree

## plot it
rpart.plot(
  x=ct_unpruned,
  type = 4,
  digits=-3,
  box.palette = "GnYlRd",
  main = "Causal Tree for Discount as Treatment (Unpruned)")

# 3) CV to prune the tree
# Table with the CV results
ct_cptable <- as.data.frame(ct_unpruned$cptable)

# Obtain the optimal cp
selected_cp <- which.min(ct_cptable$xerror)
optim_cp_ct <- ct_cptable[selected_cp,"CP"]

# Prune tree
ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)

#plot it
rpart.plot(
  x=ct_pruned,
  type = 4,
  digits=-3,
  box.palette = "GnYlRd",
  main = "Causal Tree for Discount as Treatment (Pruned)")

#printing rules
rpart.rules(ct_pruned, cover = TRUE)

# 4) Predict point estimates (on the estimation sample)
tauhat_ct_est <- predict(ct_pruned, newdata = df_est)

# 5) Compute the std. errors
# Create a factor column 'leaf' indicating leaf assignment
num_leaves <- length(unique(tauhat_ct_est))
num_leaves <- ct_cptable[selected_cp, "nsplit"] + 1
df_est$leaf <- factor(tauhat_ct_est, labels = seq(num_leaves))

# Run regression
ols_ct <- lm_robust(Revenue ~ 0 + leaf + Discount:leaf, data = df_est)
ols_ct_summary <- summary(ols_ct)

te_summary <- coef(ols_ct_summary)[(num_leaves+1):(2*num_leaves),
                                   c("Estimate", "Std. Error")]

te_summary

# 6) Predict point estimates for (test set)
tauhat_ct_test <- predict(ct_pruned, newdata = df_test)

# 7) Covariate heterogeneity
# Regress each covariate on leaf assignment to means p
cov_names <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size","Tech.Support") 
cov_means <- lapply(cov_names, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + leaf')), data = df_est)
})

# Extract the mean and standard deviation of each covariate per leaf
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df <- round(cov_table_df, digits = 2)

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Tech.Support", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1") 

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))
#########################################################################
# 2.3) Build the tree for treatment = Tech.Support & Discount
#########################################################################
# Create a formula linking y with the covariates
fmla_ct3 <- paste("Revenue ~", paste(covariate_names3, collapse = " + "))
set.seed(18062022)
# Call the tree command
ct_unpruned <- honest.causalTree(
  formula=fmla_ct3,            # Define the model
  data=df_split,              # Subset used to create tree structure
  est_data=df_est,            # Which data set to use to estimate effects
  
  treatment=df_split$Tech.Support.and.Discount,       # Splitting sample treatment variable
  est_treatment=df_est$Tech.Support.and.Discount,     # Estimation sample treatment variable
  
  split.Rule="CT",            # Define the splitting option
  cv.option="TOT",            # Cross validation options
  cp=0,                       # Complexity parameter
  
  split.Honest=TRUE,          # Use honesty when splitting
  cv.Honest=TRUE,             # Use honesty when performing cross-validation
  
  minsize=25,                 # Min. number of treatment and control cases in each leaf
  HonestSampleSize=nrow(df_est)) # Num obs used in estimation after building the tree

## plot it
rpart.plot(
  x=ct_unpruned,
  type = 4,
  digits=-1,
  box.palette = "GnYlRd",
  main = "Causal Tree for Tech Support and Discount as Treatment (unpruned)")

# 3) CV to prune the tree
# Table with the CV results
ct_cptable <- as.data.frame(ct_unpruned$cptable)

# Obtain the optimal cp
selected_cp <- which.min(ct_cptable$xerror)
optim_cp_ct <- ct_cptable[selected_cp,"CP"]

# Prune tree
ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)

## plot it
rpart.plot(
  x=ct_pruned,
  type = 4,
  digits=-1,
  box.palette = "GnYlRd",
  main = "Causal Tree for Tech Support and Discount as Treatment (pruned)")

#printing rules
rpart.rules(ct_pruned, cover = TRUE)

# 4) Predict point estimates (on the estimation sample)
tauhat_ct_est <- predict(ct_pruned, newdata = df_est)

# 5) Compute the std. errors
# Create a factor column 'leaf' indicating leaf assignment
num_leaves <- length(unique(tauhat_ct_est))
num_leaves <- ct_cptable[selected_cp, "nsplit"] + 1
df_est$leaf <- factor(tauhat_ct_est, labels = seq(num_leaves))

# Run regression
ols_ct <- lm_robust(Revenue ~ 0 + leaf + Tech.Support:leaf, data = df_est)
ols_ct_summary <- summary(ols_ct)

te_summary <- coef(ols_ct_summary)[(num_leaves+1):(2*num_leaves),
                                   c("Estimate", "Std. Error")]

# 6) Predict point estimates for (test set)
tauhat_ct_test <- predict(ct_pruned, newdata = df_test)

# 7) Covariate heterogeneity
# Regress each covariate on leaf assignment to means p
cov_names <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size", "Tech.Support", "Discount") 
cov_means <- lapply(cov_names, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + leaf')), data = df_est)
})

# Extract the mean and standard deviation of each covariate per leaf
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df <- round(cov_table_df, digits = 2)

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Tech.Support", "", "Discount", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1") 

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))

#########################################################################
# CAUSAL FOREST
#########################################################################

# 1.1) Build the forest with treatment = tech.support
cf <- causal_forest(
  X = as.matrix(df_train[,covariate_names1]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support,
  num.trees = nrow(df_train[,covariate_names1])) # ! This is only for speed. Real analysis -> more

## 1.2) Build forest with treatment = discount
cf2 <- causal_forest(
  X = as.matrix(df_train[,covariate_names2]),
  Y = df_train$Revenue,
  W = df_train$Discount,
  num.trees = nrow(df_train[,covariate_names2]))

## 1.3) Build forest with treatment = tech.support & discount
cf3 <- causal_forest(
  X = as.matrix(df_train[,covariate_names3]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support.and.Discount,
  num.trees = nrow(df_train[,covariate_names3]))

# 2) Check the nuisance parameters
# if e.bar coeff = 1 -> average prediction correct
# if e.residual coeff = 1 -> heterogeneity (if any) correct

# 2.1) Propensity score, Model 1
ggplot(data.frame(W.hat = cf$W.hat, W = factor(cf$W.orig))) +
  geom_histogram(aes(x=W.hat, y=stat(density), fill= W), alpha = 0.3,
                 position = "identity") +
  geom_density(aes(x=W.hat, color=W)) +
  theme_bw() +
  xlim(0,1) +
  labs(title = "Causal Forest Propensity Scores (W=Tech Support)")

# Extract data
DF <- data.frame(
  W          = df_train$Tech.Support,
  e.bar      = mean(cf$W.hat),
  e.residual = cf$W.hat - mean(cf$W.hat)
)

# Regression
best.linear.predictor <- lm(W ~ e.bar + e.residual + 0, data = DF)
blp.summary.outcome <- lmtest::coeftest(best.linear.predictor,
                                        vcov = sandwich::vcovCL,
                                        type = "HC3")
blp.summary.outcome
stargazer(blp.summary.outcome)

# Response function check
# Extract data
DF <- data.frame(
  Y          = df_train$Revenue,
  m.bar      = mean(cf$Y.hat),
  m.residual = cf$Y.hat - mean(cf$Y.hat)
)

# Regression
best.linear.predictor <- lm(Y ~ m.bar + m.residual + 0, data = DF)
blp.summary.prop <- lmtest::coeftest(best.linear.predictor,
                                     vcov = sandwich::vcovCL,
                                     type = "HC3")
blp.summary.prop
stargazer(blp.summary.prop)

# 2.2) Propensity score, Model 2
ggplot(data.frame(W.hat = cf2$W.hat, W = factor(cf2$W.orig))) +
  geom_histogram(aes(x=W.hat, y=stat(density), fill= W), alpha = 0.3,
                 position = "identity") +
  geom_density(aes(x=W.hat, color=W)) +
  theme_bw() + 
  xlim(0,1) +
  labs(title = "Causal Forest Propensity Scores (W=Discount)")

DF2 <- data.frame(
  W          = df_train$Tech.Support,
  e.bar      = mean(cf2$W.hat),
  e.residual = cf2$W.hat - mean(cf2$W.hat)
)

# Regression
best.linear.predictor <- lm(W ~ e.bar + e.residual + 0, data = DF2)
blp.summary.outcome2 <- lmtest::coeftest(best.linear.predictor,
                                         vcov = sandwich::vcovCL,
                                         type = "HC3")
blp.summary.outcome2
stargazer(blp.summary.outcome2)

# Response function check
# Extract data
DF2 <- data.frame(
  Y          = df_train$Revenue,
  m.bar      = mean(cf2$Y.hat),
  m.residual = cf2$Y.hat - mean(cf2$Y.hat)
)

# Regression
best.linear.predictor <- lm(Y ~ m.bar + m.residual + 0, data = DF2)
blp.summary.prop2 <- lmtest::coeftest(best.linear.predictor,
                                      vcov = sandwich::vcovCL,
                                      type = "HC3")
blp.summary.prop2
stargazer(blp.summary.prop2)

# 2.3) Propensity score, Model 3
ggplot(data.frame(W.hat = cf3$W.hat, W = factor(cf3$W.orig))) +
  geom_histogram(aes(x=W.hat, y=stat(density), fill= W), alpha = 0.3,
                 position = "identity") +
  geom_density(aes(x=W.hat, color=W)) +
  theme_bw() +
  xlim(0,1) +
  labs(title = "Causal Forest Propensity Scores (W=Tech Support & Discount)")

# Extract data
DF3 <- data.frame(
  W          = df_train$Tech.Support,
  e.bar      = mean(cf3$W.hat),
  e.residual = cf3$W.hat - mean(cf3$W.hat)
)

# Regression
best.linear.predictor <- lm(W ~ e.bar + e.residual + 0, data = DF3)
blp.summary.outcome3 <- lmtest::coeftest(best.linear.predictor,
                                         vcov = sandwich::vcovCL,
                                         type = "HC3")
blp.summary.outcome3
stargazer(blp.summary.outcome3)

# Response function check
# Extract data
DF3 <- data.frame(
  Y          = df_train$Revenue,
  m.bar      = mean(cf$Y.hat),
  m.residual = cf3$Y.hat - mean(cf3$Y.hat)
)

# Regression
best.linear.predictor <- lm(Y ~ m.bar + m.residual + 0, data = DF3)
blp.summary.prop3 <- lmtest::coeftest(best.linear.predictor,
                                      vcov = sandwich::vcovCL,
                                      type = "HC3")
blp.summary.prop3
stargazer(blp.summary.prop3)

# Calculate ATE
# Model 1
ATE = average_treatment_effect(cf)
paste("95% CI for the ATE:", round(ATE[1], 3),
      "+/-", round(qnorm(0.975) * ATE[2], 3))

# Model 2
ATE2 = average_treatment_effect(cf2)
paste("95% CI for the ATE:", round(ATE2[1], 3),
      "+/-", round(qnorm(0.975) * ATE2[2], 3))

# Model 3
ATE3 = average_treatment_effect(cf3)
paste("95% CI for the ATE:", round(ATE3[1], 3),
      "+/-", round(qnorm(0.975) * ATE3[2], 3))

# Omnibus Test
OmniTest <- test_calibration(cf)
stargazer(OmniTest)
OmniTest2 <- test_calibration(cf2)
stargazer(OmniTest2)
OmniTest3 <- test_calibration(cf3)
stargazer(OmniTest3)

# 3) Predict point estimates (training, out-of-bag)
# 3.1) Model 1
oob_pred <- predict(cf, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf <- oob_pred$predictions
oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)

# 3.2) Model 2
oob_pred <- predict(cf2, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf2 <- oob_pred$predictions
oob_tauhat_cf_se2 <- sqrt(oob_pred$variance.estimates)

# 3.3) Model 3
oob_pred <- predict(cf3, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf3 <- oob_pred$predictions
oob_tauhat_cf_se3 <- sqrt(oob_pred$variance.estimates)

# 4) Predict point estimates (test)
# 4.1) Model 1
test_pred <- predict(cf, newdata=as.matrix(df_test[covariate_names1]), estimate.variance=TRUE)
tauhat_cf_test <- test_pred$predictions
tauhat_cf_test_se <- sqrt(test_pred$variance.estimates)

# 4.2) Model 2
test_pred <- predict(cf2, newdata=as.matrix(df_test[covariate_names2]), estimate.variance=TRUE)
tauhat_cf_test <- test_pred$predictions
tauhat_cf_test_se <- sqrt(test_pred$variance.estimates)

# 4.3) Model 3
test_pred <- predict(cf3, newdata=as.matrix(df_test[covariate_names3]), estimate.variance=TRUE)
tauhat_cf_test <- test_pred$predictions
tauhat_cf_test_se <- sqrt(test_pred$variance.estimates)

# ASSESS THE HETEROGENEITY
# 1.1) Histogram of CATEs, Model 1
ggplot(as.data.frame(oob_tauhat_cf), aes(x=oob_tauhat_cf)) +
  geom_histogram(aes(x = oob_tauhat_cf),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Tech Support)")

# 1.2) Histogram of CATEs, Model 2
ggplot(as.data.frame(oob_tauhat_cf2), aes(x=oob_tauhat_cf2)) +
  geom_histogram(aes(x = oob_tauhat_cf2),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Discount)")

# 1.3) Histogram of CATEs, Model 3
ggplot(as.data.frame(oob_tauhat_cf3), aes(x=oob_tauhat_cf3)) +
  geom_histogram(aes(x = oob_tauhat_cf3),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Tech Support & Discount)") 

# 2.1) Variable Importance, Model 1
var_imp <- c(variable_importance(cf))
names(var_imp) <- covariate_names1
sorted_var_imp <- sort(var_imp, decreasing=TRUE)
sorted_var_imp_table <- formattable(as.data.frame(sorted_var_imp))
sorted_var_imp_table
selected.idx = which(var_imp > mean(var_imp))

# 2.2) Variable Importance, Model 2
var_imp2 <- c(variable_importance(cf2))
names(var_imp2) <- covariate_names2
sorted_var_imp2 <- sort(var_imp2, decreasing=TRUE)
sorted_var_imp_table2 <- formattable(as.data.frame(sorted_var_imp2))
sorted_var_imp_table2
selected.idx2 = which(var_imp2 > mean(var_imp2))

# 2.3) Variable Importance, Model 3
var_imp3 <- c(variable_importance(cf3))
names(var_imp3) <- covariate_names3
sorted_var_imp3 <- sort(var_imp3, decreasing=TRUE)
sorted_var_imp_table3 <- formattable(as.data.frame(sorted_var_imp3))
sorted_var_imp_table3
selected.idx3 = which(var_imp3 > mean(var_imp3))

# 3) Heterogeneity across subgroups
# 3.1) Model 1
# Manually creating subgroups
num_tiles <- 4  # ntiles = CATE is above / below the median
df_train$cate <- oob_tauhat_cf
df_train$ntile <- factor(ntile(oob_tauhat_cf, n=num_tiles))

# Estimating the average effect in the subgroups
# Sample ATE
ols_sample_ate <- lm_robust(Revenue ~ ntile + ntile:Tech.Support, data=df_train)
estimated_sample_ate <- coef(summary(ols_sample_ate))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
estimated_sample_ate <- data.frame(estimated_sample_ate)
hypothesis_sample_ate <- paste0("ntile1:Tech.Support = ", paste0("ntile", seq(2, num_tiles), ":Tech.Support"))
ftest_pvalue_sample_ate <- linearHypothesis(ols_sample_ate, hypothesis_sample_ate, test="F")[2,"Pr(>F)"]

# AIPW
estimated_aipw_ate <- lapply(seq(num_tiles), function(w) {
  ate <- average_treatment_effect(cf, subset = df_train$ntile == w)
})
estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))

# Testing for equality using Wald test
## define L matrix that allows us to test if the ATEs in ntile 2+ are equal to ntile 1
.L <- cbind(-1, diag(num_tiles - 1))
# e.g. [,1] [,2] [,3] [,4]
# [1,]   -1    1    0    0
# [2,]   -1    0    1    0
# [3,]   -1    0    0    1
waldtest_pvalue_aipw_ate <- wald.test(Sigma = diag(estimated_aipw_ate$std.err^2),
                                      b = estimated_aipw_ate$estimate,
                                      L = .L)$result$chi2[3]

# Make a nice table
table.df <- data.frame(Tile  = 1:4,
                       Sample.ATE = round(estimated_sample_ate$Estimate),
                       AIPW.ATE = round(estimated_aipw_ate$estimate)
)
na.df <- data.frame(Tile = "", Sample.ATE = NA, AIPW.ATE = NA)

table.df <- do.call(rbind, apply(table.df, 1, function(x) {rbind(x, na.df)}))

stat.test <- c("", paste("F-Test p-value:", round(ftest_pvalue_sample_ate)), paste("Wald-Test p-value:", round(waldtest_pvalue_aipw_ate)))

table.df <- rbind(table.df,stat.test)

table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),2] <- round(estimated_sample_ate$Std..Error)
table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),3] <- round(estimated_aipw_ate$std.err)

table.df[seq(2,nrow(table.df),2),2:3] <- paste0("(", format(unlist(table.df[seq(2,nrow(table.df),2),2:3])),")")

table.df1 <- formattable(table.df)
table.df1

# Print SATE and AIPW to compare:
box.df <- data.frame (N.tile = rep(1:4,2),
                      ATE.estimate = c(estimated_sample_ate$Estimate,estimated_aipw_ate$estimate),
                      Method = c(rep("Sample ATE", each=4), rep("AIPW ATE", each=4)),
                      se = c(estimated_sample_ate$Std..Error,estimated_aipw_ate$std.err)
)

ggplot(box.df, aes(x = N.tile, y = ATE.estimate, col = Method)) +
  geom_point(position = position_dodge(.3)) +
  geom_errorbar(aes(ymin = ATE.estimate-2.*se, ymax = ATE.estimate+2.*se), position = position_dodge(.3), width=.2) +
  theme(legend.position = 'none') +
  theme_bw() + ggtitle("ATE within N-tiles (as defined by predicted CATE)")

# Test statistical significance (different TEs)
colnames(estimated_sample_ate)[2]<-"SE"
colnames(estimated_aipw_ate) <-colnames(estimated_sample_ate)

p_values_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
differences_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                   dimnames = list(c(1:num_tiles),c(1:num_tiles)))
stderror_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
hypotheses_grid <- combn(1:num_tiles, 2)

estimated_aipw_ate <- mutate(estimated_aipw_ate, Ntile = c(1:num_tiles))

invisible(apply(hypotheses_grid, 2, function(x) {
  .diff <- with(estimated_aipw_ate, Estimate[Ntile == x[2]] - Estimate[Ntile == x[1]])
  .se <- with(estimated_aipw_ate, sqrt(SE[Ntile == x[2]]^2 + SE[Ntile == x[1]]^2))
  
  label1 <- paste0("tile", x[1])
  label2 <- paste0("tile", x[2])
  
  differences_tile_by_tile[x[2], x[1]] <<- .diff
  rownames(differences_tile_by_tile)[x[2]] <<- label2
  colnames(differences_tile_by_tile)[x[2]] <<- label2
  rownames(differences_tile_by_tile)[x[1]] <<- label1
  colnames(differences_tile_by_tile)[x[1]] <<- label1
  
  stderror_tile_by_tile[x[2], x[1]] <<- .se
  rownames(stderror_tile_by_tile)[x[2]] <<- label2
  colnames(stderror_tile_by_tile)[x[2]] <<- label2
  rownames(stderror_tile_by_tile)[x[1]] <<- label1
  colnames(stderror_tile_by_tile)[x[1]] <<- label1
  
  p_values_tile_by_tile[x[2], x[1]] <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
  rownames(p_values_tile_by_tile)[x[2]] <<- label2
  colnames(p_values_tile_by_tile)[x[2]] <<- label2
  rownames(p_values_tile_by_tile)[x[1]] <<- label1
  colnames(p_values_tile_by_tile)[x[1]] <<- label1
  
}))

p_values_tile_by_tile = round(p_values_tile_by_tile, digits = 2)
# Bonferroni correction should be applied to decide about the rejection

h1 <- ggplot(df_train, aes(x=cate)) +
  geom_histogram(aes(fill=factor(ntile)), bins = 1000) +
  ggtitle("Quartiles") +
  theme_bw()

h1

# Clustering
# Elbow method
fviz_nbclust(as.data.frame(oob_tauhat_cf), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# 4-5 seem to be optimal -> flattening afterwards

# See the results for k=4
clusters <- kmeans(as.data.frame(oob_tauhat_cf), centers = 4, nstart = 25)

df3 <- as.data.frame(oob_tauhat_cf)
df3 <- mutate(df3, cl = clusters$cluster)

h2 <- ggplot(df3, aes(x=oob_tauhat_cf)) +
  geom_histogram(aes(fill=factor(cl)),bins = 1000) +
  ggtitle("Clusters") +
  theme_bw() +
  xlab("cate")

grid.arrange(h1, h2, ncol=2)

# 4) Heterogeneity across covariates (ntiles)
# Note: the same procedure could be done for clusters after running a corresponding regression

hypothesis <- paste0("ntile1 = ntile", seq(2, num_tiles))

# Regress each covariate on ntile assignment to get the means
cov_means <- lapply(covariate_names1, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + ntile')), data = df_train)
})

names(cov_means) <- covariate_names1


# Extract the mean and standard deviation of each covariate per ntile (nicer to print to RMarkdown)
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Test
cov_ftest <- sapply(cov_means, function(cov_mean) {
  # Sometimes the regression has no residual SSE = 0,
  # we cannot perform an F-test
  tryCatch({
    linearHypothesis(cov_mean, hypothesis, test="F")[2, c("F", "Pr(>F)")]
  },
  error = function(cond){
    message(paste0("Error message during F-test for ", cov_mean$terms[[2]],"`:"))
    message(cond)
    return(c("F"=NA, "Pr(>F)"=NA))
  })
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df = cbind(cov_table_df[,1], round(cov_table_df[,2:5], digits = 2))

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Discount", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1")

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))

# 3.2) Model 2
# Manually creating subgroups
num_tiles <- 4  # ntiles = CATE is above / below the median
df_train$cate2 <- oob_tauhat_cf2
df_train$ntile <- factor(ntile(oob_tauhat_cf2, n=num_tiles))

# Estimating the average effect in the subgroups
# Sample ATE
ols_sample_ate2 <- lm_robust(Revenue ~ ntile + ntile:Discount, data=df_train)
estimated_sample_ate2 <- coef(summary(ols_sample_ate2))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
estimated_sample_ate2 <- data.frame(estimated_sample_ate2)
hypothesis_sample_ate2 <- paste0("ntile1:Discount = ", paste0("ntile", seq(2, num_tiles), ":Discount"))
ftest_pvalue_sample_ate2 <- linearHypothesis(ols_sample_ate2, hypothesis_sample_ate2, test="F")[2,"Pr(>F)"]

# AIPW
estimated_aipw_ate2 <- lapply(seq(num_tiles), function(w) {
  ate <- average_treatment_effect(cf2, subset = df_train$ntile == w)
})
estimated_aipw_ate2 <- data.frame(do.call(rbind, estimated_aipw_ate2))

# Testing for equality using Wald test
## define L matrix that allows us to test if the ATEs in ntile 2+ are equal to ntile 1
.L <- cbind(-1, diag(num_tiles - 1))
# e.g. [,1] [,2] [,3] [,4]
# [1,]   -1    1    0    0
# [2,]   -1    0    1    0
# [3,]   -1    0    0    1
waldtest_pvalue_aipw_ate2 <- wald.test(Sigma = diag(estimated_aipw_ate2$std.err^2),
                                       b = estimated_aipw_ate2$estimate,
                                       L = .L)$result$chi2[3]

# Make a nice table
table.df <- data.frame (Tile  = 1:4,
                        Sample.ATE = round(estimated_sample_ate2$Estimate),
                        AIPW.ATE = round(estimated_aipw_ate2$estimate)
)
na.df <- data.frame(Tile = "", Sample.ATE = NA, AIPW.ATE = NA)

table.df <- do.call(rbind, apply(table.df, 1, function(x) {rbind(x, na.df)}))

stat.test <- c("", paste("F-Test p-value:", round(ftest_pvalue_sample_ate2)), paste("Wald-Test p-value:", round(waldtest_pvalue_aipw_ate2)))

table.df <- rbind(table.df,stat.test)

table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),2] <- round(estimated_sample_ate2$Std..Error)
table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),3] <- round(estimated_aipw_ate2$std.err)

table.df[seq(2,nrow(table.df),2),2:3] <- paste0("(", format(unlist(table.df[seq(2,nrow(table.df),2),2:3])),")")

table.df2 <- formattable(table.df)
table.df2

# Print SATE and AIPW to compare:
box.df2 <- data.frame (N.tile = rep(1:4,2),
                       ATE.estimate = c(estimated_sample_ate2$Estimate,estimated_aipw_ate2$estimate),
                       Method = c(rep("Sample ATE", each=4), rep("AIPW ATE", each=4)),
                       se = c(estimated_sample_ate2$Std..Error,estimated_aipw_ate2$std.err)
)

ggplot(box.df2, aes(x = N.tile, y = ATE.estimate, col = Method)) +
  geom_point(size = 2, position = position_dodge(.3)) +
  geom_errorbar(aes(ymin = ATE.estimate - se, ymax = ATE.estimate + se), position = position_dodge(.3), width=.2) +
  theme(legend.position = 'none') +
  theme_bw() + ggtitle("ATE within N-tiles (as defined by predicted CATE)")

# Test statistical significance (different TEs)
colnames(estimated_sample_ate2)[2]<-"SE"
colnames(estimated_aipw_ate2) <-colnames(estimated_sample_ate2)

p_values_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
differences_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                   dimnames = list(c(1:num_tiles),c(1:num_tiles)))
stderror_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
hypotheses_grid <- combn(1:num_tiles, 2)

estimated_aipw_ate2 <- mutate(estimated_aipw_ate2, Ntile = c(1:num_tiles))

invisible(apply(hypotheses_grid, 2, function(x) {
  .diff <- with(estimated_aipw_ate2, Estimate[Ntile == x[2]] - Estimate[Ntile == x[1]])
  .se <- with(estimated_aipw_ate2, sqrt(SE[Ntile == x[2]]^2 + SE[Ntile == x[1]]^2))
  
  label1 <- paste0("tile", x[1])
  label2 <- paste0("tile", x[2])
  
  differences_tile_by_tile[x[2], x[1]] <<- .diff
  rownames(differences_tile_by_tile)[x[2]] <<- label2
  colnames(differences_tile_by_tile)[x[2]] <<- label2
  rownames(differences_tile_by_tile)[x[1]] <<- label1
  colnames(differences_tile_by_tile)[x[1]] <<- label1
  
  stderror_tile_by_tile[x[2], x[1]] <<- .se
  rownames(stderror_tile_by_tile)[x[2]] <<- label2
  colnames(stderror_tile_by_tile)[x[2]] <<- label2
  rownames(stderror_tile_by_tile)[x[1]] <<- label1
  colnames(stderror_tile_by_tile)[x[1]] <<- label1
  
  p_values_tile_by_tile[x[2], x[1]] <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
  rownames(p_values_tile_by_tile)[x[2]] <<- label2
  colnames(p_values_tile_by_tile)[x[2]] <<- label2
  rownames(p_values_tile_by_tile)[x[1]] <<- label1
  colnames(p_values_tile_by_tile)[x[1]] <<- label1
  
}))

p_values_tile_by_tile = round(p_values_tile_by_tile, digits = 2)
# Bonferroni correction should be applied to decide about the rejection

h1.2 <- ggplot(df_train, aes(x=cate2)) +
  geom_histogram(aes(fill=factor(ntile)), bins = 1000) +
  ggtitle("Quartiles") + 
  theme_bw()

h1.2

# Clustering
# Elbow method
fviz_nbclust(as.data.frame(oob_tauhat_cf2), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# 4-5 seem to be optimal -> flattening afterwards

# See the results for k=4
clusters2 <- kmeans(as.data.frame(oob_tauhat_cf2), centers = 4, nstart = 25)

df3.2 <- as.data.frame(oob_tauhat_cf2)
df3.2 <- mutate(df3, cl = clusters2$cluster)

h2.2 <- ggplot(df3.2, aes(x=oob_tauhat_cf2)) +
  geom_histogram(aes(fill=factor(cl)),bins = 1000) +
  ggtitle("Clusters") +
  theme_bw() +
  xlab("cate2")

grid.arrange(h1.2, h2.2, ncol=2)

# 4) Heterogeneity across covariates (ntiles)
# Note: the same procedure could be done for clusters after running a corresponding regression

hypothesis <- paste0("ntile1 = ntile", seq(2, num_tiles))

# Regress each covariate on ntile assignment to get the means
cov_means <- lapply(covariate_names2, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + ntile')), data = df_train)
})

names(cov_means) <- covariate_names2


# Extract the mean and standard deviation of each covariate per ntile (nicer to print to RMarkdown)
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Test
cov_ftest <- sapply(cov_means, function(cov_mean) {
  # Sometimes the regression has no residual SSE = 0,
  # we cannot perform an F-test
  tryCatch({
    linearHypothesis(cov_mean, hypothesis, test="F")[2, c("F", "Pr(>F)")]
  },
  error = function(cond){
    message(paste0("Error message during F-test for ", cov_mean$terms[[2]],"`:"))
    message(cond)
    return(c("F"=NA, "Pr(>F)"=NA))
  })
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df = cbind(cov_table_df[,1], round(cov_table_df[,2:5], digits = 2))


cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "", "Treatment", "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1")

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))

# 3.3) Model 3
# Manually creating subgroups
num_tiles <- 4  # ntiles = CATE is above / below the median
df_train$cate3 <- oob_tauhat_cf3
df_train$ntile <- factor(ntile(oob_tauhat_cf3, n=num_tiles))

# Estimating the average effect in the subgroups
# Sample ATE
ols_sample_ate3 <- lm_robust(Revenue ~ ntile + ntile:Tech.Support.and.Discount, data=df_train)
estimated_sample_ate3 <- coef(summary(ols_sample_ate3))[(num_tiles+1):(2*num_tiles), c("Estimate", "Std. Error")]
estimated_sample_ate3 <- data.frame(estimated_sample_ate3)
hypothesis_sample_ate3 <- paste0("ntile1:Tech.Support.and.Discount = ", paste0("ntile", seq(2, num_tiles), ":Tech.Support.and.Discount"))
ftest_pvalue_sample_ate3 <- linearHypothesis(ols_sample_ate3, hypothesis_sample_ate3, test="F")[2,"Pr(>F)"]

# AIPW
estimated_aipw_ate3 <- lapply(seq(num_tiles), function(w) {
  ate <- average_treatment_effect(cf3, subset = df_train$ntile == w)
})
estimated_aipw_ate3 <- data.frame(do.call(rbind, estimated_aipw_ate3))

# Testing for equality using Wald test
## define L matrix that allows us to test if the ATEs in ntile 2+ are equal to ntile 1
.L <- cbind(-1, diag(num_tiles - 1))
# e.g. [,1] [,2] [,3] [,4]
# [1,]   -1    1    0    0
# [2,]   -1    0    1    0
# [3,]   -1    0    0    1
waldtest_pvalue_aipw_ate3 <- wald.test(Sigma = diag(estimated_aipw_ate3$std.err^2),
                                       b = estimated_aipw_ate3$estimate,
                                       L = .L)$result$chi2[3]

# Make a nice table
table.df <- data.frame (Tile  = 1:4,
                        Sample.ATE = round(estimated_sample_ate3$Estimate),
                        AIPW.ATE = round(estimated_aipw_ate3$estimate)
)
na.df <- data.frame(Tile = "", Sample.ATE = NA, AIPW.ATE = NA)

table.df <- do.call(rbind, apply(table.df, 1, function(x) {rbind(x, na.df)}))

stat.test <- c("", paste("F-Test p-value:", round(ftest_pvalue_sample_ate3)), paste("Wald-Test p-value:", round(waldtest_pvalue_aipw_ate2)))

table.df <- rbind(table.df,stat.test)

table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),2] <- round(estimated_sample_ate3$Std..Error)
table.df[seq(from = 2, to = nrow(table.df)-1, by = 2),3] <- round(estimated_aipw_ate3$std.err)

table.df[seq(2,nrow(table.df),2),2:3] <- paste0("(", format(unlist(table.df[seq(2,nrow(table.df),2),2:3])),")")

table.df3 <- formattable(table.df)
table.df3

# Print SATE and AIPW to compare:
box.df3 <- data.frame (N.tile = rep(1:4,2),
                       ATE.estimate = c(estimated_sample_ate3$Estimate,estimated_aipw_ate3$estimate),
                       Method = c(rep("Sample ATE", each=4), rep("AIPW ATE", each=4)),
                       se = c(estimated_sample_ate3$Std..Error,estimated_aipw_ate3$std.err)
)

ggplot(box.df3, aes(x = N.tile, y = ATE.estimate, col = Method)) +
  geom_point(size = 2, position = position_dodge(.3)) +
  geom_errorbar(aes(ymin = ATE.estimate - se, ymax = ATE.estimate + se), position = position_dodge(.3), width=.2) +
  theme(legend.position = 'none') +
  theme_bw() + ggtitle("ATE within N-tiles (as defined by predicted CATE)")

# Test statistical significance (different TEs)
colnames(estimated_sample_ate3)[2]<-"SE"
colnames(estimated_aipw_ate3) <-colnames(estimated_sample_ate3)

p_values_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
differences_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                   dimnames = list(c(1:num_tiles),c(1:num_tiles)))
stderror_tile_by_tile <- matrix(nrow = num_tiles, ncol = num_tiles,
                                dimnames = list(c(1:num_tiles),c(1:num_tiles)))
hypotheses_grid <- combn(1:num_tiles, 2)

estimated_aipw_ate3 <- mutate(estimated_aipw_ate3, Ntile = c(1:num_tiles))

invisible(apply(hypotheses_grid, 2, function(x) {
  .diff <- with(estimated_aipw_ate3, Estimate[Ntile == x[2]] - Estimate[Ntile == x[1]])
  .se <- with(estimated_aipw_ate3, sqrt(SE[Ntile == x[2]]^2 + SE[Ntile == x[1]]^2))
  
  label1 <- paste0("tile", x[1])
  label2 <- paste0("tile", x[2])
  
  differences_tile_by_tile[x[2], x[1]] <<- .diff
  rownames(differences_tile_by_tile)[x[2]] <<- label2
  colnames(differences_tile_by_tile)[x[2]] <<- label2
  rownames(differences_tile_by_tile)[x[1]] <<- label1
  colnames(differences_tile_by_tile)[x[1]] <<- label1
  
  stderror_tile_by_tile[x[2], x[1]] <<- .se
  rownames(stderror_tile_by_tile)[x[2]] <<- label2
  colnames(stderror_tile_by_tile)[x[2]] <<- label2
  rownames(stderror_tile_by_tile)[x[1]] <<- label1
  colnames(stderror_tile_by_tile)[x[1]] <<- label1
  
  p_values_tile_by_tile[x[2], x[1]] <<- 1 - pnorm(abs(.diff/.se)) + pnorm(-abs(.diff/.se))
  rownames(p_values_tile_by_tile)[x[2]] <<- label2
  colnames(p_values_tile_by_tile)[x[2]] <<- label2
  rownames(p_values_tile_by_tile)[x[1]] <<- label1
  colnames(p_values_tile_by_tile)[x[1]] <<- label1
  
}))

p_values_tile_by_tile = round(p_values_tile_by_tile, digits = 2)
# Bonferroni correction should be applied to decide about the rejection

h1.3 <- ggplot(df_train, aes(x=cate3)) +
  geom_histogram(aes(fill=factor(ntile)), bins = 1000) +
  ggtitle("Quartiles") + 
  theme_bw()

h1.3

# Clustering
# Elbow method
fviz_nbclust(as.data.frame(oob_tauhat_cf3), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# 4-5 seem to be optimal -> flattening afterwards

# See the results for k=4
clusters3 <- kmeans(as.data.frame(oob_tauhat_cf3), centers = 4, nstart = 25)

df3.3 <- as.data.frame(oob_tauhat_cf3)
df3.3 <- mutate(df3, cl = clusters3$cluster)

h2.3 <- ggplot(df3, aes(x=oob_tauhat_cf3)) +
  geom_histogram(aes(fill=factor(cl)),bins = 1000) +
  ggtitle("Clusters") +
  theme_bw() +
  xlab("cate3")

grid.arrange(h1.3, h2.3, ncol=2)


# 4) Heterogeneity across covariates (ntiles)
# Note: the same procedure could be done for clusters after running a corresponding regression

hypothesis <- paste0("ntile1 = ntile", seq(2, num_tiles))

# Regress each covariate on ntile assignment to get the means
cov_means <- lapply(covariate_names3, function(covariate) {
  lm_robust(as.formula(paste0(covariate, ' ~ 0 + ntile')), data = df_train)
})

names(cov_means) <- covariate_names3


# Extract the mean and standard deviation of each covariate per ntile (nicer to print to RMarkdown)
cov_table <- lapply(cov_means, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[,c("Estimate", "Std. Error")]))
})

# Test
cov_ftest <- sapply(cov_means, function(cov_mean) {
  # Sometimes the regression has no residual SSE = 0,
  # we cannot perform an F-test
  tryCatch({
    linearHypothesis(cov_mean, hypothesis, test="F")[2, c("F", "Pr(>F)")]
  },
  error = function(cond){
    message(paste0("Error message during F-test for ", cov_mean$terms[[2]],"`:"))
    message(cond)
    return(c("F"=NA, "Pr(>F)"=NA))
  })
})

# Make a nice table
cov_table_df <- ldply(cov_table, data.frame)
cov_table_df = cbind(cov_table_df[,1], round(cov_table_df[,2:5], digits = 2))

cov_names <- c("Global.Flag","","Major.Flag","","SMC.Flag","","Commercial.Flag","","IT.Spend","","Employee.Count","","PC.Count","","Size",           
               "") 

cov_table_df <- cov_table_df %>%
  add_column(covariates = cov_names,
             .before = "leaf1")

cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)] <- paste0("(", format(unlist(cov_table_df[seq(2,nrow(cov_table_df),2),2:ncol(cov_table_df)])),")")

formattable(cov_table_df, list(area(col = 2:ncol(cov_table_df)) ~ color_tile("transparent", "pink")))

# 5) Partial dependence plots
# 5.1) Model 1
# Split up continuous and binary variables
binary_covariates <- sapply(covariate_names1,
                            function(x) length(unique(df_train[, x])) <= 2)

q <- 5 # Eval at minimum and quartiles (1+4)
evaluate_partial_dependency <- function(var_of_interest, is_binary) {
  if(is_binary){
    # Get two unique values for the variable
    x_grid <- sort(unique(df_train[,var_of_interest]))
  } else {
    # Get quantile values for the variable
    x_grid <- quantile(df_train[,var_of_interest], probs = seq(0, 1, length.out = q))
  }
  df_grid <- setNames(data.frame(x_grid), var_of_interest)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names1[which(covariate_names1 != var_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- expand_grid(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf, newdata=df_eval[,covariate_names1], estimate.variance=TRUE)
  rbind('Tau Hat' = pred$predictions,
        'Std. Error' = sqrt(pred$variance.estimates))
}

# Make the table for non-binary variables
nonbinary_partial_dependency_tauhats <- lapply(covariate_names1[!binary_covariates],
                                               function(variable) evaluate_partial_dependency(variable, FALSE))

names(nonbinary_partial_dependency_tauhats) <- covariate_names1[!binary_covariates]

# Make the table for binary variables
binary_partial_dependency_tauhats <- lapply(covariate_names1[binary_covariates],
                                            function(variable) evaluate_partial_dependency(variable, TRUE))

names(binary_partial_dependency_tauhats) <- covariate_names1[binary_covariates]

# Plot one variable
var_interest <- "Size"

plotPartialDep <- function(grid, values, stderr) {
  pos <- c(1:length(grid))
  df <- as.data.frame(cbind(pos,values,stderr))
  ggplot(df, aes(x=pos, y=values)) +
    geom_errorbar(aes(ymin = values - 2*stderr, ymax = values + 2*stderr), color = "blue", width = 0.1) +
    geom_line(color="red") +
    scale_x_continuous(name = paste0("Effect of ", var_interest, " evaluated at min and quartiles"),
                       breaks = pos,
                       labels = as.character(grid)) + 
    ggtitle("Partial Dependence Plot") +
    theme_bw()
}

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

var_interest <- "IT.Spend"

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

# 5.2) Model 2
# Split up continuous and binary variables
binary_covariates <- sapply(covariate_names1,
                            function(x) length(unique(df_train[, x])) <= 2)

q <- 5 # Eval at minimum and quartiles (1+4)
evaluate_partial_dependency <- function(var_of_interest, is_binary) {
  if(is_binary){
    # Get two unique values for the variable
    x_grid <- sort(unique(df_train[,var_of_interest]))
  } else {
    # Get quantile values for the variable
    x_grid <- quantile(df_train[,var_of_interest], probs = seq(0, 1, length.out = q))
  }
  df_grid <- setNames(data.frame(x_grid), var_of_interest)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names2[which(covariate_names2 != var_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- expand_grid(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf2, newdata=df_eval[,covariate_names2], estimate.variance=TRUE)
  rbind('Tau Hat' = pred$predictions,
        'Std. Error' = sqrt(pred$variance.estimates))
}

# Make the table for non-binary variables
nonbinary_partial_dependency_tauhats <- lapply(covariate_names2[!binary_covariates],
                                               function(variable) evaluate_partial_dependency(variable, FALSE))

names(nonbinary_partial_dependency_tauhats) <- covariate_names2[!binary_covariates]

# Make the table for binary variables
binary_partial_dependency_tauhats <- lapply(covariate_names2[binary_covariates],
                                            function(variable) evaluate_partial_dependency(variable, TRUE))

names(binary_partial_dependency_tauhats) <- covariate_names2[binary_covariates]

# Plot one variable
var_interest <- "Size"

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

var_interest <- "IT.Spend"

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

# 5.3) Model 3
# Split up continuous and binary variables
binary_covariates <- sapply(covariate_names3,
                            function(x) length(unique(df_train[, x])) <= 2)

q <- 5 # Eval at minimum and quartiles (1+4)
evaluate_partial_dependency <- function(var_of_interest, is_binary) {
  if(is_binary){
    # Get two unique values for the variable
    x_grid <- sort(unique(df_train[,var_of_interest]))
  } else {
    # Get quantile values for the variable
    x_grid <- quantile(df_train[,var_of_interest], probs = seq(0, 1, length.out = q))
  }
  df_grid <- setNames(data.frame(x_grid), var_of_interest)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names3[which(covariate_names3 != var_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- expand_grid(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf3, newdata=df_eval[,covariate_names3], estimate.variance=TRUE)
  rbind('Tau Hat' = pred$predictions,
        'Std. Error' = sqrt(pred$variance.estimates))
}

# Make the table for non-binary variables
nonbinary_partial_dependency_tauhats <- lapply(covariate_names3[!binary_covariates],
                                               function(variable) evaluate_partial_dependency(variable, FALSE))

names(nonbinary_partial_dependency_tauhats) <- covariate_names3[!binary_covariates]

# Make the table for binary variables
binary_partial_dependency_tauhats <- lapply(covariate_names3[binary_covariates],
                                            function(variable) evaluate_partial_dependency(variable, TRUE))

names(binary_partial_dependency_tauhats) <- covariate_names3[binary_covariates]

# Plot one variable
var_interest <- "Size"

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

var_interest <- "IT.Spend"

plotPartialDep(quantile(df_train[,var_interest], probs = seq(0, 1, length.out = q)),
               nonbinary_partial_dependency_tauhats[[var_interest]]["Tau Hat",],
               nonbinary_partial_dependency_tauhats[[var_interest]]["Std. Error",])

# 6) Two-way tables
# 6.1) Model 1
covariate_subset <- names(sorted_var_imp)[1:5] # Analyse for 5 most important vars for splitting
all_pairs <- combn(covariate_subset, m = 2)

# The CATE function’s value for x1 and x2 evaluated at combinations of 
#‘high’ and ‘low’ (e.g. HL is High-Low with x1 at its 80th percentile, 
# x2 at its 20th percentile). All other covariates are fixed at their medians.

evaluate_twoway_partial_dependency <- function(vars_of_interest) {
  # Create a grid of values: if continuous, quintiles; else, plot the actual values
  x_grids <- list(NULL, NULL)
  for(i in 1:2) {
    is_binary <- (length(unique(df_train[,vars_of_interest[i]])) <= 2)
    if(is_binary) {
      x_grids[[i]] <- sort(unique(df_train[,vars_of_interest[i]]))
    } else {
      x_grids[[i]] <- quantile(df_train[,vars_of_interest[i]], probs = c(0.2, 0.8))
    }
  }
  x_grids <- setNames(x_grids, vars_of_interest)
  df_grid <- do.call(expand.grid, x_grids)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names1[which(!covariate_names1 %in% vars_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- crossing(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf, newdata=df_eval[,covariate_names1], estimate.variance=TRUE)
  res <- rbind('Tau Hat' = pred$predictions,
               'Std. Error' = sqrt(pred$variance.estimates))
  colnames(res) <- c("LL", "HL", "LH", "HH")
  res
  
}

twoway_partial_dependency_tauhats <- lapply(1:ncol(all_pairs),
                                            function(j) evaluate_twoway_partial_dependency(all_pairs[, j]))

names(twoway_partial_dependency_tauhats) <- sapply(1:ncol(all_pairs),
                                                   function(j) paste0(all_pairs[1,j],"-",all_pairs[2,j]))

# Plot one combination
var_interest1 <- "Size"
var_interest2 <- "IT.Spend"

plotTwoWay <- function(values, var1, var2) {
  x1 <- c(1,2,1,2)
  x2 <- c(1,1,2,2)
  df <- as.data.frame(cbind(x1,x2,values))
  ggplot(df, aes(x=x1, y=x2, fill = values)) +
    geom_tile() +
    scale_x_continuous(name = var1, breaks = c(1,2), labels = c("L", "H")) +
    scale_y_continuous(name = var2, breaks = c(1,2), labels = c("L", "H")) +
    ggtitle(paste0("Two way plot of ", var1, " and ", var2, " evaluated at 20th and 80th percentiles")) +
    theme_bw()
}

plotTwoWay(twoway_partial_dependency_tauhats[[paste0(var_interest1,"-",var_interest2)]]["Tau Hat",],
           var_interest1, var_interest2)

# 6.2) Model 2
covariate_subset <- names(sorted_var_imp2)[1:5] # Analyse for 5 most important vars for splitting
all_pairs <- combn(covariate_subset, m = 2)

# The CATE function’s value for x1 and x2 evaluated at combinations of 
#‘high’ and ‘low’ (e.g. HL is High-Low with x1 at its 80th percentile, 
# x2 at its 20th percentile). All other covariates are fixed at their medians.

evaluate_twoway_partial_dependency <- function(vars_of_interest) {
  # Create a grid of values: if continuous, quintiles; else, plot the actual values
  x_grids <- list(NULL, NULL)
  for(i in 1:2) {
    is_binary <- (length(unique(df_train[,vars_of_interest[i]])) <= 2)
    if(is_binary) {
      x_grids[[i]] <- sort(unique(df_train[,vars_of_interest[i]]))
    } else {
      x_grids[[i]] <- quantile(df_train[,vars_of_interest[i]], probs = c(0.2, 0.8))
    }
  }
  x_grids <- setNames(x_grids, vars_of_interest)
  df_grid <- do.call(expand.grid, x_grids)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names2[which(!covariate_names2 %in% vars_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- crossing(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf2, newdata=df_eval[,covariate_names2], estimate.variance=TRUE)
  res <- rbind('Tau Hat' = pred$predictions,
               'Std. Error' = sqrt(pred$variance.estimates))
  colnames(res) <- c("LL", "HL", "LH", "HH")
  res
  
}

twoway_partial_dependency_tauhats <- lapply(1:ncol(all_pairs),
                                            function(j) evaluate_twoway_partial_dependency(all_pairs[, j]))

names(twoway_partial_dependency_tauhats) <- sapply(1:ncol(all_pairs),
                                                   function(j) paste0(all_pairs[1,j],"-",all_pairs[2,j]))

# Plot one combination
var_interest1 <- "Size"
var_interest2 <- "IT.Spend"

plotTwoWay(twoway_partial_dependency_tauhats[[paste0(var_interest1,"-",var_interest2)]]["Tau Hat",],
           var_interest1, var_interest2)

# 6.3) Model 3
covariate_subset <- names(sorted_var_imp3)[1:5] # Analyse for 5 most important vars for splitting
all_pairs <- combn(covariate_subset, m = 2)

# The CATE function’s value for x1 and x2 evaluated at combinations of 
#‘high’ and ‘low’ (e.g. HL is High-Low with x1 at its 80th percentile, 
# x2 at its 20th percentile). All other covariates are fixed at their medians.

evaluate_twoway_partial_dependency <- function(vars_of_interest) {
  # Create a grid of values: if continuous, quintiles; else, plot the actual values
  x_grids <- list(NULL, NULL)
  for(i in 1:2) {
    is_binary <- (length(unique(df_train[,vars_of_interest[i]])) <= 2)
    if(is_binary) {
      x_grids[[i]] <- sort(unique(df_train[,vars_of_interest[i]]))
    } else {
      x_grids[[i]] <- quantile(df_train[,vars_of_interest[i]], probs = c(0.2, 0.8))
    }
  }
  x_grids <- setNames(x_grids, vars_of_interest)
  df_grid <- do.call(expand.grid, x_grids)
  
  # For the other variables, keep them at their median
  other_covariates <- covariate_names3[which(!covariate_names3 %in% vars_of_interest)]
  df_median <- data.frame(lapply(df_train[,other_covariates], median))
  df_eval <- crossing(df_median, df_grid)
  
  # Predict the treatment effect
  pred <- predict(cf3, newdata=df_eval[,covariate_names3], estimate.variance=TRUE)
  res <- rbind('Tau Hat' = pred$predictions,
               'Std. Error' = sqrt(pred$variance.estimates))
  colnames(res) <- c("LL", "HL", "LH", "HH")
  res
  
}

twoway_partial_dependency_tauhats <- lapply(1:ncol(all_pairs),
                                            function(j) evaluate_twoway_partial_dependency(all_pairs[, j]))

names(twoway_partial_dependency_tauhats) <- sapply(1:ncol(all_pairs),
                                                   function(j) paste0(all_pairs[1,j],"-",all_pairs[2,j]))

# Plot one combination
var_interest1 <- "Size"
var_interest2 <- "IT.Spend"

plotTwoWay(twoway_partial_dependency_tauhats[[paste0(var_interest1,"-",var_interest2)]]["Tau Hat",],
           var_interest1, var_interest2)

#######################################################################
# Robustness Test: Selected Variables by Variable Importance
#######################################################################
set.seed(18062022)
# 1.1) Build the forest with treatment = tech.support
cf4 <- causal_forest(
  X = as.matrix(df_train[,selected.idx]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support,
  num.trees = nrow(df_train[,selected.idx])) # ! This is only for speed. Real analysis -> more

## 1.2) Build forest with treatment = discount
cf5 <- causal_forest(
  X = as.matrix(df_train[,selected.idx2]),
  Y = df_train$Revenue,
  W = df_train$Discount,
  num.trees = nrow(df_train[,selected.idx2]))

## 1.3) Build forest with treatment = tech.support & discount
cf6 <- causal_forest(
  X = as.matrix(df_train[,selected.idx3]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support.and.Discount,
  num.trees = nrow(df_train[,selected.idx3]))

ATE4 = average_treatment_effect(cf4)
paste("95% CI for the ATE:", round(ATE4[1], 3),
      "+/-", round(qnorm(0.975) * ATE4[2], 3))

# Model 2
ATE5 = average_treatment_effect(cf5)
paste("95% CI for the ATE:", round(ATE5[1], 3),
      "+/-", round(qnorm(0.975) * ATE5[2], 3))

# Model 3
ATE6 = average_treatment_effect(cf6)
paste("95% CI for the ATE:", round(ATE6[1], 3),
      "+/-", round(qnorm(0.975) * ATE6[2], 3))

# Omnibus Test
OmniTest4 <- test_calibration(cf4)
stargazer(OmniTest4)
OmniTest5 <- test_calibration(cf5)
stargazer(OmniTest5)
OmniTest6 <- test_calibration(cf6)
stargazer(OmniTest6)

# 3) Predict point estimates (training, out-of-bag)
# 3.1) Model 1
oob_pred <- predict(cf4, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf4 <- oob_pred$predictions
oob_tauhat_cf_se4 <- sqrt(oob_pred$variance.estimates)

# 3.2) Model 2
oob_pred <- predict(cf5, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf5 <- oob_pred$predictions
oob_tauhat_cf_se5 <- sqrt(oob_pred$variance.estimates)

# 3.3) Model 3
oob_pred <- predict(cf6, estimate.variance=TRUE)
# not passing any data set -> Out-Of-Bag predictions

oob_tauhat_cf6 <- oob_pred$predictions
oob_tauhat_cf_se6 <- sqrt(oob_pred$variance.estimates)

# ASSESS THE HETEROGENEITY
# 1.1) Histogram of CATEs, Model 1
ggplot(as.data.frame(oob_tauhat_cf4), aes(x=oob_tauhat_cf4)) +
  geom_histogram(aes(x = oob_tauhat_cf4),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Tech Support)")

# 1.2) Histogram of CATEs, Model 2
ggplot(as.data.frame(oob_tauhat_cf5), aes(x=oob_tauhat_cf5)) +
  geom_histogram(aes(x = oob_tauhat_cf5),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Discount)")

# 1.3) Histogram of CATEs, Model 3
ggplot(as.data.frame(oob_tauhat_cf6), aes(x=oob_tauhat_cf6)) +
  geom_histogram(aes(x = oob_tauhat_cf6),fill="#00BFC4", alpha = 0.8) +
  theme_bw() +
  xlab("CATE estimate") + ylab("Frequency") +
  labs(title = "Causal forests: out-of-bag CATE (W=Tech Support & Discount)") 

#######################################################################
# Policy Recommendation
#######################################################################

### PARAMETER SELECTION ###

# Enter the following Parameters to receive a recommendation for treatment

# Does the customer have Global offices?
Global.Flag     = 0    # Enter 1 for 'yes' and 0 for 'no'

# Is the customer large in their industry? 
Major.Flag      = 0    # Enter 1 for 'yes' and 0 for 'no'

# Is the customer a small or medium corporation? 
SMC.Flag        = 1    # Enter 1 for 'yes' and 0 for 'no'

# Is the customers business commercial? 
Commercial.Flag = 0    # Enter 1 for 'yes' and 0 for 'no'

# How much $ does the customer spend on IT-related purchases
IT.Spend        = 25000    # Answer in $

# Number of employees of the customer
Employee.Count  = 55   # Number of employees

# Number of PCs the customer uses
PC.Count        = 55   # Number of PCs

# Size of the customer in terms of revenue in $
Size            = 751234   # Revenue of customer in $

### END OF PARAMETER SELECTION ###

# Covariates (here we are excluding Tech Support & Discount)
covariate_names <- c("Global.Flag","Major.Flag","SMC.Flag","Commercial.Flag","IT.Spend","Employee.Count","PC.Count","Size") 

# Estimate Forest again without Tech SUpport & Discount
# 1.1) Build the forest with treatment = tech.support
cf <- causal_forest(
  X = as.matrix(df_train[,covariate_names]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support,
  num.trees = nrow(df_train[,covariate_names])) # ! This is only for speed. Real analysis -> more

## 1.2) Build forest with treatment = discount
cf2 <- causal_forest(
  X = as.matrix(df_train[,covariate_names]),
  Y = df_train$Revenue,
  W = df_train$Discount,
  num.trees = nrow(df_train[,covariate_names]))

## 1.3) Build forest with treatment = tech.support & discount
cf3 <- causal_forest(
  X = as.matrix(df_train[,covariate_names]),
  Y = df_train$Revenue,
  W = df_train$Tech.Support.and.Discount,
  num.trees = nrow(df_train[,covariate_names]))

# Create a Data Frame with customer data
Customer_Data <- data.frame(Global.Flag,  Major.Flag, SMC.Flag, Commercial.Flag, IT.Spend,  Employee.Count, PC.Count, Size)

# Predict the expected treatment effects for the customer. 
test_pred_support <- predict(cf, newdata=as.matrix(Customer_Data[covariate_names]), estimate.variance=TRUE)
test_pred_discount <- predict(cf2, newdata=as.matrix(Customer_Data[covariate_names]), estimate.variance=TRUE)
test_pred_both <- predict(cf3, newdata=as.matrix(Customer_Data[covariate_names]), estimate.variance=TRUE)

# Calculate the results
test_pred_compare <- bind_rows(test_pred_support, test_pred_discount, test_pred_both)

test_pred_compare$treatment <-c('tech support', 'discount', 'both treatments')

ggplot(data=test_pred_compare, aes(x=treatment, y=predictions))+
  geom_col(fill="#00BFC4")+
  labs(title = "Predicted Revenue Increase by Incentive", 
       x = "Type of Incentive",
       y = "Predicted Revenue growth in $")+
  theme_bw()

best_treatment_index = which.max(test_pred_compare$predictions)
best_treatment = test_pred_compare[best_treatment_index, 3]

Rec_1 <- paste("The expected increase in revenue for providing tech support is ",
               round(test_pred_support[,1],2), "$")

Rec_2 <- paste("The expected increase in revenue for giving a discount is ",
               round(test_pred_discount[,1],2), "$")

Rec_3 <- paste("The expected increase in revenue for providing both incentives is",
               round(test_pred_both[,1],2), "$")

cat(Rec_1, "\n", Rec_2,  "\n", Rec_3,   "\n", "The recommended action is to provide", best_treatment )


#######################################################################
# END
#######################################################################