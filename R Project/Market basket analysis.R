# Load libraries
install.packages("pacman")
library(pacman)
p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, 
       C50, party, reshape, dplyr,markdown, ggpubr, tidyr, hydroGOF, BBmisc, 
       tidyverse, textclean, inum, doParallel, Hmisc, caretEnsemble, mboost, 
       cluster, ade4, factoextra, asbio, FactoMineR, fpc,e1071, 
       randomForest, rstudioapi, MASS, ParamHelpers, mlr)

# Enable parallel computing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Disable scientific notation
options(scipen = 999)

#Loading dataset(s)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()
setwd("..")
setwd("Data")
getwd()
#setwd("C:/Users/Paul/Desktop/Ubiqum/Module 2/2.3 Multiple Regression/Git/Profitability")

transactions_raw   <- read.csv("ElectronidexTransactions2017.csv")
