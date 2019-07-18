# Load libraries
install.packages("pacman")
library(pacman)
p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, 
       C50, party, reshape, dplyr,markdown, ggpubr, tidyr, hydroGOF, BBmisc, 
       tidyverse, textclean, inum, doParallel, Hmisc, caretEnsemble, mboost, 
       cluster, ade4, factoextra, asbio, FactoMineR, fpc,e1071, 
       randomForest, rstudioapi, MASS, ParamHelpers, mlr,
       arules,      # analyzing transactional data
       arulesViz,    # provides visual techniques for the arules package.
       RColorBrewer
       )

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
transactions_raw <- read.transactions(file = "ElectronidexTransactions2017.csv", 
                                      header = FALSE, format = "basket", sep = ",")# rm.duplicates = FALSE)
df_transactions <- read.csv("ElectronidexTransactions2017.csv")
tO_transactions <-as(df_transactions,"transactions")

#### 1. Exploring - Get to know the data ####
transactions_raw
summary(transactions_raw)

inspect (transactions_raw[191])      # View transactions: setStart,setEnd,itemSep ruleSep
length (transactions_raw)       # Number of transactions
size (transactions_raw)         # Number of items per transaction
LIST(transactions_raw)          # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transactions_raw)    # To see the item labels

##Visualization
# Create an item frequency plot
itemFrequencyPlot(transactions_raw, topN = 15, type = "absolute", horiz = TRUE,
                  col = brewer.pal(8,'Pastel2'), main = "Absolute frequency of Top 15 items", 
                  xlab = "Item frequency")

# Plot image of distribution of random items
image(sample(transactions_raw[1:300]))
image(sample(transactions_raw, size = 300)) # of Transactions you'd like to plot))


#### 3. Applying ####
##Apriori algorithm
#-Apriori algorithm: helpful for large datasets and is used to uncover insights pertaining transactional datasets
#It is based on item frequency. E.g.: Item set {I1, I2, I3, I4} can only occur if items {I1}, {I2}, {I3} and {I4} 
#occur just as frequently.
#-Apriori assesses association rules using two types of measurements
#1. Stat. measure - Support measurement: measures itemsets or rules frequency within transactional data.
#2. Stat. measure - Confidence measurement, measures accuracy of the rules. 
#--> Rules that measure high in both support and confidence == strong rule
#Important: correlation does not imply causation! E.g.{I1} -> {I2} != {I2} -> {I1}
#