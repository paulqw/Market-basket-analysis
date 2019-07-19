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
#trans_csv <- read.csv("ElectronidexTransactions2017.csv")

transactions_raw <- read.transactions(file = "ElectronidexTransactions2017.csv", 
                                      header = FALSE, format = "basket", sep = ",")# rm.duplicates = FALSE)
df_trans_log <- data.frame(as(transactions_raw, "matrix"))

df_trans_bin <- df_trans_log

for (i in c(1:nrow(df_trans_bin))){
  for (j in c(1:ncol(df_trans_bin))){
    df_trans_bin[i, j] <- as.integer(df_trans_bin[i, j]) 
  }
}

#### 0. Item labels ####
#Matching old item 
iL <- itemLabels(transactions_raw)

iL_new <- as.vector(read.csv("itemlabel_new.csv"))
iL_n <- iL_new[,1]

for (i in 1:125) {
  old <- iL[i]
  for (j in 1:125) {
  rownum_new <- which(iL_new[, 1] == old, arr.ind = TRUE) 
  }
}

agrep(as.character(old), as.character(iL_new[c(1:25), 1]), ignore.case = FALSE, value = TRUE,
      max.distance = 10)

#Renaming items to new itemnames
#itemLabels(transaction) <- c("nail","Black Hammer 127","White desk 12","green desk","

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
itemFrequencyPlot(transactions_raw, topN = 30, type = "absolute", horiz = TRUE,
                  col = brewer.pal(8,'Pastel2'), main = "Absolute frequency of Top 30 items", 
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
#-aprioir perform two steps before creating association rules
#1. 1st step: analyzing all itemsets that meet the support measurement requirement occuring in multiple phases. 
#Each phase involves analyzing a different number of total items in an itemset. E.g. phase 5 is for itemsets consist of 5 items
#--> First step creates a set of all frequent itemsets.
#2. 2nd step: analyzes all of the itemsets to determine which meet the confidence measurement requirement. 
#The itemsets/rules that do not meet this requirement are removed.

##Out of the box
#rules cover 10% of the transactions(supp = .1) and are 80% correct (conf = .8)
rules <- apriori(transactions_raw, parameter = list(supp = 0.0007, conf = 0.90, minlen = 2, maxlen = 10)) #first rules using supp = 0.001 and
#0.0009 0.95 --> 98 rules

func_redundant(rules) 
inspect(rules_unique)
inspect(head(rules_sorted, 10))
func_sort(rules_unique)
inspect(rules_sorted)

summary(rules_sorted)

plot(rules_sorted)
inspectDT(rules_sorted)             #interactive inspect
plotly_arules(rules_sorted[1:5],  #for interactive plots
              engine = "htmlwidget")   #interactive plot 
plot(rules_sorted[1:5], method = "two-key plot", control = list(type = "items", reorder = TRUE))
plot(rules_sorted[1:10], method = "paracoord", control = list(type = "items", reorder = TRUE))

#Meaning of values:
#-values too high --> no rules or non-helpful rules
#-values too low --> computational time/memory will suffer, or too many rules. 
#-to find strong rules, increase the value of ‘conf’ parameter
#-minlen = 1 --> rules with one item, means: no matter what other items are involved, 
#the item in the RHS will appear with the probability given by the rule's confidence (= support)
#To avoid these rules increase minlen (to 2)
inspect(rules[1:25])
summary(rules) 
str(rules)


#lift measures importance of a rule --> high = important rule
#Unlike confidence, {I1} -> {I2} is the same as {I2} -> {I1} in the context of the lift measurement

##Check, exclude, sort, and select rules
#Check for redundant rules and contain them
func_redundant <- function (trans_data){
  sum(is.redundant(trans_data))
  rules_red <- trans_data[which(is.redundant(trans_data))]
  n_rules_red <- c(which(is.redundant(trans_data)))
  inspect(rules_red)
  
  #Exclude redundant rules
  rules_unique <<- trans_data[-c(n_rules_red)]
}
func_redundant(rules) 
inspect(rules_unique)

#Specific rules per item
ItemRules <- subset(rules_unique, items %in% "iMac")
inspect(ItemRules)

#Sort rules in terms of support, confidence, lift

func_sort <- function (trans_data){
  inspect(sort(trans_data, by = "count", decreasing = FALSE))
  rules_sorted <<- sort(trans_data, by = "lift")
  inspect(rules_sorted)
}
func_sort(rules_unique)

#why not working? 
nrules = 98
x <- data.frame(matrix(ncol = 6, nrow = 0)) 
names(x) <- c("lhs", "rhs", "support", "confidence", "lift", "count")

for (i in (1:nrules)) {
  x <- rbind(x, inspect(rules[i]))
}

##Visualization of rules


