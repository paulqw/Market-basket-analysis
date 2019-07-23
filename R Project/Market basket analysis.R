#### Load libraries & Setup ####
install.packages("pacman")
library(pacman)
p_load(caret, lattice, readr, Metrics, corrplot, e1071, mlr, recipes, ggplot2, 
       C50, party, reshape, dplyr,markdown, ggpubr, tidyr, hydroGOF, BBmisc, 
       tidyverse, textclean, inum, doParallel, Hmisc, caretEnsemble, mboost, 
       cluster, ade4, factoextra, asbio, FactoMineR, fpc,e1071, 
       randomForest, rstudioapi, MASS, ParamHelpers, mlr,
       arules,      # analyzing transactional data
       arulesViz,    # provides visual techniques for the arules package.
       RColorBrewer,
       plyr
       )

# Enable parallel computing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# Disable scientific notation
options(scipen = 999)

#### Loading data ####
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()
setwd("..")
setwd("Data")

## Electronidex ##
#Load transaction into class transaction (sparse matrix)
transactions_raw <- read.transactions(file = "ElectronidexTransactions2017.csv", 
                                      header = FALSE, format = "basket", sep = ",", rm.duplicates = TRUE)# rm.duplicates = FALSE)
#Load transaction into class df
df_raw <- read.csv("ElectronidexTransactions2017.csv", header = FALSE)

#Load itemlevels (new name, brand, category) into class df
df_names <- read.csv("List_itemNames.csv")

brand <- read.csv("il_brand.csv")
brand_vector <- as.character(brand$brand)

#Safe itemNames
write.table(itemLabels(transactions_raw), file = "List_itemNames.csv", sep = ",")

## Blackwell Electronics ##
be_ex <- read.csv("blackwell_existing.csv", header = TRUE)
be_new <- read.csv("blackwell_new.csv", header = TRUE)

#### Prepare data ####
#Change data type of df to character 
for(i in c(1:32)){
  df_raw[,i] <- as.character(df_raw[,i])
}
str(df_raw)

#Change data type of df itemLevels
for(i in c(1:ncol(df_names))){
  df_names[, i] <- as.character(df_names[, i])
}

#Transfer transaction matrix via logical into binary
df_trans_log <- data.frame(as(transactions_raw, "matrix"))
df_trans_bin <- df_trans_log

for (i in c(1:ncol(df_trans_bin))){
  df_trans_bin[, i] <- as.integer(df_trans_bin[, i]) 
}  

#### 0. Item labels ####
#Getting il_raw
il_raw <- itemLabels(transactions_raw)

#Getting brand level (brand search)

df_names[, "brand_found"] <- NA

for (i in c(brand_vector)) {
  for (j in c(1:125)) {
    items_found <- agrep(as.character(i), df_names[, which(colnames(df_names) == "il_raw")],  max.distance = 0, 
                         ignore.case = TRUE, value = FALSE)
    df_names[c(items_found), which(colnames(df_names) == "brand_found")] <- i
  }
}

#Renaming items to new itemnames --> aggregate itemLevels
df <- df_raw
for (k in c(1:125)) {
  on <- as.character(df_names[k, which(colnames(df_names) == "il_raw")])
  nn <- as.character(df_names[k, which(colnames(df_names) == "il_new")])
  for (i in c(1:32)){
    for (j in c(1:9835))
         df[j, i] <- replace(df[j, i], df[j, i] == on, nn)
  }
}

#Possible other options:
#1. mgsub(df, pattern = "Acer Aspire", replacement = nn)
#2. which(grepl(on, df[i, j], ignore.case = TRUE))] <- nn
#3. #within(df[, 1] == sn) <- as.character(df_names[k, 4])

#Attempt to renamw in transaction data
#e.g. itemLabels(transaction) <- c("nail","Black Hammer 127","White desk 12","green desk")
#Does not work for purpose of item reduction as itemLabels only allow for unique labels
#-->Add new itemlevel and save new transaction dataset by these itemlevel
transactions_raw@itemInfo$il_new <- df_names$il_new
transactions_raw@itemInfo$brand <- df_names$brand
transactions_raw@itemInfo$category <- df_names$category

trans_il_new <- arules::aggregate(transactions_raw, by = "il_new")
trans_brand <- arules::aggregate(transactions_raw, by = "brand")
trans_category <- arules::aggregate(transactions_raw, by = "category")
df_trans_cat <- data.frame(as(transactions_raw, "matrix"))

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

#### 2. Compare Blackwell Electronix and Electronidex ####
## Prepare portfolio of Blackwell Electronix ##
#be_portfolio <- rbind(be_ex[, c(1:3, 17:18)],be_new[c(1:4, 7), c(1:3, 17:18)]) #w/ price, margin, and volume
be_products <- rbind(be_ex[-c(35:41), c(1:2,18)],be_new[c(1:4, 7), c(1:2,18)])
pred_vol_new <- c(152, 267, 225, 77, 734)
be_products[c(74:78), 3] <- pred_vol_new
names(be_products) <- c("category", "ProductNum", "volume")
be_categories <- as.character(unique(be_products$category))   #not required
be_portfolio <- setNames(data.frame(matrix(ncol = length(be_categories), nrow = 0)), c(be_categories))
vol_agg <- data.frame(aggregate(be_products$volume, by = list(be_products$category), FUN = sum))
              
for (i in be_categories) {
  be_portfolio[1, which(colnames(be_portfolio) == i)] <- vol_agg[which(vol_agg$Group.1 == i), 2]
}

## Prepare portfolio of Electronidex ##
df_trans_category <- data.frame(as(trans_category, "matrix"))
ei_portfolio <- setNames(data.frame(matrix(ncol = ncol(df_trans_category), nrow = 0)), c(colnames(df_trans_category)))
for (i in c(1:ncol(df_trans_category))){
  ei_portfolio[1,i] <- length(which(df_trans_category[,i] == TRUE))
}

## Merge visualize portfolios from Blackwell and Electronix
#Merged via excel using be_portoflio and ei_portfolio
merged_portfolios <- read.csv("portfolio_comparison.csv")

gg_vol <- ggplot(merged_portfolios, aes(x = category, y = volume)) + 
          facet_grid(company ~.) + 
          geom_col(aes(fill = category)) + 
          scale_color_brewer(palette = "Dark2") +
          ylim(0, 26000) + 
          theme(strip.text = element_text(face="bold", size = 12), 
                strip.background = element_rect(fill = "lightblue", colour = "black", size = 0.8)) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
          theme(axis.text = element_text(size = 12)) + 
          theme(axis.title = element_text(size = 15)) +
          xlab("Product Category") + ylab("Volume(Sum)") + 
          ggtitle("Product portfolios of Blackwell and Electronix") + 
          theme(title = element_text(size = 15)) + 
          theme(legend.position = "none")

gg_port <- ggplot(merged_portfolios, aes(x = category, y = portion)) + 
           facet_grid(company ~.) + 
           geom_col(aes(fill = category)) + 
           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
           #scale_x_continuous(breaks = round(seq(min(dat$x), max(dat$x), by = 0.5),1)) +
           theme(axis.text = element_text(size = 12)) + 
           theme(axis.title = element_text(size = 15)) +
           xlab("Product Category") + ylab("Portion(in %)") + 
           ggtitle("Product portfolios of Blackwell and Electronix") + 
           theme(title = element_text(size = 15)) + 
           theme(legend.position = "none")

ggarrange(gg_vol, gg_port, ncol = 1, nrow = 2)

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

#Support: 
#-gives an idea of how frequent an itemset is in all the transactions 
#-Value of support helps us identify the rules worth considering for further analysis
#--> wich portion of transaction can be predicted by the rule

#Confidence:
#-defines the likeliness of occurrence of consequent on the cart given that the cart already has the antecedents
#--> how like it is that b is purchased if A is purchased as well

##Out of the box
#rules cover 10% of the transactions(supp = .1) and are 80% correct (conf = .8)
rules <- apriori(transactions_raw, parameter = list(supp = 0.0007, conf = 0.90, minlen = 2, maxlen = 10)) #first rules using supp = 0.001 and
#0.0009 0.95 --> 98 rules
inspect(rules)

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


