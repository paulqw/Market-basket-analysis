#### Load libraries & Setup ####
install.packages("pacman")
library(pacman)
p_load(doParallel, here, readr, rstudioapi,        #parallel computing, relative path
       caret, C50, caretEnsemble, mboost, mlr, Metrics, randomForest, 
       ParamHelpers, hydroGOF, #Classification and Regression
       cluster, corrplot, fpc, e1071, recipes, Hmisc, #Clustering: Corr visulization, Clustering & SVM, 
       ggplot2, ggpubr, RColorBrewer, lattice, #Visualization
       party, reshape, 
       ade4, inum,   #Cleaning, Preprocessing
       FactoMineR, factoextra, #PCA, MCA
       plyr, dplyr, tidyr, tidyverse, textclean,
       arules, arulesViz,    # ASsociation Rules Mining: analyzing and visualize transactional data
       markdown, shiny,  tinytex, rmdformats, knitr #html docu, dashboard, Latex for PDF docu
       #MASS, BBmisc, asbio,
)

## Enable parallel computing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

## Disable scientific notation
options(scipen = 999)

## File directory
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()
setwd("..")
setwd("Data")

#### 0. Loading data ####
## Blackwell Electronics ##
## Load data on sales volume 
be_ex <- read.csv(file = "../Data/blackwell_existing.csv", header = TRUE)
be_new <- read.csv(file = "../Data/blackwell_new.csv", header = TRUE)

## Electronidex ##
## Load transaction into class transaction (sparse matrix)
transactions_raw <- read.transactions(file = "../Data/ElectronidexTransactions2017.csv", 
                                      header = FALSE, format = "basket", sep = ",", rm.duplicates = TRUE)# rm.duplicates = FALSE)

## Load transaction into class df
df_raw <- read.csv(file = "../Data/ElectronidexTransactions2017.csv", header = FALSE)

## Load itemlevels (new name, brand, category) into class df
df_names <- read.csv(file = "../Data/List_il_new.csv", header = TRUE)

brand <- read.csv(file = "../Data/il_brand.csv", header = TRUE)
brand_vector <- as.character(brand$brand)

## Safe itemNames
write.table(itemLabels(transactions_raw), file = "../Data/itemLabels_raw.csv", sep = ",")


#### 1. Exploration and preparation of data #### 
## Amount of transactions & avg. transaction size
paste(length(transactions_raw))
paste(sum(size(transactions_raw))/length (transactions_raw))  #Avg. transaction size

## Create item frequency plot
itemFrequencyPlot(transactions_raw, topN = 20, type = "absolute", horiz = TRUE, 
                                  col = brewer.pal(8,'Pastel2'), main = "Absolute frequency of Top 20 items") 

## Create image of distribution for random items
image(sample(transactions_raw, size = 500)) # of Transactions you'd like to plot))

## Change data type of df to character 
for(i in c(1:32)){
  df_raw[,i] <- as.character(df_raw[,i])
}
str(df_raw)

## Change data type of df itemLevels
for(i in c(1:ncol(df_names))){
  df_names[, i] <- as.character(df_names[, i])
}

## Turn transaction matrix via logical into binary
df_trans_log <- as.data.frame(as(transactions_raw, "matrix"))
df_trans_bin <- df_trans_log

for (i in c(1:ncol(df_trans_bin))){
  df_trans_bin[, i] <- as.integer(df_trans_bin[, i]) 
} 

## Get original item labels (product names)
il_raw <- itemLabels(transactions_raw)  

## Add brand level (brand search) to item label on level product
df_names[, "brand_found"] <- NA
for (i in c(brand_vector)) {
  for (j in c(1:125)) {
    items_found <- agrep(as.character(i), df_names[, which(colnames(df_names) == "il_raw")],  max.distance = 0, 
                         ignore.case = TRUE, value = FALSE)
    df_names[c(items_found), which(colnames(df_names) == "brand_found")] <- i
  }
}

## Rename items to new itemnames --> aggregate itemLevels
df_test <- df_raw
for (k in c(1:125)) {
  on <- as.character(df_names[k, which(colnames(df_names) == "il_raw")])
  nn <- as.character(df_names[k, which(colnames(df_names) == "il_new")])
  for (i in c(1:32)){
    for (j in c(1:9835))
      df_test[j, i] <- replace(df_test[j, i], df_test[j, i] == on, nn)
  }
}

## Turn item labels of level brand and category in lexicographical order
## Brand
df_brand_ordered <- df_names[c(order(df_names$brand)), ] 
transactions_raw@itemInfo$brand <- df_brand_ordered$brand
trans_brand <- arules::aggregate(transactions_raw, by = "brand")
df_trans_brand <- data.frame(as(trans_brand, "matrix"))

## Category
df_cat_ordered <- df_names[c(order(df_names$category)), ]
transactions_raw@itemInfo$category <- df_cat_ordered$category
trans_category <- arules::aggregate(transactions_raw, by = "category")
df_trans_category <- data.frame(as(trans_category, "matrix"))


#### 2. Comparison of product portfolio ####
## Prepare portfolio of Blackwell Electronix(be)
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

## Prepare portfolio of Electronidex(ei)
ei_portfolio <- setNames(data.frame(matrix(ncol = ncol(df_trans_category), nrow = 0)), c(colnames(df_trans_category)))
for (i in c(1:ncol(df_trans_category))){
  ei_portfolio[1,i] <- length(which(df_trans_category[,i] == TRUE))
}

## Merge be_portfolio and ei_portfolio using excel
merged_portfolios <- read.csv(file = "../Data/portfolio_comparison.csv", header = TRUE)

## Visualize portfolios from Blackwell and Electronix
plot_compare <- ggplot(merged_portfolios, aes(x = category, y = portion)) + 
                      facet_grid(company ~.) + 
                      geom_col(aes(fill = category)) + 
                      scale_color_brewer(palette = "Dark2") +
                      ylim(0, 60) + 
                      theme(strip.text = element_text(face="bold", size = 12), 
                            strip.background = element_rect(fill = "lightblue", colour = "black", size = 0.8)) + 
                      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
                      theme(axis.text = element_text(size = 12)) + 
                      theme(axis.title = element_text(size = 15)) +
                      xlab("Product Category") + ylab("Volume(Sum)") + 
                      ggtitle("Product portfolio of Blackwell and Electronix") + 
                      theme(title = element_text(size = 15)) + 
                      theme(legend.position = "none")

  
#### 3. Analysis on customer segments (B2C, B2B) ####
## Extract 'big products' (PC and Laptop)
bigprod <- c("Acer Aspire", "Alienware Laptop", "Apple MacBook Air",
             "Apple MacBook Pro", "ASUS Chromebook", "Dell Laptop",
             "Eluktronics Pro Gaming Laptop", "HP Laptop", "HP Notebook Touchscreen Laptop",
             "LG Touchscreen Laptop", "Acer Desktop", "ASUS Desktop", "CYBERPOWER Gamer Desktop",
             "Dell 2 Desktop", "Dell Desktop", "HP Desktop", "iMac",
             "Intel Desktop", "Lenovo Desktop Computer")
colnos_big <- c()

## Finde transactions for PC and/or laptop
for (i in bigprod) {
  colnos_big <- c(colnos_big, which(colnames(df_trans_bin) == i))
}

colnos_big
valueprod <- rowSums(df_trans_bin[, c(colnos_big)])
df_comparison <- df_trans_bin
df_comparison$valueprods <- valueprod

others <- rowSums(df_trans_bin[, -c(colnos_big)])
df_comparison$others <- others

## Extract 'printers'
printers <- c("Brother Printer", "Canon Office Printer", "DYMO Label Manker",
              "Epson Printer", "HP Wireless Printer")

colnos_print <- c()

for (i in printers) {
  colnos_print <- c(colnos_print, which(colnames(df_trans_bin) == i))
}

colnos_print
printers <- rowSums(df_trans_bin[, c(colnos_print)])
df_comparison$printers <- printers

## Extract 'monitors'
monitors <- c("Acer Monitor", "AOC Monitor", "ASUS 2 Monitor",
              "ASUS Monitor", "Dell Monitor", "HP Monitor",
              "LG Monitor", "Samsung Monitor", "Sceptre Monitor",
              "ViewSonic Monitor"
)

colnons_monit <- c()

for (i in monitors) {
  colnons_monit <- c(colnons_monit, which(colnames(df_trans_bin) == i))
}

colnons_monit
monitors <- rowSums(df_trans_bin[, c(colnons_monit)])
df_comparison$monitors <- monitors

Trsize <- c()

for (i in 1:nrow(df_trans_bin)) {
  Trsize <- c(Trsize, sum(df_comparison[i,-c(126:128)]))
}

df_comparison$tsize <- Trsize

## Exclude transactions containing zero items
df_comparison <- df_comparison[-c(which(df_comparison$tsize == 0)),]

## Create df for cluster analysis
clusterdata_big <- c(df_comparison$valueprods, df_comparison$tsize)
clusterdata_others <- c(df_comparison$others, df_comparison$tsize)
clusterdata_bigothers <- c(df_comparison$valueprods, df_comparison$others)
clusterdata_print <- c(df_comparison$printers, df_comparison$tsize)
clusterdata_monit <- c(df_comparison$monitors, df_comparison$tsize)

## Apply cluster algorithm
trans_kmeans_big <- kmeans(clusterdata_big, 3)
trans_kmeans_others <- kmeans(clusterdata_others, 3)
trans_kmeans_bigothers <- kmeans(clusterdata_bigothers, 3)
trans_kmeans_print <- kmeans(clusterdata_print, 2)
trans_kmeans_monit <- kmeans(clusterdata_monit, 2)

bigdeals <- which(df_comparison$tsize >= 10)

Bigdeals.df <- df_comparison[c(bigdeals),]

## Visualization of clustering 
plot_clus_value <- plot(x = df_comparison$valueprods, y = df_comparison$tsize, pch = trans_kmeans_big$cluster) + 
                         points(trans_kmeans_big$centers, pch = 8, cex =3)

plot_clus_others <- plot(x = df_comparison$others, y = df_comparison$tsize, pch = trans_kmeans_others$cluster) + 
                          points(trans_kmeans_others$centers, pch = 8, cex =3)

plot_clust_monitors <- plot(x = df_comparison$monitors, y = df_comparison$tsize, pch = trans_kmeans_monit$cluster) + 
                            points(trans_kmeans_monit$centers, pch = 8, cex =3)

plot_clus_big <- ggplot(data = Bigdeals.df, aes(x = monitors, y= tsize)) + geom_point()

## Split total transactions into b2c and b2b by cutting values defined during cluster analysis
twocpus <- which(df_comparison$valueprods >= 2)
twoprints <- which(df_comparison$printers >= 2)
twomons <- which(df_comparison$monitors >= 2)
tensize <- which(df_comparison$tsize >= 10)

rows_b2b_trans <- unique(c(twocpus, twoprints, twomons, tensize))

## Create seperate transaction objects for b2c and b2b
trans_b2c <- as(df_comparison[-c(rows_b2b_trans), -c(126:130)] == 1, "transactions")
trans_b2b <- as(df_comparison[c(rows_b2b_trans), -c(126:130)] == 1, "transactions")

## Visualization of item frequencies for each customer segment
itemFrequencyPlot(trans_b2c, topN = 10, type = "absolute", horiz = TRUE, 
                                col = brewer.pal(8,'Pastel2'), main = "Frequency of Top 10 items - B2C", 
                                xlab = "Item frequency")
itemFrequencyPlot(trans_b2b, topN = 10, type = "absolute", horiz = TRUE, 
                                col = brewer.pal(8,'Pastel2'), main = "Frequency of Top 10 items - B2B", 
                                xlab = "Item frequency")


#### 4. Association rules mining ####
## Define function to create rules with apriori, remove redundant rules, and sort
rules_func <- function (trans_data, support, confidence, min){
  rules <- apriori(trans_data, parameter = list(supp = support, conf = confidence, minlen = min, maxlen = 10) 
                   #, appearance = list(rhs = c("Apple"))
  )
}

## Define functions to store rules in df and merge levels
df_rules_func <- function(rules_sort_x) {
  
  df_rules_x <- data.frame(lhs = labels(lhs(rules_sort_x)), rhs = labels(rhs(rules_sort_x)), 
                           rules_sort_x@quality)
}

## Create rules for 5 transaction objects
rules_sort_product <- rules_func(transactions_raw, support = 0.01, confidence = 0.4, min = 2)
rules_sort_category <- rules_func(trans_category, support = 0.05, confidence = 0.4, min = 2)
rules_sort_brand <- rules_func(trans_brand, support = 0.05, confidence = 0.40, min = 1)
rules_sort_b2c <- rules_func(trans_b2c, support = 0.001, confidence = 0.40, min = 2)
rules_sort_b2b <- rules_func(trans_b2b, support = 0.01, confidence = 0.55, min = 2)

## Create dfs for 5 transaction objects
df_rules_product <- df_rules_func(rules_sort_product)
df_rules_category <- data.frame(lhs = labels(lhs(rules_sort_category)), rhs = labels(rhs(rules_sort_category)), 
                                rules_sort_category@quality)
df_rules_brand <- data.frame(lhs = labels(lhs(rules_sort_brand)), rhs = labels(rhs(rules_sort_brand)), 
                             rules_sort_brand@quality)
df_rules_b2c <- df_rules_func(rules_sort_b2c)
df_rules_b2b <- df_rules_func(rules_sort_b2b)

## Explore rules
arulesViz::ruleExplorer(rules_sort_product)

## Select specific rules per item
ItemRules <- subset(rules_sort_product, items %in% "iMac")
inspect(ItemRules)

## Visualize rules using scatterplot on performance metrics
plot_scatter_prod <- plot(rules_sort_product, main = "Scatterplot - product level")
plot_scatt_cat <- plot(rules_sort_category, main = "Scatterplot - category level")
plot_Scatt_brand <- plot(rules_sort_brand, main = "Scatterplot - brand level")
plot_scatt_b2c <- plot(rules_sort_b2c, main = "Scatterplot - segment level b2c")
plot_Scatt_b2b <- plot(rules_sort_b2b, main = "Scatterplot - segment level b2b")


#### 5. Selection of rules ####
## Store rules selected in seperate dfs
df_rules_final <- NULL

df_rules_final <- cbind(data.frame(No = 1:5, level = "product"), 
                        df_rules_product[c(22, 19, 66, 65, 54),])

df_rules_final <- rbind(df_rules_final, cbind(data.frame(No = 6, level = "category"),
                                              df_rules_category[c(151),]))

df_rules_final <- rbind(df_rules_final, cbind(data.frame(No = 7:9, level = "brand"),
                                              df_rules_brand[c(98, 100, 102),]))

df_rules_final <- rbind(df_rules_final, cbind(data.frame(No = 10:16, level = "b2c"),
                                              df_rules_b2c[c(2, 4, 6, 11, 18, 19, 20),]))

df_rules_final <- rbind(df_rules_final, cbind(data.frame(No = 17:20, level = "b2b"),
                                              df_rules_b2b[c(88, 99, 101, 110),]))

## Visualize one rule for each transaction set
plot(rules_sort_product[22], method = "graph", 
                       control = list(type = "items", reorder = TRUE), main = "Rule - product level")  

plot(rules_sort_category[151], method = "graph", 
                      control = list(type = "items", reorder = TRUE), main = "Rule - category level")  

plot(rules_sort_brand[c(98, 100)], method = "graph", 
                        control = list(type = "items", reorder = TRUE), main = "Rule - brand level")  

plot(rules_sort_b2c[20], method = "graph", 
                      control = list(type = "items", reorder = TRUE), main = "Rule - B2C")  

plot(rules_sort_b2b[101], method = "graph", 
                      control = list(type = "items", reorder = TRUE), main = "Rule - B2B")



#### Creating presentation ####
## To pptx
getwd()
setwd("..")
setwd("Presentation")

CEO <- read_pptx("C:/Users/Paul/Desktop/Ubiqum/Module 2/2.4 Discover Associations Between Products/Market basket analysis/Market-basket-analysis/Presentation/190726_CEO Presentation.pptx")
layout_summary(CEO)




plot.new()
itemFrequencyPlot(trans_b2c, topN = 10, type = "absolute", horiz = TRUE, 
                                  col = brewer.pal(8,'Pastel2'), main = "Frequency of Top 10 items - b2c", 
                                  xlab = "Item frequency")
plot_iF_b2c <- recordPlot()

plot.new()
itemFrequencyPlot(trans_b2b, topN = 10, type = "absolute", horiz = TRUE, 
                  col = brewer.pal(8,'Pastel2'), main = "Frequency of Top 10 items - b2b", 
                  xlab = "Item frequency")
plot_iF_b2b <- recordPlot()

plot.new()
itemFrequencyPlot(transactions_raw, topN = 20, type = "absolute", horiz = TRUE, 
                                   col = brewer.pal(8,'Pastel2'), main = "Absolute frequency of Top 20 items")
plot_iF_total <- recordPlot()

plot.new()
image(sample(transactions_raw, size = 500))
plot_image <- recordPlot()


CEO <- add_slide(CEO, layout = "Title Only", master = "Office Theme")
CEO <- ph_with(x = CEO, value = plot_iF_total, location = ph_location_fullsize())

CEO <- add_slide(CEO, layout = "Title Only", master = "Office Theme")
CEO <- ph_with(x = CEO, value = plot_iF_b2c.pryr, location = ph_location_fullsize())

CEO <- add_slide(CEO, layout = "Title Only", master = "Office Theme")
CEO <- ph_with(x = CEO, value = plot_iF_b2b, location = ph_location_fullsize())

CEO <- add_slide(CEO, layout = "Title Only", master = "Office Theme")
CEO <- ph_with(x = CEO, value = plot_compare, location = ph_location_fullsize())

CEO <- add_slide(CEO, layout = "Title Only", master = "Office Theme")
CEO <- ph_with(x = CEO, value = plot_image, location = ph_location_fullsize())

print(CEO, target = "C:/Users/Paul/Desktop/Ubiqum/Module 2/2.4 Discover Associations Between Products/Market basket analysis/Market-basket-analysis/Presentation/190726_CEO Presentation.pptx") 

doc <- add_slide(doc, layout = "Title and Content",
                 master = "Office Theme")
gg_plot <- test_plot
doc <- 
print(doc, target = "C:/Users/Paul/Desktop/Ubiqum/190726_CEO Presentation.pptx") 

