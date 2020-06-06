#################################### QUESTION 1 ####################################

#################################### QUESTION 1 ####################################

# Step 1: Import packages for the exercise
library(tidyverse)
#library(readr)
#library(dplyr)
#library(tidyr)
library(arules)
library(arulesViz)
library(knitr)
library(methods)
library(grid)
setwd("C:/Users/Ming/OneDrive - National University of Singapore/NUS/Year 3/Semester 2/PSA Data Analytics Biz Challenge/Dataset")
PN_TRANS_feb2020 <- read.csv("PN_TRANS_feb2020.txt", comment.char="#")
PN_TRANS_jan2020 <- read.csv("PN_TRANS_jan2020.txt", comment.char="#")
PN_WEB_ACCESS_06JAN2020 <- read.csv("PN_WEB_ACCESS_06JAN2020.csv")
PN_WEB_ACCESS_07JAN2020 <- read.csv("PN_WEB_ACCESS_07JAN2020.csv")
PN_WEB_ACCESS_08JAN2020 <- read.csv("PN_WEB_ACCESS_08JAN2020.csv")
PN_WEB_ACCESS_09JAN2020 <- read.csv("PN_WEB_ACCESS_09JAN2020.csv")
PN_WEB_ACCESS_10JAN2020 <- read.csv("PN_WEB_ACCESS_10JAN2020.csv")
PN_WEB_ACCESS_11JAN2020 <- read.csv("PN_WEB_ACCESS_11JAN2020.csv")
# 
# PN_TRANS_jan2020_clean <- read.csv("PN_TRANS_jan2020_clean.csv")
# str(PN_TRANS_jan2020_clean)
# unique(PN_TRANS_jan2020_clean$Category)
# unique(PN_TRANS_jan2020_clean$Subcategory)
# unique(PN_TRANS_jan2020_clean$Website)
# 
# PN_TRANS_feb2020_clean <- read.csv("PN_TRANS_feb2020_clean.csv")
# str(PN_TRANS_feb2020_clean)
# unique(PN_TRANS_feb2020_clean$Category)
# unique(PN_TRANS_feb2020_clean$Subcategory)
# unique(PN_TRANS_feb2020_clean$PRI_SEC)
# head(PN_TRANS_feb2020)
# 
# trans <- rbind(PN_TRANS_jan2020_clean, PN_TRANS_feb2020_clean)
# user.freq <- table(trans$USERDATA)
# user.freq
# View(user.freq)
# LOGDATE <- colnames(trans)[1]
# 
# hi <- trans %>%
#   mutate(Date = dmy(str_sub(LOGDATE, 1, 9))) %>%
#   mutate(Time = hms(str_sub(LOGDATE,11,18)))

# Step 2: Import datasets
trans <- read.csv("trans_combined_cleaned.csv")

x <- sample(1:nrow(trans),50000)
small <- trans[x,]
"USERID" <- colnames(small)[1]
colnames(small)

# Step 3: Prepare Datasets
order_baskets <- trans %>% 
  mutate(CATEGORY = str_trim(CATEGORY)) %>%
  group_by(ï..USERID, DATE) %>%
  summarise(basket = as.vector(list(CATEGORY)))

order_baskets <- trans %>% 
  mutate(CATEGORY = str_trim(CATEGORY)) %>%
  group_by(USERID, DATE) %>%
  summarise(basket = as.vector(list(CATEGORY)))


head(order_baskets)
library(data.table)
df <- setDT(order_baskets)
colnames(df) <- c("USERID", "DATE", "basket")
fwrite(df,"order_baskets.csv")

# compute transactions
transactions <- as(order_baskets$basket, "transactions")

# Step 4: Visualize & examine the order data (1)

transactions <- as(baskets$basket, "transactions")
transactions <- as(baskets$basket, "transactions")

# Draw a histogram
hist(size(transactions),xaxt="n",breaks = 0:16, ylim=c(0,12000), main = "Number of Categories per session", xlab = "No. of Categories")
axis(1, at=seq(0,20,by=1), cex.axis=0.8)
mtext(paste("Total:", length(transactions), "Sessions,", sum(size(transactions)), "Categories"))

# Step 4: Visualize & examine the order data (2)

# We can see which items are frequently purchased. 
# The threshold is 2 displaying that products included at least 2 % of all the baskets contain. 

frequencies <- itemFrequency(transactions, type="a")
support <- 0.02
#support <- 0.01
freq_items <- sort(frequencies, decreasing = FALSE)
freq_items <- freq_items[freq_items>support*length(transactions)]

# alternative barplot
par(mar=c(2,10,2,2)); options(scipen=5)
barplot(freq_items, horiz=T, las=1, main="Frequent Categories", cex.names=.8, xlim=c(0,30000))
mtext(paste("support:",support), padj = .8)
abline(v=support*length(transactions), col="red")


# Step 4: Visualize & examine the order data (3)

# Computing the frequent itemsets. 

support <- 0.1
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=3), control = list(verbose = FALSE))

sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = FALSE))

# Draw a barplot
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.3), horiz = T, las = 2, cex.names = .8)

# alternative barplot
par(mar=c(5,25,2,2)+.1)
sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = FALSE))
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.3), horiz = T, las = 2, cex.names = .8)
title("Frequent Itemsets", adj = 0)
mtext(paste("support:",support), padj = .8, adj = 0.15)

# Step 5: Association Rule Mining

rules1 <- apriori(transactions, parameter=list(supp = 0.05, conf = 0.6, minlen = 2), control = list(verbose=FALSE))
rules2 <- apriori(transactions, parameter=list(supp = 0.05, conf = 0.8, maxlen = 3), control = list(verbose=FALSE))
rules3 <- apriori(transactions, parameter=list(supp = 0.05, conf = 0.8, minlen = 4), control = list(verbose=FALSE))
summary(rules1)
summary(rules2)
summary(rules3)

top100lifts1 <- head(sort(rules1, decreasing=TRUE,by="lift"),100) 
top25lifts1 <- head(sort(rules1,decreasing=TRUE, by="lift"), 25)
top100con1 <- head(sort(rules1, decreasing=TRUE,by="confidence"),100) 
top25con1 <- head(sort(rules1, decreasing=TRUE,by="confidence"),25) 
top100sup1 <- head(sort(rules1, decreasing=TRUE,by="support"),100) 
top25sup1 <- head(sort(rules1, decreasing=TRUE,by="support"),25) 

top100lifts2 <- head(sort(rules2, decreasing=TRUE,by="lift"),100) 
top25lifts2 <- head(sort(rules2,decreasing=TRUE, by="lift"), 25)
top100con2 <- head(sort(rules2, decreasing=TRUE,by="confidence"),100) 
top25con2 <- head(sort(rules2, decreasing=TRUE,by="confidence"),25) 
top100sup2 <- head(sort(rules2, decreasing=TRUE,by="support"),100) 
top25sup2 <- head(sort(rules2, decreasing=TRUE,by="support"),25) 

top100lifts3 <- head(sort(rules3, decreasing=TRUE,by="lift"),100) 
top25lifts3 <- head(sort(rules3,decreasing=TRUE, by="lift"), 25)
top100con3 <- head(sort(rules3, decreasing=TRUE,by="confidence"),100) 
top25con3 <- head(sort(rules3, decreasing=TRUE,by="confidence"),25) 
top100sup3 <- head(sort(rules3, decreasing=TRUE,by="support"),100) 
top25sup3 <- head(sort(rules3, decreasing=TRUE,by="support"),25)
#1: 0.05s/0.6c/2min, 2: 0.05s/0.8c/3max, 3: 0.05s/0.8s/4min
inspect(top25lifts1)
inspect(top25lifts2)
inspect(top25lifts3)

inspect(top25con1)
inspect(top25con2)
inspect(top25con3)

inspect(top25sup1)
inspect(top25sup2)
inspect(top25sup3)

plot(top100lifts, engine = "interactive", shading = "lift", control = list(verbose=FALSE))


plot(top25rules, engine = "htmlwidget", shading = "lift", control = list(verbose=TRUE))
plot(top25rules, engine = "interactive", shading = "lift", control = list(verbose=FALSE))
plot(top100rules, engine = "interactive", shading = "lift")
plot(rules1, method = "graph", engine = "htmlwidget", shading = "lift", reorder = TRUE)
rules_lhs <-head(sort(rules1, decreasing=TRUE,by="confidence"),10) 
inspect(rules_lhs) 


#Subrules
rules_lhs <-head(sort(rules1, decreasing=TRUE,by="lift"),10) 
inspect(rules_lhs) 
plot(rules_lhs,method="graph",shading="lift")


rules_lhs <- top25rules
plot(rules_lhs, method = "graph", engine = "interactive", shading = "lift")
plot(rules_lhs, method = "graph", engine = "htmlwidget", shading = "lift")
plot(rules_lhs, engine = "interactive", shading = "lift")
plot(rules_lhs, method = "two-key plot", engine = "htmlwidget", shading = "lift")
plot(rules_lhs, method = "grouped matrix", engine = "default", shading = "lift")
plot(rules_lhs, method = "paracoord", shading = "lift")
plot(rules_lhs, method = "paracoord", shading = "lift", control=list(verbose = TRUE))
plot(top100rules, method = "paracoord", engine = "default", shading = "lift", control=list(redorder = TRUE))

# Function to create the average
averagetor <- function(corpro) {
  a <- scale(corpro[, which(colnames(corpro) %in% c("support", "confidence", "lift"))])
  average <- a[, which(colnames(a) == "support")] + a[, which(colnames(a) == "confidence")] + a[, which(colnames(a) == "lift")]
  corpro <- cbind(corpro, average)
  corpro <- corpro[order(-average), ]
  return(corpro)
}

# The most populars products
rules1_sup <- inspect(head(sort(rules1, by = "support"), 10))
# Items that have high chances of being bought together
rules1_con <- inspect(head(sort(rules1, by = "confidence"), 10))
# Lift
rules1_lift <- inspect(head(sort(rules1, by = "lift"), 10))
rules1cat <- rbind(rules1_sup, rules1_con, rules1_lift)
avg <- averagetor(rules1cat)
head(avg)
foo <- avg[, which(colnames(avg) %in% c("lhs", "Var.2", "rhs", "support", "lift", "confidence", "average"))]
head(foo[order(foo$average, decreasing = TRUE),],10)

# Step 6: Association Rule Mining & Display Results (1)


# You can use both. 
summary(quality(rules1)) 
summary(rules1) 

# Step 6: Association Rule Mining & Display Results (2)

# get the actual result

inspect(sort(rules1, by="lift")[1:15])


#################################### QUESTION 2 ####################################

#################################### QUESTION 2 ####################################
str(trans)
colnames(trans) <- c("USERID", "ORGCODE_N", "DATE", "HOUR", "MIN", "SEC", "CATEGORY", "SUBCATEGORY", "PRI_SEC", "TIME")
str(trans)
picked <- trans %>% select(USERID, ORGCODE_N, DATE, TIME, CATEGORY)
pee <- picked %>%
  group_by(USERID, DATE) %>%
  arrange(TIME) %>%
  mutate(timediff = as.numeric(difftime(TIME, lag(TIME), units = "mins")),
         prev_cat = lag(CATEGORY)) %>%
  filter(timediff <= 10) %>%
  unite("cat_and_prevcat",c("CATEGORY", "prev_cat"), sep = ",", remove = F) %>%
  summarise(consecutive = paste(cat_and_prevcat, collapse = ",")) 

filter(pee, USERID == "0jas3")

hi <- pee %>%
  group_by(USERID, DATE) %>%
  mutate(consecutive = paste(c(unique(unlist(strsplit(consecutive,",")))), collapse = ","))

filter(hi, USERID == "0jas3")
