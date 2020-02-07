#DISCOVER ASSOCIATIONS BETWEEN PRODUCTS

# Load libraries
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load("readr","ggplot2","arules","arulesViz","plotly","RColorBrewer","dplyr")

# Upload data and set seed + set number of displayed significant
Transactions<- read.transactions("ElectronidexTransactions2017.csv",format = "basket", 
                                 sep = ",", rm.duplicates = TRUE)
set.seed(123)
options(digits = 2)

# Upload the product category data
productCategory <- read.csv("ProductCategories.csv")
productCategory<- as.data.frame(productCategory)

#Inspect dataset

inspect(Transactions)
length (Transactions) # Number of transactions.
size(Transactions) # Number of items per transaction
LIST(Transactions) # Lists the transactions by conversion
itemLabels(Transactions)# To see the item labels
summary(Transactions)


#Visualize dataset
itemFrequencyPlot(Transactions,topN=10, type="absolute")

# Removing transaction size 0

Transactions <-Transactions[which(size(Transactions)!=0)]

##########################################################
##   Compare Blackwell and Electronidex portofolio      ##
##########################################################


# Prepare Blackwell product data
blackwellNew <- read.csv("blackwellNew.csv")
blackwellNew <- blackwellNew[c(2,3)]

View(blackwellNew)
blackwellOld <- read.csv("blackwellOld.csv")
blackwellOld <- blackwellOld[c(1,2)]

blackwellProductTypes <- rbind(blackwellOld, blackwellNew)
blackwellProductTypes <- data.frame(blackwellProductTypes$ProductType, blackwellProductTypes$ProductNum)


blackwellProductTypes$Company <- "Blackwell"
colnames(blackwellProductTypes)[1] <- "Category"
colnames(blackwellProductTypes)[2] <- "Product"

# Prepare Electronidex product data 

productCategoryPlot <- read.csv("ProductCategoriesForPlot.csv")
productCategoryPlot<- as.data.frame(productCategoryPlot)
productCategoryPlot$Company <- "Electronidex"

# Merge dataframes

ProductTypes <- rbind(productCategoryPlot, blackwellProductTypes)


# Visually compare Blackwell and Electronidex product offering 

ggplot(tally(group_by(ProductTypes, Category, Company)),aes(Category, n, fill = Category)) + 
  geom_col() + facet_grid(Company ~ .) + theme(axis.text.x = element_text(angle = 90, hjust = 0, size =12))+
  coord_cartesian()+
  guides(fill=FALSE) + xlab("") + ylab("Inventory Size") + 
  ggtitle("Visual Representation of Portofolios")


#########################################################################################

# Adding the labels
Transactions@itemInfo$category <- productCategory$Category


# TOP 10 most frequent products by product

itemFrequencyPlot(Transactions, topN = 10, col = rainbow(4), type="absolute")
head(Transactions@itemInfo)

# Plot nr items in a transaction

transactionSize <- data.frame(size(Transactions))

ggplot(transactionSize,aes(size.Transactions.))+
      geom_bar(fill="red")+
      labs(title = "Size of transactions")


################################################
##          Association rules                  ##
################################################


rules <- apriori(Transactions, parameter=list(minlen=2, support=0.001, confidence=0.4))
rules
inspect(head(rules, n = 10, by ="lift"))
inspect(head(rules, n = 3, by ="lift"))
is.redundant(rules)
rules <- rules[!is.redundant(rules)]
nrow(Transactions)

#Rules without laptops,desktops and computers

`%!in%` = Negate(`%in%`)
NoLaptopRules <- subset(rules,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                            "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop"))
inspect(NoLaptopRules[1:10])


#Top 10 rules

inspect(rules[1:10])
top.rules <-(rules[1:10])
inspect(head(top.rules, by = "confidence"))
inspect(head(top.rules, by = "lift"))
inspect(head(top.rules, by = "support"))


#Plot rules

plot(rules, measure = c("support", "lift"), shading = "confidence",jitter=10)
plot(rules, method = "two-key plot")
plot(top.rules, method = "grouped")
plot (NoLaptopRules[1:5]
      , method = "graph", engine = "htmlwidget")


#Improve by inspect model
inspect(sort(rules [1:10], by = "support"))
inspect(sort(rules[1:10], by = "confidence"))
inspect(sort(rules[1:10], by = "lift"))


#Improve and subset model

inspect(sort(top.rules, by = "lift"))
is.redundant(top.rules) #no redundant rules

##########################################
# #        RULES BY PRODUCTS ON LHS      ##
##########################################


# iMac Rules

iMacrules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.1), 
               appearance = list(default="rhs",lhs="iMac"))
inspect(iMacrules)
plot (iMacrules, method = "graph", engine = "htmlwidget")

# iMac no high value on rhs

ruleiMacNoHighValueRhs <- subset(iMacrules ,items %!in% c("HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                              "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                              "Acer Desktop","Apple MacBook Pro","HP Monitor","LG Monitor",'Acer Aspire',"ASUS Monitor",
                                                              "ASUS Chromebook","ASUS 2 Monitor"))
inspect(ruleiMacNoHighValueRhs)

# HP laptop rules

hpLaptoprules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.1), 
                   appearance = list(default="rhs",lhs="HP Laptop"))
inspect(hpLaptoprules)

# Hp no high values on rhs

plot (ruleHpLaptopsnoLaptopsRhs, method = "graph", engine = "htmlwidget")


ruleHpLaptopsnoLaptopsRhs <- subset(hpLaptoprules ,items %!in% c("iMac", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                                 "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                                 "Acer Desktop","Apple MacBook Pro","HP Monitor","LG Monitor",'Acer Aspire',"ASUS Monitor",
                                                                 "ASUS Chromebook"))
 
                                                                 
                                                                                                                                                                                         
inspect(ruleHpLaptopsnoLaptopsRhs)

# CYBERPOWER Gamer Desktop Rules

CPGamingLaptoprules<-apriori(Transactions, parameter=list(supp=0.001,conf = 0.2), 
                       appearance = list(default="rhs",lhs="CYBERPOWER Gamer Desktop"))
inspect(CPGamingLaptoprules)
plot (CPGamingLaptoprules, method = "graph", engine = "htmlwidget")


# Rules without laptops

rulesNoLaptops <- apriori(Transactions, parameter=list(supp=0.001,conf = 0.2),
                          appearance = items %in% "CYBERPOWER Gamer Desktop")

# Rules Airpods rhs 

AirpodRules <- apriori (data=Transactions, parameter=list (supp=0.01,conf = 0.1), appearance = list (default="lhs",rhs="Apple Earpods")) 
inspect(AirpodRules)
# Airpods on rhs high value on lhs

ruleAirpodsHighValueLhs <- subset(AirpodRules ,items %in% c("iMac", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                                 "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                                 "Acer Desktop","Apple MacBook Pro","HP Monitor","LG Monitor",'Acer Aspire',"ASUS Monitor",
                                                                 "ASUS Chromebook","iPad Pro",'Apple Magic Keyboard','Apple Wireless Keyboard'))
inspect(ruleAirpodsHighValueLhs)


#########################################################
##          Split data by categories                   ##
#########################################################

Data_catsplit <- aggregate(Transactions, by = 'category')
itemLabels(Data_catsplit)
library(RColorBrewer)

# Plotting frequency by category

itemFrequencyPlot(Data_catsplit, topN = 10, type = "relative", 
                             col = colorRampPalette(brewer.pal(9, "Paired"))(10), 
                             main = "Categories Relative Item Frequency Plot")



###########################################################
##                Split Data by B2B AND B2C            ####
###########################################################

transactions_B2C <- Transactions[which(size(Transactions)<=4)] #B3B Transactions
transactions_B2B <- Transactions[which(size(Transactions)>4)]  #B2C Transactions
transactions_oneItem <- Transactions[which(size(Transactions)==1)] #Transactions with only one item

# Piechart proportion of customers that are individuals or businesses

transactionSize$customerType <- ifelse(transactionSize$size.Transactions.>4, transactionSize$customerType <- "Business", transactionSize$customerType <- "Private" )

ggplot(transactionSize,aes(x="",fill=customerType))+
      geom_bar()+
      coord_polar(theta = "y") + labs(title="Customer base")

##########################################################
###             Rules by Category                       ##
##########################################################

rulesCat <- apriori(Data_catsplit, parameter=list(minlen=2, support=0.001, confidence=0.4))
TopRulesCat<-(rulesCat[1:10])

inspect(head(TopRulesCat, by="lift"))
plotCat2 <-(head(TopRulesCat, by="lift"))
plot(plotCat2, method = "graph", engine = "htmlwidget")


#Laptops
rulesCat1 <-apriori(Data_catsplit, parameter=list(supp=0.001,conf = 0.2), 
          appearance = list(default="rhs",lhs="Laptop"))
topTenCat<-rulesCat1[1:10]
inspect(head(rulesCat1, by="lift"))

rulesCat1 <-apriori(Data_catsplit, parameter=list(supp=0.001,conf = 0.1), 
          appearance = list(lhs="Laptop",rhs=""))
inspect(rulesCat1)
topTenCat<-rulesCat1[1:10]
inspect(head(rulesCat1, by="conf"))
inspect(topTenCat)
plotCat <-(head(rulesCat1, by="lift"))

plot(plotCat, method = "graph", engine = "htmlwidget")

#Desktop
rulesCat3 <-apriori(Data_catsplit, parameter=list(supp=0.001,conf = 0.2), 
                    appearance = list(default="rhs",lhs="Desktop"))
topTenDesk<-rulesCat3[1:10]
plotDesk<-(head(rulesCat3, by ="lift"))

plot(plotDesk, method = "graph", engine = "htmlwidget")

#Printer Ink
rulesCatPrint <-apriori(Data_catsplit, parameter=list(supp=0.001,conf = 0.2), 
                    appearance = list(default="lhs",rhs="Printer Ink"))
rulesCatPrint[1:10]
topTenPrint<-(head(rulesCatPrint, by="lift"))
inspect(topTenPrint)
plot(topTenPrint, method="grouped")

##########################################################
###             Rules by Type of Customer               ##
##########################################################

### Rules B2B ###

rule_B2B <- apriori(transactions_B2B, parameter=list(supp=0.02,conf = 0.2) )
inspect(rule_B2B)
topTenB2B<-(rule_B2B [1:10])
inspect(head(topTenB2B, by="lift"))

# Rules no Laptops, desktops and computers

rule_B2BnoLaptops <- subset(rule_B2B,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                   "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                   "Acer Desktop","Apple MacBook Pro","HP Monitor"))
inspect(rule_B2BnoLaptops)
inspect(rule_B2B)
topTenNoLap<-(rule_B2BnoLaptops [1:10])
inspect(head(topTenNoLap, by="lift"))

#Rule Apple Earpods B2B

rule_B2BappleEarpods <- apriori(transactions_B2B,parameter=list(supp=0.002,conf = 0.2), 
                                appearance = list(default="rhs",lhs="Apple Earpods"))
inspect(rule_B2BappleEarpods)
inspect(head(rule_B2BappleEarpods[1:10], by="lift"))

# Rule B2B iMac

rule_B2BiMac <- apriori(transactions_B2B,parameter=list(supp=0.002,conf = 0.2), 
            appearance = list(default="rhs",lhs="iMac"))
inspect(rule_B2BiMac)

### Rules B2C ###

rule_B2C <- apriori(transactions_B2C, parameter=list(supp=0.005,conf = 0.1) )
inspect(head(rule_B2C[1:10], by="lift"))


# Rules no Laptops, desktops and computers

rule_B2CnoLaptops <- subset(rule_B2C,items %!in% c("iMac","HP Laptop", "Apple MacBook Air", "CYBERPOWER Gamer Desktop", 
                                                   "Lenovo Desktop Computer","Dell Desktop","ViewSonic Monitor","Eluktronics Pro Gaming Laptop","Samsung Monitor",
                                                   "Acer Desktop","Apple MacBook Pro","HP Monitor"))
inspect(rule_B2CnoLaptops)

#Rule Apple Earpods B2B

rule_B2CappleEarpods <- apriori(transactions_B2C,parameter=list(supp=0.001,conf = 0.1), 
                                appearance = list(default="rhs",lhs="Apple Earpods"))
inspect(rule_B2CappleEarpods, by ="lift")

# Rule iMac B2C

rule_B2CiMac <- apriori(transactions_B2C,parameter=list(supp=0.001,conf = 0.1), 
                        appearance = list(default="rhs",lhs="iMac"))
inspect(rule_B2CiMac)

ruleExplorer(rules,parameter = NULL)


