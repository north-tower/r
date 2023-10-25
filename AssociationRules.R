library(arules)
library(arulesViz)
data('Groceries')

#Parameters
#support: a numeric value for the minimal support of an item set (default:0.1)
#minlen: an integer value for the minimal number of items per item set (default:1 item)
#maxlen:an integer value for the maximal number of items per item set (default: 10 items)
#confidence:a numeric value for the minimal confidence of rules (default:0.8)
#smax:a numeric value for the maximal support of rules (default: 1)

grocery_rules <- apriori(data=Groceries, 
                         parameter = list(support = 0.01, confidence = 0.5),
                         control = list(verbose=F))
inspect(head(grocery_rules,5))
inspect(head(sort(grocery_rules, by = "confidence"), 3))

# Find rules that show what products are bought before buying "whole milk" 
wholemilk_rules <- apriori(data=Groceries, 
                           parameter=list (support=0.001,confidence=0.08), 
                           appearance = list(default="lhs",rhs="whole milk"),
                           control = list(verbose=F))
inspect(head(sort(wholemilk_rules, by = "confidence"), 3))
#To find out what the Customers who bought ‘Whole Milk’ also bought
wholemilk_rules <- apriori(data=Groceries, 
                           parameter=list (support=0.001,confidence=0.08), 
                           appearance = list(default="rhs",lhs="whole milk"),
                           control = list(verbose=F))
inspect(head(sort(wholemilk_rules, by = "confidence"), 3))

#If you want to get stronger rules, increase the confidence. 
#If you want lengthier rules increase the maxlen parameter. 
#If you want to eliminate shorter rules, decrease the minlen parameter.

#Convert data frame to transactional data
data("AdultUCI")
AdultUCI <- data.frame(lapply(AdultUCI, function(x){as.factor(x)}))
transactional_data <- as(AdultUCI, "transactions")

#Visualization with arulesViz library
plot(grocery_rules)
plot(grocery_rules, measure=c("support", "lift"), shading = "confidence")
plot(grocery_rules, method = "two-key plot")
plot(grocery_rules, method = "grouped")
