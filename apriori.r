library(arules)
library(arulesViz)


txn=list(
  c("Milk","Bread"),
  c("Bread","Butter"),
  c("Milk","Butter"),
  c("Milk","Bread","Butter"),
  c("Bread")
)

txn=as(txn,"transactions")
rules=apriori(txn,parameter =list(supp=0.4,conf=0.6))
rules
inspect(rules)

rules=apriori(txn,parameter = list(supp=0.4,conf=0.6,target="frequent itemsets"))
inspect(rules)

support <- support(itemsets(list(c("Milk","Bread"))), txn)
support

a=apriori(txn,parameter=list(supp=0,conf=0),appearance =list(lhs="Milk",rhs="Bread"))
inspect(a)

plot(rules,method = "graph")