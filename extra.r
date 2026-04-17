# id3 
library(infotheo)
library(rpart)
library(rpart.plot)

data <- data.frame(
  Contract = c("Month","Month","Year","Year","Month","Year"),
  InternetService = c("DSL","Fiber","DSL","Fiber","DSL","Fiber"),
  PaymentMethod = c("Card","Cash","Card","Cash","Cash","Card"),
  Churn = c("Yes","Yes","No","No","Yes","No")
)
data
data[] <- lapply(data, as.factor)
data
entropy(data$Churn)
entropy(data$Contract)

mutinformation(data$Contract,data$Churn)

model <- rpart(Churn ~ .,data=data,method="class")
rpart.plot(model)
print(model)
plot(model)

# page rank
library(igraph)
edges <- c("A","B", "A","C","B","C", "B","D","C","A","D","C")

edge_matrix <- matrix(edges,ncol=2,byrow=TRUE)
g <- graph_from_edgelist(edge_matrix,directed=TRUE)
# g <- graph(edges,direction=TRUE)
pr <- page_rank(g,damping=0.85)

print(pr$vector)
plot(g,main="Page rank visualization")
top_node <- names(which.max(pr$vector))
print(top_node)

# naivebayes
# code for naivebayes

library(naivebayes)

data <- data.frame(
  Age = c("Young","Young","Middle","Old","Old","Old","Middle","Young","Young","Old"),
  Income = c("High","High","High","Medium","Low","Low","Low","Medium","Low","Medium"),
  Student = c("No","No","No","No","Yes","Yes","Yes","No","Yes","Yes"),
  Credit = c("Fair","Excellent","Fair","Fair","Fair","Excellent","Excellent","Fair","Fair","Fair"),
  Buy = c("No","No","Yes","Yes","Yes","No","Yes","No","Yes","Yes")
)

data[] <- lapply(data,as.factor)

model <- naive_bayes(Buy ~ .,data=data,laplace=1)
print(model)

new_data <- data.frame(Age="Young",Income="Medium",Student="Yes",Credit="Fair")
prediction <- predict(model,new_data)
print(prediction)
print(predict(model,new_data,type="prob"))

# apriori / fp growth 
library(arules)
library(arulesViz)

txn=list(
  c("Milk","Bread"),
  c("Bread","Butter"),
  c("Milk","Butter"),
  c("Milk","Bread","Butter"),
  c("Bread")
)

txn = as(txn,"transactions")
rules = apriori(txn,parameter=list(supp=0.4,conf=0.6))
rules
rules = apriori(txn,parameter=list(supp=0.4,conf=0.6,target="frequent itemsets"))
rules

support <- support(itemsets(list(c("Milk","Bread"))),txn)
a = apriori(txn,parameter=list(supp=0,conf=0),appearance = list(lhs="Milk",rhs="Bread"))
inspect(a)
plot(rules,method="graph")

transactions_list <- list(
  c("Laptop","Mouse","Keyboard"),
  c("Laptop","Mouse"),
  c("Mouse","Keyboard"),
  c("Laptop","Keyboard"),
  c("Laptop","Mouse","Keyboard"),
  c("Mouse")
)

trans = as(transactions_list,"transactions")
inspect(trans)
freq_eclat_40 <- eclat(trans,parameter=list(supp=0.4))

inspect(freq_eclat_40)
itemFrequency(trans)

rules <- ruleInduction(freq_eclat_40,trans,confidence=0.6)
inspect(rules)

freq_eclat_60 <- eclat(trans,parameter = list(supp=0.6))
inspect(freq_eclat_60)

plot(freq_eclat_40,method="itemFrequency")

# clusters
x = c(2,2,8,5,7,6,1,4)
y = c(10,5,4,8,5,4,2,9)

df = data.frame(x,y)
points = paste0("A",c(1,2,3,4,5,6,7,8))
head(df)
km = kmeans(df,centers=df[c(1,4,7),],iter.max = 1)
km
df$clusters = as.factor(km$cluster)
cent = data.frame(km$centers)
library(cluster)
plot(df$x,df$y,col=df$clusters,pch=19)
kmediod = pam(df,k=3,medoids=c(1,4,7))
kmediod
df$clusters1 = as.factor(kmediod$cluster)
df
plot(df$x,df$y,col=df$clusters1,pch=19)
mediods = as.data.frame(kmediod$medoids)
d = dist(df)
hc1 = hclust(d,"single")
hc1
plot(hc1,label=points,main="Single Link")

hc2 = hclust(d,"complete")
hc2
plot(hc2,label=points,main="Complete")

hc3 = hclust(d,"average")
hc3
plot(hc3,label=points,main="average")


# code for dbscan

df = USArrests
summary(df)
head(df)

nrow(df)
ncol(df)

print(ncol(df))
df = na.omit(df)
df = df[,sapply(df,is.numeric)]

minmax = function(x){
  (x-min(x))/(max(x)-min(x))
}

library(dbscan)
kNNdistplot(df,k=3)
abline(h=1,col="red")
res = dbscan(df,0.3,4)
res$cluster
plot(df[,1],df[,2],col=res$cluster,pch=20)
