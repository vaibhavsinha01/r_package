---
title: "clustering"
author: 
date: "2026-03-27"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


x = c(2,2,8,5,7,6,1,4),
  y = c(10,5,4,8,5,4,2,9)
  
```{r}
df=data.frame(
x = c(2,2,8,5,7,6,1,4),
  y = c(10,5,4,8,5,4,2,9))
points=paste0("A",c(1,2,3,4,5,6,7,8))
head(df)
```


```{r}
km=kmeans(df,centers = df[c(1,4,7),],iter.max = 1)
km
```
```{r}

```
```{r}
df$clusters=as.factor(km$cluster)
cent=data.frame(km$centers)
library(ggplot2)
ggplot(df,aes(x,y,colour = clusters))+geom_point(size=3)+geom_text(aes(label=points),vjust=-1)+geom_point(data=cent, aes(x, y), 
             color = "black", size = 2)
theme_classic()

```

```{r}
plot(df$x,df$y,col=df$clusters,pch=19)
```
```{r}
library(cluster)
kmediod=pam(df,k=3,medoids = c(1,4,7))
kmediod
```


```{r}
df$clusters1=as.factor(kmediod$cluster)
df
```


```{r}
plot(df$x,df$y,col=df$clusters1,pch=19)
```


```{r}
mediods=as.data.frame(kmediod$medoids)
```


```{r}
ggplot(df,aes(x,y,colour=clusters1))+geom_point(size=3)+geom_point(data=mediods,aes(x,y),colour="black")
```


```{r}
d=dist(df)
d
```


```{r}
hc1=hclust(d,"single")
hc1
plot(hc1,label=points,main = "Single Link")
```


```{r}
hc2=hclust(d,"complete")
plot(hc2,label=points,main="Complete")
```
```{r}
hc3=hclust(d,"average")
plot(hc3,label=points,main="average")
```
```{r}

```