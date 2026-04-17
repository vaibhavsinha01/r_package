---
title: "dbscan"
author: 
date: "2026-03-27"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

df=USArrests
```
```{r}
summary(df)
```


```{r}
head(df)
```
```{r}
nrow(df)
```
```{r}
print(ncol(df))
df=na.omit(df)
df = df[, sapply(df, is.numeric)]

```
```{r}

minmax = function(x){
  (x - min(x)) / (max(x) - min(x))
}

df = as.data.frame(lapply(df, minmax))
```


```{r}
head(df)
```
```{r}
library(dbscan)

kNNdistplot(df, k = 3)
abline(h = 1, col = "red")
```
```{r}
res=dbscan(df,0.3,4)
res$cluster
```


```{r}
plot(df[,1],df[,2],col=res$cluster,pch=20)

```