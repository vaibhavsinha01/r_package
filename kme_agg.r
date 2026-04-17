---
title: "K-Means and Agglomerative Clustering"
author: 
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
library(cluster)
```

# Given Data

```{r}
df <- data.frame(
  name=c("A1","A2","A3","A4","A5","A6","A7","A8"),
  x=c(2,2,8,5,7,6,1,4),
  y=c(10,5,4,8,5,4,2,9)
)
df
```

---

# PART 1 — K-MEANS (Manual)

Initial seeds: **A1, A4, A7**

```{r}
centroids <- df[c(1,4,7),c("x","y")]
rownames(centroids) <- c("C1","C2","C3")
centroids
```

---

## Epoch 1 — Assign Clusters

```{r}
coords <- df[,c("x","y")]

dist_mat <- as.matrix(dist(rbind(centroids, coords)))[1:3,4:11]

cluster_epoch1 <- apply(dist_mat,2,which.min)
df_epoch1 <- df
df_epoch1$cluster <- cluster_epoch1
df_epoch1
```

### (a) New clusters after 1 epoch

Cluster 1:
```{r}
df_epoch1 %>% filter(cluster==1)
```

Cluster 2:
```{r}
df_epoch1 %>% filter(cluster==2)
```

Cluster 3:
```{r}
df_epoch1 %>% filter(cluster==3)
```

---

## (b) New Centroids

```{r}
centroid_epoch1 <- df_epoch1 %>%
  group_by(cluster) %>%
  summarise(cx=mean(x), cy=mean(y))

centroid_epoch1
```

---

## (c) Plot after first epoch

```{r}
ggplot(df_epoch1,aes(x,y,color=factor(cluster),label=name))+
  geom_point(size=4)+
  geom_text(vjust=-0.6)+
  geom_point(data=centroid_epoch1,
             aes(cx,cy),
             inherit.aes=FALSE,
             color="black",
             size=6,
             shape=8)+
  xlim(0,10)+ylim(0,10)+
  ggtitle("Clusters after Epoch 1")
```

---

## (d) Run Until Convergence

```{r}
centroids_iter <- centroid_epoch1[,c("cx","cy")]
iteration <- 1

repeat{

  # rename to match data columns
  colnames(centroids_iter) <- c("x","y")

  dist_iter <- as.matrix(dist(rbind(centroids_iter, coords)))[1:3,4:11]
  cluster_iter <- apply(dist_iter,2,which.min)

  new_centroids <- data.frame(cluster=cluster_iter,coords) %>%
    group_by(cluster) %>%
    summarise(cx=mean(x),cy=mean(y)) %>%
    arrange(cluster)

  iteration <- iteration + 1

  if(all(round(as.matrix(centroids_iter),3)==round(as.matrix(new_centroids[,2:3]),3)))
    break

  centroids_iter <- new_centroids[,2:3]
}
```

---

## Final Clusters

```{r}
df_final <- df
df_final$cluster <- cluster_iter
df_final
```

```{r}
ggplot(df_final,aes(x,y,color=factor(cluster),label=name))+
  geom_point(size=4)+
  geom_text(vjust=-0.6)+
  geom_point(
    data=centroids_iter,
    aes(x,y),
    inherit.aes=FALSE,
    color="black",
    size=6,
    shape=17
  )+
  xlim(0,10)+ylim(0,10)+
  ggtitle("Final K-Means Clusters")
```

---

# PART 2 — AGGLOMERATIVE CLUSTERING

Distance Matrix

```{r}
d <- dist(coords)
as.matrix(d)
```

---

## Single Link

```{r}
hc_single <- hclust(d,"single")
plot(hc_single,main="Single Link")
rect.hclust(hc_single,k=3,border="red")
cutree(hc_single,k=3)
```

---

## Complete Link

```{r}
hc_complete <- hclust(d,"complete")
plot(hc_complete,main="Complete Link")
rect.hclust(hc_complete,k=3,border="blue")
cutree(hc_complete,k=3)
```

---

## Average Link

```{r}
hc_avg <- hclust(d,"average")
plot(hc_avg,main="Average Link")
rect.hclust(hc_avg,k=3,border="green")
cutree(hc_avg,k=3)
```

---

## Centroid Method

```{r}
hc_centroid <- hclust(d,"centroid")
plot(hc_centroid,main="Centroid Method")
rect.hclust(hc_centroid,k=3,border="purple")
cutree(hc_centroid,k=3)
```

---

## Medoid (PAM)

```{r}
pam_model <- pam(coords,k=3)
pam_model$clustering
```

```{r}
ggplot(df,aes(x,y,color=factor(pam_model$clustering),label=name))+
  geom_point(size=4)+
  geom_text(vjust=-0.6)+
  xlim(0,10)+ylim(0,10)+
  ggtitle("K-Medoid Clustering")
```

---

# Conclusion

Manual K-Means first epoch was computed using given seeds.  
Algorithm converged after the above number of iterations.  
Agglomerative clustering produced different cluster structures based on linkage rule.