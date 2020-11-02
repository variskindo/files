
setwd("~/Documents/Streit/USST/Engagement/Install")

co.data<-read.csv(file = "co_validation_data_3-29-17.csv", stringsAsFactors = FALSE)
co.data[is.na(co.data)] <- 0
co.data$brand<-"co"
co.non0<-co.data[ co.data$final_score > 0 , -1]

rs.data<-read.csv(file = "rs_validation_data_3-29-17.csv", stringsAsFactors = FALSE)
rs.data[is.na(rs.data)] <- 0
rs.data$brand<-"rs"
rs.non0<-rs.data[ rs.data$final_score > 0 , -1]

sk.data<-read.csv(file = "sk_validation_data_3-29-17.csv", stringsAsFactors = FALSE)
sk.data[is.na(sk.data)] <- 0
sk.data$brand<-"sk"
sk.non0<-sk.data[ sk.data$final_score > 0 , -1]


full.data <-rbind(co.non0, sk.non0, rs.non0)



cuts<-cut(full.data$final_score, seq(0,1000,10))
table(cuts)
data<-cbind(full.data, cuts)



#group by % engaging in 3 months
require(dplyr)

engagement.cuts<- data %>%
  group_by(cuts) %>%
  summarise(engaged1 = mean(engage_dep_flag1),engaged3 = mean(engage_dep_flag3),engaged6 = mean(engage_dep_flag6),n =n())

engagement.cuts.co<- data[data$brand == "co", ] %>%
  group_by(cuts) %>%
  summarise(engaged1 = mean(engage_dep_flag1),engaged3 = mean(engage_dep_flag3),engaged6 = mean(engage_dep_flag6),n =n())

engagement.cuts.rs<- data[data$brand == "rs", ] %>%
  group_by(cuts) %>%
  summarise(engaged1 = mean(engage_dep_flag1),engaged3 = mean(engage_dep_flag3),engaged6 = mean(engage_dep_flag6),n =n())

engagement.cuts.sk<- data[data$brand == "sk", ] %>%
  group_by(cuts) %>%
  summarise(engaged1 = mean(engage_dep_flag1),engaged3 = mean(engage_dep_flag3),engaged6 = mean(engage_dep_flag6),n =n())

engagement.cuts.usst<-rbind(engagement.cuts.co,engagement.cuts.rs,engagement.cuts.sk)
#write.csv(engagement.cuts.usst, file = "brand engagement levels_3-24-17.csv")

#CO score groups
set.seed(11232016)
wssi<-rep(0,20)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:20){
  cluster<-kmeans(engagement.cuts.co[ , names(engagement.cuts.co) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:20, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster3<-kmeans(engagement.cuts.co[ , names(engagement.cuts.co) %in% c("engaged1", "engaged3")], 3)

engagement.clust.co<-cbind(engagement.cuts.co, cluster3$cluster, cluster4$cluster, cluster5$cluster, cluster6$cluster, cluster7$cluster)
write.csv(engagement.clust.co, file = "CO_eng1-3_clusters_5-3-17.csv")

table(engagement.clust.co$`cluster5$cluster`)

for (i in 1:3){
  sum=0
  for (row in 1:length(engagement.clust.co$n)){
    if (engagement.clust.co$`cluster3$cluster`[row] == i){
      sum = sum + engagement.clust.co$n[row]
    }
  }
  print(c(i, sum))
}

engagement.cuts.co2 <- engagement.clust.co[engagement.clust.co$`cluster6$cluster`==5,c(1:5,9)]

set.seed(11232016)
wssi<-rep(0,7)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:6){
  cluster<-kmeans(engagement.cuts.co2[ , names(engagement.cuts.co2) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:7, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster4<-kmeans(engagement.cuts.co2[ , names(engagement.cuts.co2) %in% c("engaged1", "engaged3")], 4)

engagement.clust.co2<-cbind(engagement.cuts.co2, cluster2$cluster, cluster3$cluster, cluster4$cluster)

write.csv(engagement.clust.co2, file = "CO_Clus6-5_clusters_5-3-17.csv")

table(engagement.clust.co2$`cluster2$cluster`)

for (i in 1:4){
  sum=0
  for (row in 1:length(engagement.clust.co2$n)){
    if (engagement.clust.co2$`cluster4$cluster`[row] == i){
      sum = sum + engagement.clust.co2$n[row]
    }
  }
  print(c(i, sum))
}

engagement.cuts.co3 <- engagement.clust.co[engagement.clust.co$`cluster6$cluster`==6,c(1:5,9)]

set.seed(11232016)
wssi<-rep(0,8)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:7){
  cluster<-kmeans(engagement.cuts.co3[ , names(engagement.cuts.co3) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:8, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster4<-kmeans(engagement.cuts.co3[ , names(engagement.cuts.co3) %in% c("engaged1", "engaged3")], 4)

engagement.clust.co3<-cbind(engagement.cuts.co3, cluster2$cluster, cluster3$cluster, cluster4$cluster)

write.csv(engagement.clust.co3, file = "CO_Clus6-6_clusters_5-3-17.csv")

table(engagement.clust.co3$`cluster2$cluster`)

for (i in 1:3){
  sum=0
  for (row in 1:length(engagement.clust.co3$n)){
    if (engagement.clust.co3$`cluster3$cluster`[row] == i){
      sum = sum + engagement.clust.co3$n[row]
    }
  }
  print(c(i, sum))
}

engagement.cuts.co4 <- rbind(engagement.cuts.co2,engagement.cuts.co3)

set.seed(11232016)
wssi<-rep(0,15)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:14){
  cluster<-kmeans(engagement.cuts.co4[ , names(engagement.cuts.co4) %in% c("engaged1")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:15, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster7<-kmeans(engagement.cuts.co4[ , names(engagement.cuts.co4) %in% c("engaged1")], 7)

engagement.clust.co4<-cbind(engagement.cuts.co4, cluster4$cluster, cluster5$cluster, cluster6$cluster, cluster7$cluster)

write.csv(engagement.clust.co4, file = "CO_Clus56_clusters_5-4-17.csv")


engagement.cuts.co5 <- engagement.clust.co2[engagement.clust.co2$`cluster4$cluster`==4,c(1:6,9)]

set.seed(11232016)
wssi<-rep(0,4)
wss<-cbind(wssi,wssi,wssi)
dim(wss)

for(i in 2:3){
  cluster<-kmeans(engagement.cuts.co5[ , names(engagement.cuts.co5) %in% c("engaged1", "engaged3","engaged6")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:4, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster2<-kmeans(engagement.cuts.co5[ , names(engagement.cuts.co5) %in% c("engaged1", "engaged3","engaged6")], 2)

engagement.clust.co5<-cbind(engagement.cuts.co5, cluster2$cluster, cluster3$cluster)

write.csv(engagement.clust.co5, file = "CO_Clus5-4_clusters_5-4-17.csv")

#SK score groups
set.seed(11232016)
wssi<-rep(0,20)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:20){
  cluster<-kmeans(engagement.cuts.sk[ , names(engagement.cuts.sk) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:20, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster7<-kmeans(engagement.cuts.sk[ , names(engagement.cuts.sk) %in% c("engaged1", "engaged3")], 7)

engagement.clust.sk<-cbind(engagement.cuts.sk, cluster6$cluster, cluster7$cluster)

write.csv(engagement.clust.sk, file = "SK_eng1-3_clusters_5-15-17.csv")

table(engagement.clust.sk$`cluster7$cluster`)


engagement.cuts.sk2_1 <- engagement.clust.sk[engagement.clust.sk$`cluster7$cluster`==4,c(1:5,7)]
engagement.cuts.sk2_2 <- engagement.clust.sk[engagement.clust.sk$`cluster7$cluster`==7,c(1:5,7)]
engagement.cuts.sk2 <- rbind(engagement.cuts.sk2_1,engagement.cuts.sk2_2)

set.seed(11232016)
wssi<-rep(0,7)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:6){
  cluster<-kmeans(engagement.cuts.sk2[ , names(engagement.cuts.sk2) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:7, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster3<-kmeans(engagement.cuts.sk2[ , names(engagement.cuts.sk2) %in% c("engaged1", "engaged3")], 3)

engagement.clust.sk2<-cbind(engagement.cuts.sk2, cluster3$cluster, cluster4$cluster, cluster5$cluster, cluster6$cluster)
write.csv(engagement.clust.sk2, file = "SK_Clus47_clusters_5-15-17.csv")


engagement.cuts.sk3 <- engagement.clust.sk2[engagement.clust.sk2$`cluster4$cluster`==4,c(1:6,8)]

set.seed(11232016)
wssi<-rep(0,5)
wss<-cbind(wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:4){
  cluster<-kmeans(engagement.cuts.sk3[ , names(engagement.cuts.sk3) %in% c("engaged1", "engaged3","engaged6")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:5, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster2<-kmeans(engagement.cuts.sk3[ , names(engagement.cuts.sk3) %in% c("engaged1", "engaged3","engaged6")], 2)

engagement.clust.sk3<-cbind(engagement.cuts.sk3, cluster2$cluster, cluster3$cluster, cluster4$cluster)
write.csv(engagement.clust.sk3, file = "SK_Clus7-4-4_clusters_5-15-17.csv")

engagement.cuts.sk4 <- engagement.clust.sk2[engagement.clust.sk2$`cluster2$cluster`==1,c(1:7)]

set.seed(11232016)
wssi<-rep(0,6)
wss<-cbind(wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:5){
  cluster<-kmeans(engagement.cuts.sk4[ , names(engagement.cuts.sk4) %in% c("engaged1", "engaged3","engaged6")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:6, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster2<-kmeans(engagement.cuts.sk4[ , names(engagement.cuts.sk4) %in% c("engaged1", "engaged3","engaged6")], 2)

engagement.clust.sk4<-cbind(engagement.cuts.sk4, cluster2$cluster, cluster3$cluster, cluster4$cluster)
write.csv(engagement.clust.sk4, file = "SK_Clus6-2-1_clusters_5-3-17.csv")

#RS score groups
set.seed(11232016)
wssi<-rep(0,20)
wss<-cbind(wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:20){
  cluster<-kmeans(engagement.cuts.rs[ , names(engagement.cuts.rs) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:20, wssi, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


cluster7<-kmeans(engagement.cuts.rs[ , names(engagement.cuts.rs) %in% c("engaged1", "engaged3")], 7)

engagement.clust.rs<-cbind(engagement.cuts.rs, cluster3$cluster, cluster4$cluster, cluster5$cluster, cluster6$cluster, cluster7$cluster)

write.csv(engagement.clust.rs, file = "RS_eng1-3_clusters_5-3-17.csv")

table(engagement.clust.rs$`cluster7$cluster`)


engagement.cuts.rs2 <- engagement.clust.rs[engagement.clust.rs$`cluster7$cluster`==1,c(1:5,10)]

set.seed(11232016)
wssi<-rep(0,5)
wss<-cbind(wssi,wssi,wssi,wssi)
dim(wss)

for(i in 2:4){
  cluster<-kmeans(engagement.cuts.rs2[ , names(engagement.cuts.rs2) %in% c("engaged1", "engaged3")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:5, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster4<-kmeans(engagement.cuts.rs2[ , names(engagement.cuts.rs2) %in% c("engaged1", "engaged3")], 4)

engagement.clust.rs2<-cbind(engagement.cuts.rs2, cluster2$cluster, cluster3$cluster, cluster4$cluster)
write.csv(engagement.clust.rs2, file = "RS_Clus7-1_clusters_5-3-17.csv")

table(engagement.clust.rs2$`cluster10$cluster`)

for (i in 1:9){
  sum=0
  for (row in 1:length(engagement.clust.rs2$n)){
    if (engagement.clust.rs2$`cluster9$cluster`[row] == i){
      sum = sum + engagement.clust.rs2$n[row]
    }
  }
  print(c(i, sum))
}

engagement.cuts.rs3 <- engagement.clust.rs[engagement.clust.rs$`cluster7$cluster`==2,c(1:5,10)]
engagement.cuts.rs3 <- engagement.cuts.rs3[-5,]

set.seed(11232016)
wssi<-rep(0,4)
wss<-cbind(wssi,wssi,wssi)
dim(wss)

for(i in 2:3){
  cluster<-kmeans(engagement.cuts.rs3[ , names(engagement.cuts.rs3) %in% c("engaged1", "engaged3", "engaged6")], i)
  wssi[i]<-sum(cluster$withinss)
}
plot(1:4, wssi, type = "b", xlab = "Number of Clusters, 1 and 3", ylab = "Within groups sum of squares")


cluster3<-kmeans(engagement.cuts.rs3[ , names(engagement.cuts.rs3) %in% c("engaged1", "engaged3", "engaged6")], 3)

engagement.clust.rs3<-cbind(engagement.cuts.rs3, cluster2$cluster, cluster3$cluster)
write.csv(engagement.clust.rs3, file = "RS_Clus7-2_clusters_5-3-17.csv")
