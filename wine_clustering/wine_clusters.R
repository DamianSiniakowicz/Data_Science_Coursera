# wine clustering

# question: will k-means divide white and red wine into two clusters, without the quality variable?

# step 0 : add a color variable to each data frame

# step 1 : merge white and red wine data frames

#setwd("Coursera_R_Folder/wine_quality")
library(dplyr)
# My scale is 0 to 1 for each variable 
# For any variable, a difference of X% of the range is weighed equally
red_wine <- read.csv("winequality-red.csv",sep=";")
white_wine <- read.csv("winequality-white.csv", sep=";")

white_wine$color <- rep("white",nrow(white_wine))
red_wine$color <- rep("red",nrow(red_wine))
all_wine <- rbind(red_wine,white_wine)

normalized_wine <- lapply(select(all_wine,-c(color,quality)), 
                          function(wine) {(wine-min(wine))/max(wine-min(wine))})

norm_wine_df <- as.data.frame(normalized_wine)

wine_with_colors <- cbind(norm_wine_df, color = c(red_wine$color,white_wine$color))

two_clusters <- kmeans(select(wine_with_colors,-color), centers = 2 , iter.max = 10, nstart=1)

clustered_wine_one <- cbind(wine_with_colors,two_clusters$cluster)
clustered_wine_one <- mutate(clustered_wine_one, Cluster = two_clusters$cluster)
clustered_wine_one <- select(clustered_wine_one, -two_clusters$cluster)

clustered_wine_one$color <- as.factor(clustered_wine_one$color)
clustered_wine_one$Cluster <- as.factor(clustered_wine_one$Cluster)
clustered_wine_final <- mutate(clustered_wine_one, cluster_color = interaction(Cluster,color))

table(clustered_wine_one$color, clustered_wine_one$Cluster)


########
many_clusterd <- kmeans(x=select(wine_with_colors,-color),centers=2,iter.max=20,nstart=1000)
Cluster <- as.factor(many_clusterd$cluster)
clustered_wine_many <- cbind(wine_with_colors,Cluster)
clustered_wine_many$color <- as.factor(clustered_wine_many$color)
table(clustered_wine_many$color, clustered_wine_many$Cluster)

#### what would i look like without alcohol content?
# so, does k-means basically group by the variable with the most variation???

by_cluster <- arrange(clustered_wine_many,Cluster) %>% select(-c(color,Cluster)) 
wine_matrix <- as.matrix(by_cluster)
image(z=t(wine_matrix))

divider <- table(many_clusterd$cluster)[2]/(table(many_clusterd$cluster)[2] + table(many_clusterd$cluster)[1])







