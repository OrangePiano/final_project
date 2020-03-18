#OVERLEAF DOCUMENT IS HERE:
#https://www.overleaf.com/4124683755yxjnvhysywdt

#download the required packages:
#install.packages("klaR")
#install.packages("clustMixType")
#install.packages("dendextend")
#install.packages("randomcoloR")

library(klaR)
library(clustMixType)
library(stats)
library(dplyr)
library(cluster)
library(ggplot2)
library(dendextend)
library(randomcoloR)

########---------------------data loading and cleaning: ####
#from https://datahub.io/machine-learning/soybean#resource-soybean
#soy datatset, contains only categorical features:
soy = read.csv("soybean_csv.csv", sep = ",", header = T, stringsAsFactors = F, na.strings = c("", "NA"))
head(soy)
str(soy)
summary(soy)
soy_set = na.omit(soy)
soy_set$class <- droplevels(soy_set$class)

# Checking the unique values in provided variables
unique_soy_set_var <- data.frame(matrix(NA, nrow = 35, ncol = 2))
for (i in 1:35) {
  unique_soy_set_var[i,1] <- names(soy_set[i])
  unique_soy_set_var[i,2] <- nlevels(unique(soy_set[,i]))
}
unique_soy_set_var[which(unique_credit_set_var$X2==1),]
nlevels(unique(soy_set_class))


#from https://datahub.io/machine-learning/credit-approval#resource-credit-approval
#credit dataset, contains both categorical and numerical features
credit = read.csv("credit-approval_csv.csv", sep = ",", header = T, stringsAsFactors = F, na.strings = c("", "NA"))
head(credit)
str(credit)
summary(credit)
credit_set <- na.omit(credit)

# Checking uniqueness in each variable
unique_credit_set_var <- data.frame(matrix(NA, nrow = 15, ncol = 2))
for (i in 1:15) {
  unique_credit_set_var[i,1] <- names(credit_set[i])
  unique_credit_set_var[i,2] <- ifelse(nlevels(unique(credit_set[,i]))==0,
                                  length(unique(credit_set[,i])),
                                  nlevels(unique(credit_set[,i]))
  )
}

#### Soy Data Set ####
########---------------------kmodes - clustering categorical data ####
#create the mode algorithm by hand - kmodes:
kmodes_manually <- function(dataset, modes_count = 2, method = 1, max_iter = 100){
  #arguments are:
  #   dataset: dataset to cluster, all columns are treated as categories (characters)
  #   modes_count: number of clusters to cluster the dataset
  #   method: method of cluster modes initialization, values: 1, 2
  #         * 1 = select first "modes_count" distinct
  #         * 2 = apply the smart selection algoritm 2 from the paper
  #   max_iter: maximum number of iterations when the algorithm does no converge
  #function returns: list
  #   list[1]: vector of numbers with an index of each cluster
  #   list[2]: dataframe, each row represents one cluster and its index corespond to the returned indices of clusters

  #----------define useful functions for the algorithm:
    #function that finds distance between two rows of categorical variables:
  cat_dist = function(x,y){
    sum(x != y)
  }  
  
    #function that finds the index of closest mode from modes defining clusters to the row x:
  closest_mode = function(x, modes){
    distances = apply(modes, MARGIN = 1, cat_dist, y = x)
    index = which(distances==min(distances))
    index = index[1] #to avoid multiple modes
    return(index)
  }
    
    #function that finds the most frequent string in a column:
  find_mode = function(x){
    ta = table(x)
    tam = max(ta)
    mod = names(ta)[ta == tam]
    mod = mod[1] #select only one mode
    return(mod)
  }
  #-----------end of useful functions
  
  #convert the dataset into matrix of characters (it was simplier than to work with factors)
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  
  #1. Select "modes_count" initial modes, one for each cluster.
    #1.a First method of selection:
  if(method == 1){
    nondupl = dataset[!duplicated(dataset),]
    modes = nondupl[1:modes_count,]
  }
    #1.b Second method of selection:
  if(method == 2){
      #initialize the object of modes:
    modes = dataset[1:modes_count,]
    
      #define a last useful function :) :
    k_most_frequent = function(x,k){ #function that finds up to k most frequent categories
      ta = table(x)                  #very similar to "find_mode_function" but includes slower sort function
      tas = sort(ta, decreasing = T)
      k = min(length(tas),k)
      k_top = names(tas)[1:k]
      return(k_top)
    }
      #get a list of up to k the most frequent characters for each column
    values = apply(dataset, 2, k_most_frequent, k=modes_count)
    
      #repeat until find a suitable solution:
    repeat{
      for(i in 1:length(values)){
        modes[,i] = sample(values[[i]], size = modes_count, replace = T)
      }
      modes = dataset[apply(modes, 1, closest_mode, modes = dataset),]
        #check that there are no same modes by a small chance:
      if(nrow(modes)==nrow(unique.matrix(modes))){
        break  
      }
    }
  }  
  
  assigned_clusters_old = c(rep(0, nrow(dataset))) #object to store past values of clusters
  
  iter = 1
  repeat{
    #2a. Allocate an object to the cluster whose mode is the nearest to it according to (5).
    assigned_clusters = apply(dataset, MARGIN = 1, closest_mode, modes = modes)
    
    #2b. Update the mode of the cluster after each allocation according to Theorem 1.
    for(i in 1:modes_count){
      if(sum(assigned_clusters==i) > 1){ #must be at least two rows, otherwise apply raises error
        new_mode = t(as.matrix(apply(dataset[assigned_clusters==i,], MARGIN = 2, find_mode)))
        modes[i,] = new_mode  
      } else if (sum(assigned_clusters==i) == 1) { #because function apply does not handle single row
        modes[i,] = dataset[assigned_clusters==i,]
      }
    }
    
    iter = iter+1
    
      #stop if there is no change in clusters:
    if(all(assigned_clusters == assigned_clusters_old)){
      print(paste("Algorithm converged sucessfully after",iter,"iterations"))
      break
    }
      #stop if max_iter is reached:
    if(iter == max_iter){
      print("Algorithm failed to converge")
    }
      #save the old clusters to verify convergence:
    assigned_clusters_old = assigned_clusters
    
  }
  
  return(list(assigned_clusters, as.data.frame(modes)))
}
cluster_kmodes = kmodes_manually(dataset = soy[,-ncol(soy)])

####or use library "klaR" with function kmodes: ####
c_soy = kmodes(soy_set, 10)
str(c_soy)

c_soy_total_within_diff <- data.frame(matrix(ncol = 2, nrow = 20))
names(c_soy_total_within_diff) <- c("Number of clusters", "Total within diff")
c_soy_total_within_diff$`Number of clusters` <- 1:20

for (i in 1:20) {
  c_soy_loop <- kmodes(soy_set, i)
  c_soy_total_within_diff[i,2] <- sum(c_soy_loop$withindiff)
}
plot(c_soy_total_within_diff, type = "b")
unique(soy[,36])

# Based on the visualization, the number of clusters recommmended is 4. 
# Which is not equal to 20, which is in fact our number of diseases that was classified.
# This can be due to similarities in the diseases
hc <- hclust(daisy(soy_set), method = "complete")
str(hc)
plot(hc)
dendro <- as.dendrogram(hc)

dendro.col <- dendro %>%
  set("branches_k_color", k = 15, value = distinctColorPalette(15)) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)

ggplot(dendro.col, theme = theme_minimal()) +
  scale_y_reverse(expand = c(0.2,0)) +
  coord_polar(theta = "x")


#evaluate the clusters somehow, vizualize the clustering measures based on the number of clusters
#for each cluster calculate its most common values and put them in the table

scale(soy_set)

#### Credit Data Set ####
####---------------------kproto - clustering both numerical and categorical features ####
#create kproto_manually:
kproto_manually = function(dataset, cluster_count = 2, gamma = 1, max_iter = 100, method = 1){
  #arguments are:
  #   dataset: dataset to cluster
  #         * columns of type "double" or "interger" are treated as real numbers
  #         * columns of other types are treated as categories  
  #   cluster_count: number of clusters to cluster the dataset
  #   method: method of cluster modes initialization, values: 1, 2
  #         * 1 = select first "modes_count" distinct
  #         * 2 = apply the smart selection algoritm 2 from the paper
  #   max_iter: maximum number of iterations when the algorithm does no converge
  #function returns: list
  #   list[1]: vector of numbers with an index of each cluster
  #   list[2]: dataframe, each row represents one cluster and its index corespond to the returned indices of clusters
  
  #--------define useful functions for the algorithm:
    #function that finds distance between two variables:
  mixed_dist = function(x,y){
    ED =  sum((as.numeric(x[number]) - as.numeric(y[number]))**2)**0.5
    mix = gamma * sum(x[!number] != y[!number]) + ED
    return(mix)
  } 
  
    #function that finds the closest mode from given cluster modes to the x:
  closest_mix_mode = function(x, modes){
    distances = apply(modes, MARGIN = 1, mixed_dist, y = x)
    index = which(distances==min(distances))
    index = index[1] #to avoid multiple modes
    return(index)
  }
  
    #function that finds the most frequent string in a vector:
  find_mode = function(x){
    #categorical features
    ta = table(x)
    tam = max(ta)
    mod_cat = names(ta)[ta == tam]
    mod_cat = mod_cat[1] #select only one mode
    return(mod_cat)
  }
  #--------end of useful functions
  
    #store position of numeric columns:
  type = lapply(dataset, typeof)
  number = type %in% c("double","integer")
  
    #convert data into matrix of characters
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)

  #1. Select "modes_count" initial modes, one for each cluster.
  #1.a First method of selection:
  if(method == 1){
    nondupl = dataset[!duplicated(dataset),]
    modes = nondupl[1:modes_count,]
  }
  #1.b Second method of selection:
  if(method == 2){
    modes = dataset[1:modes_count,]
    
    k_most_frequent = function(x,k){ #function that finds up to k most frequent categories
      ta = table(x)                  #very similar to "find_mode_function" but includes slower sort function
      tas = sort(ta, decreasing = T)
      k = min(length(tas),k)
      k_top = names(tas)[1:k]
      return(k_top)
    }
    
    values = apply(dataset, 2, k_most_frequent, k=modes_count)
    
    repeat{#repeat until find a suitable solution:
      for(i in 1:length(values)){
        modes[,i] = sample(values[[i]], size = modes_count, replace = T)
      }
      modes = dataset[apply(modes, 1, closest_mode, modes = dataset),]
      #check that there are no same modes by a small chance:
      if(nrow(modes)==nrow(unique.matrix(modes))){
        break  
      }
    }
  }  
  
  assigned_clusters_old = c(rep(0, nrow(dataset)))
  
  iter = 1
  repeat{
    #2a. Allocate an object to the cluster whose mode is the nearest to it according to (5).
    assigned_clusters = apply(dataset, MARGIN = 1, closest_mix_mode, modes = modes)
    
    #2b. Update the mode of the cluster after each allocation according to Theorem 1.
    for(i in 1:modes_count){
      if(sum(assigned_clusters==i)>1){
        new_mode_cat = t(as.matrix(apply(dataset[assigned_clusters==i,!number], MARGIN = 2, find_mode)))
        new_mode_num = t(as.matrix(apply(dataset[assigned_clusters==i,number], MARGIN = 2, 
                                         function(w) mean(as.numeric(w))
                                         )))
        modes[i,!number] = new_mode_cat
        modes[i, number] = new_mode_num
      } else if( sum(assigned_clusters==i)==1 ) {
        modes[i,] = dataset[assigned_clusters==i,]
      }
    }
    
    iter = iter+1
    
    #stop if there is no change in clusters:
    if(all(assigned_clusters == assigned_clusters_old)){
      print(paste("Algorithm converged sucessfully after",iter,"iterations"))
      break
    }
    #stop if max_iter is reached:
    if(iter == max_iter){
      print("Algorithm failed to converge")
    }
    
    assigned_clusters_old = assigned_clusters
    
  }
  
  modes = as.data.frame(modes)
  modes[,number] = apply(modes[,number], 2, as.numeric)
  return(list(assigned_clusters, modes))
}

clust_kproto = kproto_manually(credit[,-ncol(credit)])

#or use library "clustMixType" with function kproto:
c_cred = kproto(credit_set,2,1)
c_cred_lambda <- lambdaest(x = credit_set)
c_cred <- kproto(x = credit_set, k = 2, lambda = 940941)

c_cred$cluster

c_cred_tau_opt <- tau_kproto(data = credit_set, k = 2:5, nstart = 5, verbose = FALSE)
c_cred_sil_opt <- silhouette_kproto(data = credit_set, k = 2:5, nstart = 5, verbose = FALSE)
str(c_cred_tau_opt) # The test take a long time to achieve the results, we got 5 clusters
str(c_cred_sil_opt) # The test take a long time to achieve the results, we got 4 clusters

for (i in 1:10) {
  c_cred_loop <- kproto(data = credit_set, k = i, lambda = 940941)
}

#nejake vizualiace a zavery, zhodnoceni kvality clusteru, nalezeni optimalniho poctu clusteru
#hodnota gamma/lamda jestli dava smysl apod.

##### Replicating the results ####
##### Soy Bean data set ####
##### To do list ####
# 1. Which diseas is initial cluster? 
# 2. Which diseas is each cluster? 
soy_set_testset_1 <- soy_set[sample(1:dim(soy_set)[1]),]
soy_set_testset_1_class <- soy_set$class
soy_set_testset_1 <- soy_set[,-36]
table(soy_set_testset_1_class)

c_soy_testset_1 <- kmodes(data = soy_set_testset_1, modes = 19)
c_soy_testset_1$cluster
sum(c_soy_testset_1$size)

str(table(c_soy_testset_1$cluster))
test_soy_results <- table(c_soy_testset_1$cluster)
table(c_soy_testset_1$cluster)[[1]]
soy_set_testset_1_class
mean(c_soy_testset_1$cluster==soy_set_testset_1_class) # not working as we dont know which class is which diseases


soy_results <- data.frame(matrix(NA, ncol = 15, nrow = 10))
for(i in 1:10) {
  soy_set_testset <- soy_set[sample(1:dim(soy_set)[1]),]
  soy_set_testset <- soy_set[,-36]
  c_soy_testset <- kmodes(data = soy_set_testset_1, modes = 15, iter.max = 100)
  for(j in 1:15) {
    soy_results[i,j] <- c_soy_testset$size[[j]]
  }
}

soy_results %>%
  mutate(SUM = rowSums(soy_results))
boxplot(soy_results)
table(soy_set$class)


#### Credit Data Set ####
credit_set_testset_1 <- credit_set[sample(1:dim(credit_set)[1]),]
credit_set_testset_1_class <- credit_set$class

c_credit_set_testset_1 <- kproto(x = credit_set_testset_1[,-16], k = 2)
credit_set_testset_1$cluster <- c_credit_set_testset_1$cluster
table(credit_set_testset_1$class, credit_set_testset_1$cluster)
maxSumDiagonal(table(credit_set_testset_1$class, credit_set_testset_1$cluster))/dim(credit_set_testset_1)[1]

maxSumDiagonal <- function(df) {
  ifelse(sum(as.matrix(df)[1,1], as.matrix(df)[2,2]) > sum(as.matrix(df)[1,2], as.matrix(df)[2,1]),
         sum(as.matrix(df)[1,1], as.matrix(df)[2,2]),
         sum(as.matrix(df)[1,2], as.matrix(df)[2,1])
  )
}

credit_results <- data.frame(matrix(NA, ncol = 1, nrow = 100))
for(i in 1:100) {
  credit_set_testset <- credit_set[sample(1:dim(soy_set)[1]),]
  c_credit_testset <- kproto(x = credit_set_testset[,-16], k = 2, iter.max = 100)
  credit_set_testset$cluster <- c_credit_testset$cluster
  credit_results[i,] <- maxSumDiagonal(table(credit_set_testset$class, credit_set_testset$cluster))/dim(credit_set_testset)[1]
}

credit_results_manually <- data.frame(matrix(NA, ncol = 1, nrow = 100))
for(i in 1:100) {
  credit_set_testset <- credit_set[sample(1:dim(soy_set)[1]),]
  c_credit_testset <- kproto_manually(dataset = credit_set_testset[,-16], cluster_count = 2, max_iter = 100, method = 1,gamma = 1)
  credit_set_testset$cluster <- c_credit_testset$cluster
  credit_results_manually[i,] <- maxSumDiagonal(table(credit_set_testset$class, credit_set_testset$cluster))/dim(credit_set_testset)[1]
}


