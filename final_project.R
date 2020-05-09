#OVERLEAF DOCUMENT IS HERE:
#https://www.overleaf.com/4124683755yxjnvhysywdt

#download the required packages:
#install.packages("klaR")
#install.packages("clustMixType")
#install.packages("dendextend")
#install.packages("randomcoloR")
#install.packages("clue")
library(clue)
library(klaR)
library(clustMixType)
library(stats)
library(dplyr)
library(cluster)
library(ggplot2)
library(dendextend)
library(randomcoloR)
library(stargazer)

#################################################################################
#### -------------  PART 1, Definition of functions \ algorithms ----------- ####
#################################################################################

##### ---------------------------  K-modes -------------------------------- #####

# >>> define useful functions for the algorithm:
# function that finds distance between two rows of categorical variables:
cat_dist = function(x,y){
  sum(x != y)
}  

# function that finds the index of closest mode from modes defining clusters to the row x:
closest_mode = function(x, modes){
  distances = apply(modes, MARGIN = 1, cat_dist, y = x)
  index = which(distances==min(distances))
  index = index[1] #to avoid multiple modes
  return(index)
}

# function that finds the most frequent string in a column:
find_mode = function(x){
  ta = table(x)
  tam = max(ta)
  mod = names(ta)[ta == tam]
  mod = mod[1] # select only one mode
  return(mod)
}

# >>> define the kmodes algorithm with the helper functions as function kmodes_fit:
kmodes_fit <- function(dataset, modes_count = 4, method = 1, max_iter = 100){
  # arguments are:
  #   dataset: dataset to cluster, all columns are treated as categories (characters)
  #   modes_count: number of clusters to cluster the dataset
  #   method: method of cluster modes initialization, values: 1, 2
  #         * 1 = select first "modes_count" distinct rows from the dataset as initial modes
  #         * 2 = apply the smart selection algoritm 2 from the paper
  #   max_iter: maximum number of iterations when the algorithm does no converge
  # function returns: list
  #   list[1]: vector of numbers with an index of each cluster
  #   list[2]: dataframe, each row represents one cluster and its index corespond to the returned indices of clusters
  
  # convert the dataset into matrix of characters
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  
  # 1. Select "modes_count" initial modes, one for each cluster.
  # 1.a First method of selection:
  if(method == 1){
    nondupl = dataset[!duplicated(dataset),]
    modes = nondupl[1:modes_count,]
  }
  # 1.b Second method of selection:
  if(method == 2){
    #initialize the object of modes:
    modes = dataset[1:modes_count,]
    
    # define a last useful function :) :
    k_most_frequent = function(x,k){ # function that finds up to k most frequent categories
      ta = table(x)                  # very similar to "find_mode_function" but includes slower sort function
      tas = sort(ta, decreasing = T)
      k = min(length(tas),k)
      k_top = names(tas)[1:k]
      return(k_top)
    }
    # get a list of up to k the most frequent characters for each column
    values = lapply(data.frame(dataset,stringsAsFactors=F), k_most_frequent, k=modes_count)
    
    # repeat until find a suitable solution:
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
  
  # 2. Perform the iterations:
  assigned_clusters_old = c(rep(0, nrow(dataset))) # object to store past values of clusters
  
  iter = 1
  repeat{
    # 2a. Allocate an object to the cluster whose mode is the nearest to it according to (5).
    assigned_clusters = apply(dataset, MARGIN = 1, closest_mode, modes = modes)
    
    # 2b. Update the mode of the cluster after each allocation (according to Theorem 1 from the paper).
    for(i in 1:modes_count){
      if(sum(assigned_clusters==i) > 1){ # must be at least two rows, otherwise apply raises error
        new_mode = t(as.matrix(apply(dataset[assigned_clusters==i,], MARGIN = 2, find_mode)))
        modes[i,] = new_mode  
      } else if (sum(assigned_clusters==i) == 1) { # because function apply does not handle a single row
        modes[i,] = dataset[assigned_clusters==i,]
      }
    }
    
    iter = iter+1
    
    # stop if there is no change in clusters:
    if(all(assigned_clusters == assigned_clusters_old)){
      print(paste("Algorithm converged sucessfully after",iter,"iterations"))
      break
    }
    # stop if max_iter is reached:
    if(iter == max_iter){
      print("Algorithm failed to converge")
    }
    # save the old clusters to verify convergence:
    assigned_clusters_old = assigned_clusters
    
  }
  
  # 3. Return the results
  out = list(assigned_clusters, as.data.frame(modes, stringsAsFactors = F))
  names(out) = c('labels', 'centers')
  return(out)
}

#cluster_kmodes = kmodes_fit(dataset = soy[1:400,-ncol(soy)], method = 2)

# >>> Define function kmodes_predict to be able to assign data to given centroids
# function returns index of closest cluster to each row in dataset
kmodes_predict <-function(dataset, modes){
  
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  
  prediction = apply(dataset, 1, closest_mode, modes = modes)
  return(prediction)
}
#predicted_kmodes = kmodes_predict(dataset = soy[401:683,-ncol(soy)], cluster_kmodes[[2]])
#table(predicted_kmodes, soy[401:683,ncol(soy)])

# >>> Define function kmodes_measures to be able to assess 'quality' of clusters
# inputs are 'data', their 'labels' (index of cluster) and the actual cluster centers 'modes'
# functions returns list with WSS - within sum of square, TSS - total sum of squares, CH - Calinski-Harabasz index
kmodes_measures = function(data, labels, modes){
 
  modes_count = nrow(modes)
  
  # function that finds the distance for each data point from given cluster
  # then squares the distance and sums it with other squared distances
  WSS_cluster = function(index, data, labels, modes){
    sum(apply(data[labels==index,], 1, cat_dist, y=modes[index,])^2)
  }
  
  # for each index in 1 to 'modes_count' calculate its WSS and then sum it
  indices = matrix(data = c(1:modes_count), nrow=modes_count)
  WSS = sum(apply(indices, 1, WSS_cluster, data=data, 
                  labels=labels, modes=modes))
  
  
  data_center = t(as.matrix(apply(data, MARGIN = 2, find_mode)))
  TSS = sum(apply(data, 1, cat_dist, y=data_center)^2)
  
  CH = ((TSS - WSS)/(modes_count - 1)) / (WSS / (nrow(data) - modes_count))
  
  out = list(WSS,TSS,CH)
  names(out) = c('WSS','TSS','CH')
  
  return(out)
}

##### ---------------------------  K-proto -------------------------------- #####
# >>> define useful functions for the algorithm:
# function that finds distance between two variables of a mixed type:
mixed_dist = function(x, y, number, gamma){
  ED =  sum((as.numeric(x[number]) - as.numeric(y[number]))**2)**0.5
  mix = gamma * sum(x[!number] != y[!number]) + ED
  return(mix)
}

# function that finds the closest mode from given cluster modes to the x:
closest_mix_mode = function(x, modes, number, gamma){
  distances = apply(modes, MARGIN = 1, mixed_dist, y=x, number=number, gamma=gamma)
  index = which(distances==min(distances))
  index = index[1] # to avoid multiple modes
  return(index)
}

# >>> define the kproto algorithm with the helper functions as function kproto_fit:
kproto_fit = function(dataset, modes_count=2, gamma=1, max_iter=100, method=2){
  # arguments are:
  #   dataset: dataset to cluster
  #         * columns of type "double" or "interger" are treated as real numbers
  #         * columns of other types are treated as categories  
  #   modes_count: number of clusters to cluster the dataset
  #   method: method of cluster modes initialization, values: 1, 2
  #         * 1 = select first "modes_count" distinct
  #         * 2 = apply the smart selection algoritm 2 from the paper
  #   max_iter: maximum number of iterations when the algorithm does no converge
  # function returns: list
  #   list[1]: vector of numbers with an index of each cluster
  #   list[2]: dataframe, each row represents one cluster and its index corespond to the returned indices of clusters
  
  # store position of numeric columns:
  type = lapply(dataset, typeof)
  number = type %in% c("double","integer")
  
  # convert data into matrix of characters
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  
  # 1. Select "modes_count" initial modes, one for each cluster.
  # 1.a First method of selection:
  if(method == 1){
    nondupl = dataset[!duplicated(dataset),]
    modes = nondupl[1:modes_count,]
  }
  # 1.b Second method of selection:
  if(method == 2){
    modes = dataset[1:modes_count,]
    
    k_most_frequent = function(x,k){ # function that finds up to k most frequent categories
      ta = table(x)                  # very similar to "find_mode_function" but includes slower sort function
      tas = sort(ta, decreasing = T)
      k = min(length(tas),k)
      k_top = names(tas)[1:k]
      return(k_top)
    }
    
    values = lapply(data.frame(dataset,stringsAsFactors=F), k_most_frequent, k=modes_count)
    
    repeat{ # repeat until find a suitable solution:
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
  
  # 2. Run the iteraions:
  assigned_clusters_old = c(rep(0, nrow(dataset)))
  
  iter = 1
  repeat{
    #2a. Allocate an object to the cluster whose mode is the nearest to it according to (5).
    assigned_clusters = apply(dataset, MARGIN = 1, closest_mix_mode, modes = modes, 
                              gamma = gamma, number = number)
    
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
    
    # stop if there is no change in clusters:
    if(all(assigned_clusters == assigned_clusters_old)){
      print(paste("Algorithm converged sucessfully after",iter,"iterations"))
      break
    }
    # stop if max_iter is reached:
    if(iter == max_iter){
      print("Algorithm failed to converge")
    }
    
    assigned_clusters_old = assigned_clusters
    
  }
  
  # 3. Return the results
  modes = as.data.frame(modes, stringsAsFactors = F)
  modes[,number] = apply(modes[,number], 2, as.numeric)
  
  out = list(assigned_clusters, modes)
  names(out) = c('labels', 'centers')
  return(out)
}

#clust_kproto = kproto_fit(credit[,-ncol(credit)])

# >>> Define function kproto_predict to be able to assign data to given centroids
# function returns index of closest cluster to each row in dataset
kproto_predict = function(dataset, modes, gamma=1){# use the same gamma as when fitting!
  
  type = lapply(dataset, typeof)
  number = type %in% c("double","integer")
  
  # convert data into matrix of characters
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  modes = as.matrix(modes)
  prediction = apply(dataset, MARGIN = 1, FUN = closest_mix_mode, modes = modes,
                     gamma = gamma, number = number)
  
  return(prediction)
}

#predicted_kproto = kproto_predict(credit[,-ncol(credit)],clust_kproto[[2]])

# function that finds total within sum of squares for given 'data', their labels
# 'clusters' and the actual cluster centers 'modes

# >>> Define function kproto_measures to be able to assess 'quality' of clusters
# inputs are 'data', their 'labels' (index of cluster), the actual cluster centers 'modes', gamma
# functions returns list with: WSS - within sum of square, TSS - total sum of squares, CH - Calinski-Harabasz index
kproto_meassures = function(data, labels, modes, gamma){
  
  modes_count = nrow(modes)
  type = lapply(data, typeof)
  number = type %in% c("double","integer")
  
  # function that finds the distance for each data point from given cluster
  # then squares the distance and sums it with other squared distances
  WSS_cluster = function(index, data, labels, modes, gamma){
    sum(apply(data[labels==index,], 1, mixed_dist, y=modes[index,],
              number=number, gamma=gamma)^2)
  }
  
  # for each index in 1 to modes count calculate its WSS and then sum it
  indices = matrix(data = c(1:modes_count), nrow=modes_count)
  WSS = sum(apply(indices, 1, WSS_cluster, data=data, 
                  labels=labels, modes=modes, gamma=gamma))
  
  center = modes[1,]
  new_mode_cat = t(as.matrix(apply(data[,!number], MARGIN = 2, find_mode)))
  new_mode_num = t(as.matrix(apply(data[,number], MARGIN = 2, 
                                   function(w) mean(as.numeric(w)))))
  center[,!number] = new_mode_cat
  center[, number] = new_mode_num
  TSS = sum(apply(data, 1, mixed_dist, y=center, number=number, gamma=gamma)^2)
  
  CH = ((TSS - WSS)/(modes_count - 1)) / (WSS / (nrow(data) - modes_count))
  
  out = list(WSS,TSS,CH)
  names(out) = c('WSS','TSS','CH')
  
  return(out)
}

#################################################################################
#### -------------  PART 2, Data loading, replication of results ----------- ####
#################################################################################                                 
                                   
#### ------------------- data loading and cleaning --------------------------####
# >>> Loading and Cleaning Soy Dataset:
#from https://datahub.io/machine-learning/soybean#resource-soybean
#soy datatset, contains only categorical features:
soy = read.csv("soybean_csv.csv", sep = ",", header = T, stringsAsFactors = F, na.strings = c("", "NA"))
str(soy)
# cleaning NAs from the dataset
soy <-  na.omit(soy) 
# checking unique class assinged 
unique(soy$class) 
# creating a vector to get the numbers based on the 
chosen_class <- c("diaporthe-stem-canker", "charcoal-rot", "rhizoctonia-root-rot", "phytophthora-rot") 
# filter the data from the vector to get the numbers based on the chosen class
soy_set <- filter(soy, class %in% chosen_class)
# check to see that we have 4 classes in soy_set$class
length(unique(soy_set$class))
# get the numbers of each disease
table(soy_set$class)
str(ncol(soy_set))

# find columns with more than one unique value and subset these variables as a new dataframe
rv_soy <- vector(length = 36)
for(i in 1:ncol(soy_set)){
  rv_soy[i] <- length(unique(soy_set[,i]))>1
}
soy_set <- soy_set[,rv_soy]
str(soy_set)

# >>> Loading and Cleaning Credit Dataset:
#from https://datahub.io/machine-learning/credit-approval#resource-credit-approval
#credit dataset, contains both categorical and numerical features
credit = read.csv("credit-approval_csv.csv", sep = ",", header = T, stringsAsFactors = F, na.strings = c("", "NA"))
head(credit)
str(credit)
# remove observations with NAs
credit_set <- na.omit(credit)

# find columns with more than one value, see that all of them have more than one
rv_cre <- vector(length = 16)
for(i in 1:ncol(credit_set)){
  rv_cre[i] <- length(unique(credit_set[,i]))>1
}
rv_cre
str(credit_set)
# rescaling the data set
credit_set <- credit_set %>% mutate_if(is.numeric, scale)

#### ---------------------- Soy dataset analysis ----------------------------####
# Defining a function to calculate the accuracy using Hungarian Algorithm
soyAccuracy <- function(tbl_matrix){
  sum <- 0
  c_a <- solve_LSAP(table_matrix, maximum = TRUE)
  for(i in 1:4) {
    sum <- sum + sum(table_matrix[i,c_a[i]])
  }
  return(sum/nrow(soy_testset))
}

# creating data frame
soy_cl_ttlwithin <- data.frame(matrix(ncol = 2, nrow = 20))
names(soy_cl_ttlwithin) <- c("clusters_number", "within_diff")
soy_cl_ttlwithin$clusters_number <- 1:20

# Using visualization to see the numbers of recommended modes
for (i in 1:20) {
  clust_soy_loop <- kmodes(soy_set, i)
  soy_cl_ttlwithin[i,2] <- sum(clust_soy_loop$withindiff)
}
str(soy_cl_ttlwithin)
plot(soy_cl_ttlwithin, type = "b", ylab = "Total within difference", xlab = "Number of clusters") 
# The flex point is at approximately 4 and 5

#### Replication of Soy Dataset ####
#### using klaR::kmodes to replicate the paper #####
# asFactors for kmodes #
soy_set_asFactors <- mutate_if(soy_set, is.character, as.factor)
soy_testset <- soy_set_asFactors[sample(1:nrow(soy_set_asFactors)),]
str(soy_testset[,-ncol(soy_testset)])
table(soy_testset$class)
clust_soy_testset <- kmodes(data = soy_testset[-ncol(soy_testset)], modes = 4)

# soy set testset to design the function
str(clust_soy_testset)
soy_testset$cluster <- clust_soy_testset$cluster
table_matrix <- as.matrix(table(soy_testset$class, soy_testset$cluster))
table_matrix
solve_LSAP(table_matrix, maximum = TRUE)
soyAccuracy(table_matrix)

# loop to replicate the results
soy_results <- data.frame(matrix(NA, ncol = 1, nrow = 10))
names(soy_results) <- "accuracy"

for(i in 1:100) {
  soy_testset <- soy_set_asFactors[sample(1:nrow(soy_set_asFactors)),]
  clust_soy_testset <- kmodes(data = soy_testset[-ncol(soy_testset)], modes = 4)
  soy_testset$cluster <- clust_soy_testset$cluster
  table_matrix <- as.matrix(table(soy_testset$class, soy_testset$cluster))
  soy_results[i,1] <- soyAccuracy(table_matrix)
}
boxplot(soy_results$accuracy,main = "Histogram of accuracy using kmodes", ylab = "Accuracy", ylim = c(0,1))
  abline(h = 0.5, lty = 3)
hist(soy_results$accuracy, breaks = 10, main = "Histogram of accuracy using kmodes", xlab = "Accuracy")
  abline(v = 0.5, lty = 3)

## the paper does not further elaborate on how they solved the issue when the cluster have the same centroid

#### using manually::kmodes_fit ####
soy_testset <- soy_set[sample(1:nrow(soy_set)),]
table(soy_testset$class)

cl_m_soy_testset <- kmodes_fit(data = soy_testset, modes = 4)
kmodes_measures(data = soy_testset, labels = cl_m_soy_testset[[1]], modes = 4)
str(cl_m_soy_testset)
length(cl_m_soy_testset[[1]])
table(soy_testset$class, cl_m_soy_testset[[1]])

# looping
soy_results_m1 <- data.frame(matrix(NA, ncol = 1, nrow = 100))
names(soy_results_m1) <- "accuracy"
#method 1 - loop
for(i in 1:100) {
  soy_testset <- soy_set[sample(1:nrow(soy_set)),]
  clust_soy_testset <- kmodes_fit(data = soy_testset[-ncol(soy_testset)], modes = 4, method  = 1)
  soy_testset$cluster <- clust_soy_testset[[1]]
  table_matrix <- as.matrix(table(soy_testset$class, soy_testset$cluster))
  soy_results_m1[i,1] <- soyAccuracy(table_matrix)
}
summary(soy_results_m1)
hist(soy_results_m1$accuracy, breaks = 10, main = "Histogram of accuracy using kmodes_fit method = 1", xlab = "Accuracy") 
  abline(v = 0.5, lty = 3)

#method 2
soy_results_m2 <- data.frame(matrix(NA, ncol = 1, nrow = 100))
names(soy_results_m2) <- "accuracy"
for(i in 1:100) {
  soy_testset <- soy_set[sample(1:nrow(soy_set)),]
  clust_soy_testset <- kmodes_fit(data = soy_testset[-ncol(soy_testset)], modes = 4, method = 2)
  soy_testset$cluster <- clust_soy_testset[[1]]
  table_matrix <- as.matrix(table(soy_testset$class, soy_testset$cluster))
  soy_results_m2[i,1] <- soyAccuracy(table_matrix)
}
#stargazer(cbind(summary(soy_results_m1),summary(soy_results_m2))) #for latex text
hist(soy_results_m2$accuracy, breaks = 10, main = "Histogram of accuracy using kmodes_fit method = 2", xlab = "Accuracy")
  abline(v = 0.5, lty = 3)
boxplot(soy_results$accuracy,main = "Histogram of accuracy using kmodes_fit method = 2", ylab = "Accuracy", ylim = c(0,1))
  abline(h = 0.5, lty = 3)

# comparison of both methods 
par(mfrow=c(1,2))
hist(soy_results_m1$accuracy, breaks = 10, main = "method = 1", xlab = "accuracy")
hist(soy_results_m2$accuracy, breaks = 10, main = "method = 2", xlab = "accuracy")
par(mfrow=c(1,1))

#### ---------------------- Credit dataset analysis -------------------------####

# defining a function to later find the highest accuracy from the confusion matrix 
crAccuracy <- function(df) {
  ifelse(sum(as.matrix(df)[1,1], as.matrix(df)[2,2]) > sum(as.matrix(df)[1,2], as.matrix(df)[2,1]),
         sum(as.matrix(df)[1,1], as.matrix(df)[2,2]),
         sum(as.matrix(df)[1,2], as.matrix(df)[2,1])
  )
}
#### using clustMixType::kproto ####
# NOTE: Using these optimization funciton from the clustMixType takes a lot of time
#c_cred_tau_opt <- tau_kproto(data = credit_set, k = 2:5, nstart = 5, verbose = FALSE)
#c_cred_sil_opt <- silhouette_kproto(data = credit_set, k = 2:5, nstart = 5, verbose = FALSE)
#str(c_cred_tau_opt) # The test take a long time to achieve the results, we got recommended 5 clusters
#str(c_cred_sil_opt) # The test take a long time to achieve the results, we got recommended 4 clusters

# credit_set_asFactors for kproto function
cr_set_asFactors <- mutate_if(credit_set, is.character, as.factor)
str(cr_set_asFactors)
cr_asF_testset <- cr_set_asFactors[sample(1:nrow(cr_set_asFactors)),]
str(cr_asF_testset)
cr_cl <- kproto(cr_asF_testset[,-ncol(cr_asF_testset)], k = 2)
crAccuracy(table(cr_asF_testset$class,cr_cl$cluster))/nrow(cr_asF_testset)

# looping results
cr_results <- data.frame(matrix(NA, nrow = 10, ncol = 1))
names(cr_results) <- "accuracy"
for (i in 1:100) {
  cr_asF_testset <- cr_set_asFactors[sample(1:nrow(cr_set_asFactors)),]
  cr_cl <- kproto(cr_asF_testset[,-ncol(cr_asF_testset)], k = 2)
  cr_results[i,1] <- crAccuracy(table(cr_asF_testset$class,cr_cl$cluster))/nrow(cr_asF_testset)
}

# histogram of the accuracy
hist(cr_results$accuracy, breaks = 10, main = "Histogram of approval accuracy", xlab = "Accuracy")
boxplot(cr_results$accuracy)
mean(cr_results$accuracy)
median(cr_results$accuracy)

#### using manually::kproto_fit ####
cr_testset <- credit_set[sample(1:nrow(credit_set)),]
cr_cl_m <- kproto_fit(cr_testset[,-ncol(cr_testset)], modes_count = 2, gamma = 0, max_iter = 100, method = 1)
cr_testset$cluster <- cr_cl_m[[1]]
table(cr_testset$class, cr_testset$cluster)
crAccuracy(table(cr_testset$class, cr_testset$cluster))/nrow(cr_testset)

#method 1
cr_results_m1 <- data.frame(matrix(NA, ncol = 1, nrow = 100))
names(cr_results_m1) <- "accuracy"
for(i in 1:100) {
  cr_testset <- credit_set[sample(1:nrow(credit_set)),]
  cr_cl_m <- kproto_fit(cr_testset[,-ncol(cr_testset)], modes_count = 2, gamma = 0, max_iter = 100, method = 1)
  cr_testset$cluster <- cr_cl_m[[1]]
  cr_results_m1[i,1] <- crAccuracy(table(cr_testset$class, cr_testset$cluster))/nrow(cr_testset)
}
unique(cr_results_m1$accuracy)
hist(cr_results_m1$accuracy, breaks = 20)

#method 2
cr_results_m2 <- data.frame(matrix(NA, ncol = 1, nrow = 100))
names(cr_results_m2) <- "accuracy"
for(i in 1:100) {
  cr_testset <- credit_set[sample(1:nrow(credit_set)),]
  cr_cl_m <- kproto_fit(cr_testset[,-ncol(cr_testset)], modes_count = 2, gamma = 0, max_iter = 100, method = 2)
  cr_testset$cluster <- cr_cl_m[[1]]
  cr_results_m2[i,1] <- crAccuracy(table(cr_testset$class, cr_testset$cluster))/nrow(cr_testset)
}
unique(cr_results_m2$accuracy)
hist(cr_results_m2$accuracy)
stargazer(cbind(summary(cr_results_m1), summary(cr_results_m2)))

#testing gamma loop method 1
cr_results_g_m1 <- data.frame(matrix(NA, ncol = 9, nrow = 100))
names(cr_results_g_m1)
gamma_vector <- c("0","0.5","0.7","0.9","1","1.1","1.2","1.3","1.4")
names(cr_results_g_m1) <- gamma_vector
head(cr_results_g_m1)
for(j in gamma_vector) {
  for (i in 1:100) {
    cr_testset <- credit_set[sample(1:nrow(credit_set)), ]
    cr_cl_m <- kproto_fit(cr_testset[, -ncol(cr_testset)], modes_count = 2, gamma = as.numeric(j),
        max_iter = 100,method = 1
        )
    cr_testset$cluster <- cr_cl_m[[1]]
    cr_results_g_m1[i, j] <-
      round(crAccuracy(table(cr_testset$class, cr_testset$cluster)) / nrow(cr_testset),2)
  }
}

cr_results_g_m1_formatted <- cr_results_g_m1
cr_results_g_m1_formatted[cr_results_g_m1_formatted <= 0.71] <- 0.71
cr_results_g_m1_formatted[cr_results_g_m1_formatted >= 0.83] <- 0.83
cr_results_g_m1_formatted <- cr_results_g_m1_formatted %>%
  mutate_all(as.character)

cr_results_values <- c(0.83, 0.82, 0.81, 0.80, 0.79, 0.78, 0.77, 0.76, 0.75, 0.74, 0.73, 0.72, 0.71)
df_values_count_m1 <- data.frame(matrix(NA, nrow = 13, ncol = 1))
names(df_values_count_m1) <- "accuracy"
df_values_count_m1$accuracy <- cr_results_values

cr_g_m1_0 <- as.data.frame(table(cr_results_g_m1_formatted$'0', dnn =list("accuracy")), responseName = '0')
cr_g_m1_0$accuracy <- as.numeric(levels(cr_g_m1_0$accuracy))
cr_g_m1_05 <- as.data.frame(table(cr_results_g_m1_formatted$'0.5', dnn =list("accuracy")), responseName = '0.5')
cr_g_m1_05$accuracy <- as.numeric(levels(cr_g_m1_05$accuracy))
cr_g_m1_07 <- as.data.frame(table(cr_results_g_m1_formatted$'0.7', dnn =list("accuracy")), responseName = '0.7')
cr_g_m1_07$accuracy <- as.numeric(levels(cr_g_m1_07$accuracy))
cr_g_m1_09 <- as.data.frame(table(cr_results_g_m1_formatted$'0.9', dnn =list("accuracy")), responseName = '0.9')
cr_g_m1_09$accuracy <- as.numeric(levels(cr_g_m1_09$accuracy))
cr_g_m1_1 <- as.data.frame(table(cr_results_g_m1_formatted$'1', dnn =list("accuracy")), responseName = '1')
cr_g_m1_1$accuracy <- as.numeric(levels(cr_g_m1_1$accuracy))
cr_g_m1_11 <- as.data.frame(table(cr_results_g_m1_formatted$'1.1', dnn =list("accuracy")), responseName = '1.1')
cr_g_m1_11$accuracy <- as.numeric(levels(cr_g_m1_11$accuracy))
cr_g_m1_12 <- as.data.frame(table(cr_results_g_m1_formatted$'1.2', dnn =list("accuracy")), responseName = '1.2')
cr_g_m1_12$accuracy <- as.numeric(levels(cr_g_m1_12$accuracy))
cr_g_m1_13 <- as.data.frame(table(cr_results_g_m1_formatted$'1.3', dnn =list("accuracy")), responseName = '1.3')
cr_g_m1_13$accuracy <- as.numeric(levels(cr_g_m1_13$accuracy))
cr_g_m1_14 <- as.data.frame(table(cr_results_g_m1_formatted$'1.4', dnn =list("accuracy")), responseName = '1.4')
cr_g_m1_14$accuracy <- as.numeric(levels(cr_g_m1_14$accuracy))

cr_results_gamma_table_m1 <- df_values_count_m1 %>% 
  left_join(cr_g_m1_0, by = "accuracy") %>%
  left_join(cr_g_m1_05, by = "accuracy") %>%
  left_join(cr_g_m1_07, by = "accuracy") %>%
  left_join(cr_g_m1_09, by = "accuracy") %>%
  left_join(cr_g_m1_1, by = "accuracy") %>%
  left_join(cr_g_m1_11, by = "accuracy") %>%
  left_join(cr_g_m1_12, by = "accuracy") %>%
  left_join(cr_g_m1_13, by = "accuracy") %>%
  left_join(cr_g_m1_14, by = "accuracy") 
cr_results_gamma_table_m1[is.na(cr_results_gamma_table_m1)] <- ''
cr_results_gamma_table_m1
#stargazer(cr_results_gamma_table)

#testing gamma loop method 2
cr_results_g_m2 <- data.frame(matrix(NA, ncol = 9, nrow = 100))
names(cr_results_g_m2)
gamma_vector <- c("0","0.5","0.7","0.9","1","1.1","1.2","1.3","1.4")
as.numeric(gamma_vector[3])
names(cr_results_g_m2) <- gamma_vector
head(cr_results_g_m2)
for(j in gamma_vector) {
  for (i in 1:100) {
    cr_testset <- credit_set[sample(1:nrow(credit_set)), ]
    cr_cl_m <-
      kproto_fit(
        cr_testset[, -ncol(cr_testset)],
        modes_count = 2,
        gamma = as.numeric(j),
        max_iter = 100,
        method = 2
      )
    cr_testset$cluster <- cr_cl_m[[1]]
    cr_results_g_m2[i, j] <-
      round(crAccuracy(table(cr_testset$class, cr_testset$cluster)) / nrow(cr_testset),2)
  }
}

cr_results_g_m2_formatted <- cr_results_g_m2
cr_results_g_m2_formatted[cr_results_g_m2_formatted <= 0.71] <- 0.71
cr_results_g_m2_formatted[cr_results_g_m2_formatted >= 0.83] <- 0.83
cr_results_g_m2_formatted <- cr_results_g_m2_formatted %>%
  mutate_all(as.character)

cr_results_values <- c(0.83, 0.82, 0.81, 0.80, 0.79, 0.78, 0.77, 0.76, 0.75, 0.74, 0.73, 0.72, 0.71)
df_values_count <- data.frame(matrix(NA, nrow = 13, ncol = 1))
names(df_values_count) <- "accuracy"
df_values_count$accuracy <- cr_results_values

cr_g_m2_0 <- as.data.frame(table(cr_results_g_m2_formatted$'0', dnn =list("accuracy")), responseName = '0')
cr_g_m2_0$accuracy <- as.numeric(levels(cr_g_m2_0$accuracy))
cr_g_m2_05 <- as.data.frame(table(cr_results_g_m2_formatted$'0.5', dnn =list("accuracy")), responseName = '0.5')
cr_g_m2_05$accuracy <- as.numeric(levels(cr_g_m2_05$accuracy))
cr_g_m2_07 <- as.data.frame(table(cr_results_g_m2_formatted$'0.7', dnn =list("accuracy")), responseName = '0.7')
cr_g_m2_07$accuracy <- as.numeric(levels(cr_g_m2_07$accuracy))
cr_g_m2_09 <- as.data.frame(table(cr_results_g_m2_formatted$'0.9', dnn =list("accuracy")), responseName = '0.9')
cr_g_m2_09$accuracy <- as.numeric(levels(cr_g_m2_09$accuracy))
cr_g_m2_1 <- as.data.frame(table(cr_results_g_m2_formatted$'1', dnn =list("accuracy")), responseName = '1')
cr_g_m2_1$accuracy <- as.numeric(levels(cr_g_m2_1$accuracy))
cr_g_m2_11 <- as.data.frame(table(cr_results_g_m2_formatted$'1.1', dnn =list("accuracy")), responseName = '1.1')
cr_g_m2_11$accuracy <- as.numeric(levels(cr_g_m2_11$accuracy))
cr_g_m2_12 <- as.data.frame(table(cr_results_g_m2_formatted$'1.2', dnn =list("accuracy")), responseName = '1.2')
cr_g_m2_12$accuracy <- as.numeric(levels(cr_g_m2_12$accuracy))
cr_g_m2_13 <- as.data.frame(table(cr_results_g_m2_formatted$'1.3', dnn =list("accuracy")), responseName = '1.3')
cr_g_m2_13$accuracy <- as.numeric(levels(cr_g_m2_13$accuracy))
cr_g_m2_14 <- as.data.frame(table(cr_results_g_m2_formatted$'1.4', dnn =list("accuracy")), responseName = '1.4')
cr_g_m2_14$accuracy <- as.numeric(levels(cr_g_m2_14$accuracy))

cr_results_gamma_table_m2 <- df_values_count %>% 
  left_join(cr_g_m2_0, by = "accuracy") %>%
  left_join(cr_g_m2_05, by = "accuracy") %>%
  left_join(cr_g_m2_07, by = "accuracy") %>%
  left_join(cr_g_m2_09, by = "accuracy") %>%
  left_join(cr_g_m2_1, by = "accuracy") %>%
  left_join(cr_g_m2_11, by = "accuracy") %>%
  left_join(cr_g_m2_12, by = "accuracy") %>%
  left_join(cr_g_m2_13, by = "accuracy") %>%
  left_join(cr_g_m2_14, by = "accuracy") %>%
  as.data.frame()
cr_results_gamma_table_m2[is.na(cr_results_gamma_table_m2)] <- ''
cr_results_gamma_table_m2
names(cr_results_gamma_table_m2) <- c("accuracy", gamma_vector)
cr_results_gamma_table_m2

stargazer(as.data.frame(cr_results_gamma_table_m1), summary  = FALSE, title = "Method 1", digits = 2)
stargazer(as.data.frame(cr_results_gamma_table_m1), summary  = TRUE, title = "Method 1", digits = 2)
stargazer(as.data.frame(cr_results_gamma_table_m2), summary  = FALSE, title = "Method 2", digits = 2)
stargazer(as.data.frame(cr_results_gamma_table_m2), summary  = TRUE, title = "Method 2", digits = 2)


