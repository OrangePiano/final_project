#OVERLEAF DOCUMENT IS HERE:
#https://www.overleaf.com/4124683755yxjnvhysywdt

#download the required packages:
#install.packages("klaR")
#install.packages("clustMixType")

library(klaR)
library(clustMixType)
library(stats)

########---------------------data loading and cleaning:
#from https://datahub.io/machine-learning/soybean#resource-soybean
#soy datatset, contains only categorical features:
soy = read.csv("soybean_csv.csv", sep = ",", header = T, stringsAsFactors = F)
head(soy)
str(soy)
summary(soy)
soy = na.omit(soy)


#from https://datahub.io/machine-learning/credit-approval#resource-credit-approval
#credit dataset, contains both categorical and numerical features
credit = read.csv("credit-approval_csv.csv", sep = ",", header = T, stringsAsFactors = F)
head(credit)
str(credit)
summary(credit)
credit = na.omit(credit)

#################################################################
########---------------------kmodes - clustering categorical data
#create the mode algorithm by hand - kmodes:
kmodes_fit <- function(dataset, modes_count = 4, method = 1, max_iter = 100){
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
cluster_kmodes = kmodes_fit(dataset = soy[1:400,-ncol(soy)])

kmodes_predict <-function(dataset, modes){
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  
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
  
  prediction = apply(dataset, 1, closest_mode, modes = modes)
  return(prediction)
}
predicted_kmodes = kmodes_predict(dataset = soy[401:683,-ncol(soy)], cluster_kmodes[[2]])
table(predicted_kmodes, soy[401:683,ncol(soy)])

#or use library "klaR" with function kmodes:
c_soy = kmodes(soy, 10)
c_soy$cluster

#evaluate the clusters somehow, vizualize the clustering measures based on the number of clusters
#for each cluster calculate its most common values and put them in the table

########################################################################################
########---------------------kproto - clustering both numerical and categorical features
#create kproto_manually:
kproto_fit = function(dataset, cluster_count = 2, gamma = 1, max_iter = 100, method = 1){
  #arguments are:
  #   dataset: dataset to cluster
  #         * columns of type "double" or "interger" are treated as real numbers
  #         * columns of other types are treated as categories  
  #   modes_count: number of clusters to cluster the dataset
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
clust_kproto = kproto_fit(credit[,-ncol(credit)])

kproto_predict = function(dataset, modes, gamma = 1){#use the same gamma as when fitting!
  
  #function that finds distance between two variables:
  mixed_dist = function(x,y){
    ED =  sum((as.numeric(x[number]) - as.numeric(y[number]))**2)**0.5
    mix = gamma * sum(x[!number] != y[!number]) + ED
    return(mix)
  } 
  
  #function that finds the index of the closest mode from the given cluster modes to the x:
  closest_mix_mode = function(x, modes){
    distances = apply(modes, MARGIN = 1, mixed_dist, y = x)
    index = which(distances==min(distances))
    index = index[1] #to avoid multiple modes
    return(index)
  }
  
  type = lapply(dataset, typeof)
  number = type %in% c("double","integer")
  
  #convert data into matrix of characters
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  modes = as.matrix(modes)
  prediction = apply(dataset, MARGIN = 1, FUN = closest_mix_mode, modes = modes)
  return(prediction)
}
predicted_kproto = kproto_predict(credit[,-ncol(credit)],clust_kproto[[2]])

#or use library "clustMixType" with function kproto:
c_cred = kproto(credit,10,1, keep.data = T)

#nejake vizualiace a zavery, zhodnoceni kvality clusteru, nalezeni optimalniho poctu clusteru
#hodnota gamma/lamda jestli dava smysl apod.




