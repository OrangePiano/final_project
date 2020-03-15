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
soy = read.csv("soybean_csv.csv", sep = ",", header = T)
head(soy)
str(soy)
summary(soy)
#remove NA's: ... 


#from https://datahub.io/machine-learning/credit-approval#resource-credit-approval
#credit dataset, contains both categorical and numerical features
credit = read.csv("credit-approval_csv.csv", sep = ",", header = T)
head(credit)
str(credit)
summary(credit)
#remove NA's: ... 

########---------------------kmodes - clustering categorical data
#create the mode algorithm by hand - kmodes:
kmodes_manually <- function(dataset, modes_count, max_iter){
  #define useful functions for the algorithm:
  
    #function that finds distance between two rows of categorical variables:
  cat_dist = function(x,y){
    sum(x != y)
  }  
  
    #function that finds the closest mode from cluster modes the to row x:
  closest_mode = function(x, modes){
    distances = apply(modes, MARGIN = 1, cat_dist, y = x)
    index = which(distances==min(distances))
    index = index[1] #to avoid multiple modes
    return(index)
  }
    
    #function that finds the most frequent string in a vector:
  find_mode = function(x){
    ta = table(x)
    tam = max(ta)
    mod = names(ta)[ta == tam]
    mod = mod[1] #select only one mode
    return(mod)
  }
  
  #1. Select k initial modes, one for each cluster.
  dataset = apply(dataset, MARGIN = 2, FUN = as.character)
  dataset = as.matrix(dataset)
  modes = dataset[1:modes_count,] #vyber prvnich modes
  assigned_clusters_old = c(rep(1, nrow(dataset)))
  
  iter = 1
  repeat{
    #2a. Allocate an object to the cluster whose mode is the nearest to it according to (5).
    assigned_clusters = apply(dataset, MARGIN = 1, closest_mode, modes = modes)
    
    #2b. Update the mode of the cluster after each allocation according to Theorem 1.
    for(i in 1:modes_count){
      new_mode = t(as.matrix(apply(dataset[assigned_clusters==i,], MARGIN = 2, find_mode)))
      modes[i,] = new_mode
    }
    
    iter = iter+1
    
    if( all(assigned_clusters == assigned_clusters_old) | iter == max_iter ){
      break
    }
    
    assigned_clusters_old = assigned_clusters
    
  }
  
  return(assigned_clusters)
}

cluster = kmodes_manually(dataset = soy, modes_count = 10, max_iter = 1000)
print(cluster)

#or use library "klaR" with function kmodes:
c_soy = kmodes(soy, 10)
c_soy$cluster

#evaluate the clusters somehow, vizualize the clustering measures based on the number of clusters
#for each cluster calculate its most common values and put them in the table

########---------------------kproto - clustering both numerical and categorical features
#create kproto_manually:
kproto_manually = function(dataset, clusts, gamma, iterations){
  #select factors and numerics from dataset
  #combine factors and numerics dissimilarity with gamma
}

#or use library "clustMixType" with function kproto:
c_cred = kproto(credit,10,1)
c_cred$cluster

#nejake vizualiace a zavery, zhodnoceni kvality clusteru, nalezeni optimalniho poctu clusteru
#hodnota gamma/lamda jestli dava smysl apod.





