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
kmodes_manually <- function(dataset, modes, iterations){
  #here we can implement it mannualy:
    #1. Select k initial modes, one for each cluster.
    #2. Allocate an object to the cluster whose mode is the nearest to it according to (5). Update
    #the mode of the cluster after each allocation according to Theorem 1.
    #3. After all objects have been allocated to clusters, retest the dissimilarity of objects against
    #the current modes. If an object is found such that its nearest mode belongs to another
    #cluster rather than its current one, reallocate the object to that cluster and update the
    #modes of both clusters.
    #4. Repeat 3 until no object has changed clusters after a full cycle test of the whole data set.
}

clusters_manually = kmodes_manually(soy, 10, 100)

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




