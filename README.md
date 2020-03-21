# Categorical and mixed data clustering
## Introduction
The aim of this project is to replicate paper 'Extensions to the k-Means Algorithm for Clustering Large Data Sets with Categorical Values' (see the reporitory). In the paper, the authors decsribes clustering of categirical data and suggest a new method for clustering mixed data type - i.e. data with both categorical and numerical variables. In out project, we aim to replicate the clustering analysis. We write our own two functions replicating the two algorithms described in the paper - 'kmodes' (clustering of only categorical data) and 'kproto' (clustering of mixed type data). Consequently, we use these two functions on two datasets (soybean and credit, see the repository) used by the authors. Next, we compare our results of clustering with the results from the paper.

## Description of algorithms
### K-modes
The k modes algorithm is designed for clustering data with only categorical variables. The algorithm is very similar to kmeans algoritm except for the modification of disimilarity meassure. Instead of Euclidean distance the algorithm uses Hamming distance as a meassure of dissimilarity. The Hamming distance for two observations is given as a number of components for which the categories between the two observations differ. Formally:
$d(X,Y) = \sum_i_{1_n} \delta(x_i,y_i)$
where
\delta(x_i,y_i) = 0 if x_i = y_i and 1 otherwise.

<img src="https://render.githubusercontent.com/render/math?math=e^{i \pi} = -1">


## Data

## Results of the clustering
