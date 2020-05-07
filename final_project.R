function(data, labels, modes, gamma){
  
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
