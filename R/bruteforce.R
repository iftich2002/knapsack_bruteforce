#'Knapsack problem solver
#'
#'take the weights, values as parameters and return maximum values and their possible combination
#'@param x: data frame of weights (first column) and value (second column)
#'@param W: Maximum capacity (numeric)
#'@return Return the maximum value and elements (index of values)


brute_force_knapsack<-function(x, W){
  ##attach dplyr package for filter option
  dplyr::filter()
  ##make the binary matrix of 2^n
  bin_matrix<-as.matrix(expand.grid(replicate(nrow(x), 0:1, simplify = FALSE)))
  ##multiply weight and value by binary matrix and take sum
  value_weight<-cbind(bin_matrix %*% x[,2],bin_matrix %*% x[,1])
  ##rename columns
  colnames(value_weight)<-c("value","weight")
  ##combine value weight with binary
  value_weight_bin<-cbind(bin_matrix,value_weight)
  ##selecting the rows which are less than the given weight and convert it to dataframe
  value_weight_opt<-as.data.frame(value_weight_bin[value_weight_bin[,"weight"]<W,])
  ##row with maximum weight
  weight_max<-filter(value_weight_opt, weight == max(weight))
  elements<-which(weight_max==1)
  result<-list(round(weight_max$value,0),elements)
  names(result)<-c("Value","Elements")
  return(result)
}
