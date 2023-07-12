convert_generic <- function(labels.df,score_scaling="raw",labels="labels"){
  if(score_scaling=="raw"){
    labels.df = labels.df
  }else if(score_scaling=="center-scale"){
    labels.df = apply(labels.df,1,scale,center=T)
  }else if(score_scaling=="softmax"){
    labels.df = apply(labels.df,1,softmax)
  }else{
    stop("Select raw, center-scale, or softmax")
  }
  if(labels == "labels"){
    labels = colnames(labels.df)[apply(labels.df,1,which.max)]
    return(labels)
  }else{
    return(labels.df)
  }
}

preprocess_reference <- function(reference){
  return(reference)
}
preprocess_query <- function(query){
  return(query)
}

softmax <- function(x){
  exp(x) / sum(exp(x))
}
