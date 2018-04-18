

check_input <- function(data){
  
  stopifnot(c(COL_CONDITION, COL_ID) %in% colnames(data))
  
  if(anyNA(data[,c(COL_CONDITION, COL_ID)])) 
    stop("COL_CONDITION/COL_ID contain missings!")
  
  # check MPT file
  mpt_model <- TreeBUGS::readEQN(EQN_FILE)
  stopifnot(is.data.frame(mpt_model))
  
  # check whether frequencies are all included
  stopifnot(all(mpt_model$Category %in% colnames(data)))
  
  # check missings
  freq <- data[,levels(mpt_model$Category)]
  stopifnot(!anyNA(freq))
  
  stopifnot(all(freq == round(freq)))
  
  cat("##### input successfully checked ####\n")
}


