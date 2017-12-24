readData = function (path) {
  
  d <- openxlsx::read.xlsx(path, sheet = 1, colNames = FALSE)
  d[d == ""] <- NA

  
  f <- d[!is.na(d[,1]),c(which(is.na(d[1,])), sum(is.na(d[1,]))+1)]
  colnames(f) = as.character(f[1,]);f = data.frame(f[-1,], stringsAsFactors = F, check.names = FALSE);rownames(f) = 1:nrow(f)
  
  f. = lapply(f, function(x){
    if(sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    }else{
      x
    }
  })
  
  f. = do.call(cbind, lapply(f., data.frame, stringsAsFactors=FALSE))
  colnames(f.) = colnames(f)
  f = f.
  
  if (colnames(f)[1] != "BinBase name") {
    f = f[,c(ncol(f),2:ncol(f)-1)]
  }
  f[[1]] = make.unique(f[[1]], sep = '_')
  
  p <- d[c(which(is.na(d[,1])), max(which(is.na(d[,1])))+1) ,!is.na(d[1,])]
  p <- t(p); colnames(p) = p[1,]; p = data.frame(p[-1,], stringsAsFactors = F, check.names = FALSE)
  
  p.= lapply(p, function(x){
    if(sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    }else{
      x
    }
  })
  
  p. = do.call(cbind, lapply(p., data.frame, stringsAsFactors=FALSE))
  colnames(p.) = colnames(p)
  p = p.
  
  if (colnames(p)[1] != "label") {
    p_label <- 1:ncol(p)
    p = p[,c(which(colnames(p) == "label"), p_label[-(which(colnames(p) == "label"))])]
  }
    
  p[[1]] = make.unique(make.names(p[[1]]), sep = '_')
  
  e <- d[!is.na(d[,1]), !is.na(d[1,])] [-1,-1]
  e <- sapply(e, as.numeric)
  e <- data.frame(e, stringsAsFactors = F)
  colnames(e) = p[[1]]; rownames(e) = f[[1]]
  
  return (list(eData = e, fData = f, pData = p))
}
