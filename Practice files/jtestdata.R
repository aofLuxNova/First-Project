#readData = function(
  path1 =  "E:\\Documents\\Programming\\R\\First-Project\\jtestdata.xlsx" #){
  
  #check if it is csv of xlsx
  if(grepl("xlsx", path1)){
    d <- openxlsx::read.xlsx(path1, sheet = 1,colNames = FALSE)
  }else if(grepl("csv", path1)){
    # file = "C:\\Users\\Sili Fan\\Downloads\\val (18).csv"
    d <- data.table::fread(path)
  }
  
  # make "" as NA
  d[d==""] <- NA
  
  fData <- d[!is.na(d[,1]),c(which(is.na(d[1,])),sum(is.na(d[1,]))+1)]
  colnames(fData) = as.character(fData[1,]);fData = data.frame(fData[-1,],stringsAsFactors = F,check.names = FALSE);rownames(fData) = 1:nrow(fData)
  fData.=lapply(fData,function(x){
    if(sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    }else{
      x
    }
  })
  
 # fcheck <- lapply(fData., data.frame, stringsAsFactors=FALSE)
                 
  fData. = do.call(cbind, lapply(fData., data.frame, stringsAsFactors=FALSE))
  colnames(fData.) = colnames(fData)
  fData = fData.
  
  fData = fData[,c(ncol(fData),2:ncol(fData)-1)]
  fData[[1]] = make.unique(fData[[1]], sep = '_')
  
  # pData
  sa_description <- d[c(which(is.na(d[,1])),max(which(is.na(d[,1])))+1), !is.na(d[1,])]
  
  colnames(sa_description) <- sa_description[,1]
  
  sa_description <- data.frame(sa_description[,-1], stringsAsFactors = FALSE, check.names = FALSE)
  colnames(sa_description) = 1:ncol(sa_description)
  
  
  
  
  
  
  #}
