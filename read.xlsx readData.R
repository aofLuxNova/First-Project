# Studying reference code

# Make a function and later return subset data
readData = function(path = "E:\\Documents\\Programming\\R\\excel\\example.xlsx"){

#check file extension
# if(grepl(".xlsx", path)){
    d <- openxlsx::read.xlsx(path, sheet = 1, colNames = FALSE) # only reads FIRST sheet
# }
# else if(grepl(".csv", path)){
#   d <- data.table::fread(path)
# }
  
  # make all blanks NA
  d[d == ""] <- NA
  
  #______________INDEX_DATA
  # d[rows that do not have NA, columns that have NA plus one "label" column]
  index_Data <- d[which(!is.na(d[,1])), c(which(is.na(d[1,])), max(which(is.na(d[1,])))+1)]
  # Need to name columns
  # Force first row to be character type, saved column names
  colnames(index_Data) <- as.character(index_Data[1,])
  # Delete first row from index_Data.
  index_Data <- data.frame(index_Data[-1,], stringsAsFactors = FALSE, check.names = FALSE)
  # renumber rows, saved row numbers
  rownames(index_Data) <- 1:nrow(index_Data)
  
  # lapply coerces index_Data into list, then applies FUN to each column in index_Data
  # If column is numeric, all cells return TRUE, and sum of column = length of column. Keep/change column to numeric.
  # If column contains elements with characters, some/all cells will be coerced to NA. Sum of column != length. Keep original type.
  index_temp = lapply(index_Data, function(x){
    if (sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    } else {
      x
    }
  }
  )
  
  # index_temp is a list. Turn index_temp into data.frame
  index_temp = do.call(cbind, lapply(index_temp, data.frame, stringsAsFactors = FALSE))
  colnames(index_temp) <- colnames(index_Data)
  index_Data = index_temp
  
  # Make "labels" first column
  index_Data = index_Data[, c(ncol(index_Data), 2:ncol(index_Data)-1)]
  # Make any repeated labels unique (this is writing, do it last)
  index_Data[[1]] = make.unique(index_Data[[1]], sep = '_')
  
  #__________SA_DESC
  # d[rows with NA +1, columns that are not NA]
  sa_Desc <- d[c(which(is.na(d[,1])),max(which(is.na(d[,1])))+1), !is.na(d[1,])]
  
  # swap rows and columns with t()
  sa_Desc = t(sa_Desc)
  colnames(sa_Desc) <- sa_Desc[1,]
  
  sa_Desc <- data.frame(sa_Desc[-1,], stringsAsFactors = FALSE, check.names = FALSE)
  
  # preserve column type
  sa_temp = lapply(sa_Desc, function(x){
    if (sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    } else {
      x
    }
  })
  
  sa_temp = do.call(cbind, lapply(sa_temp, data.frame, stringsAsFactors = FALSE))
  colnames(sa_temp) <- colnames(sa_Desc)
  sa_Desc = sa_temp
  
  sa_Desc = sa_Desc[, c(ncol(sa_Desc), 2:ncol(sa_Desc)-1)]
  sa_Desc[[1]] = make.unique(make.names(sa_Desc[[1]]), sep = '_')
  
  #__________SA_DATA
  # d[first column without NA, first row without NA], then delete column and row names
  sa_Data <- d[!is.na(d[,1]), !is.na(d[1,])][-1,-1]
  # sa_Data is a data.frame of numerical values. Keep all as.numeric, no need to check each column.
  sa_Data <- sapply(sa_Data, as.numeric)
  # remake data.frame
  sa_Data <- data.frame(sa_Data, stringsAsFactors = FALSE)
  # column and row names can be taken from above
  rownames(sa_Data) = index_Data[[1]]
  
  # Replace unwanted characters in column names with "_"
  # colnames(index_Data) = gsub("([_])[[:punct:]]","_",colnames(index_Data))
  # colnames(sa_Desc) = gsub("([_])[[:punct:]]","_",colnames(sa_Desc))
  # colnames(sa_Data) = gsub("([_])[[:punct:]]","_",colnames(sa_Data))
  
  # Make NA into "NA" to avoid unknown parameter error
  index_Data[is.na(index_Data)] = "NA"
  sa_Desc[is.na(sa_Desc)] = "NA"
  sa_Data[is.na(sa_Data)] = "NA"

  # 
  return(list(p = sa_Desc, f = index_Data, e = sa_Data))

}