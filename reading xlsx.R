# Make a function and later return subset data
#readData = function(){}
path = "E:\\Documents\\Programming\\R\\First-Project\\example.xlsx"
  
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
  
  #____________sa_desc
  # d[rows with NA +1, columns that are not NA]
  sa_desc <- d[c(which(is.na(d[,1])),max(which(is.na(d[,1])))+1), !is.na(d[1,])]
  
  # swap rows and columns with t()
  sa_desc = t(sa_desc)
  colnames(sa_desc) = sa_desc[1,]
  sa_desc <- data.frame(sa_desc[-1,], stringsAsFactors = FALSE, check.names = FALSE)
  
  sa_temp = lapply(sa_desc, function(x){
    if (sum(!is.na(as.numeric(x))) == length(x)){
      as.numeric(x)
    } else {
      x
    }
  }
  )
  
  sa_temp = do.call(cbind, lapply(sa_temp, data.frame, stringsAsFactors = FALSE))
  colnames(sa_temp) <- colnames(sa_desc)
  sa_desc = sa_temp
  
  sa_desc = sa_desc[, c(ncol(sa_desc), 2:ncol(sa_desc)-1)]
  sa_desc[[1]] = make.unique(sa_desc[[1]], sep = '_')
  
  


#read_excel(path, sheet = "sub")

#read_excel(path, range = cell_cols("A:H"))

# goal: subset first column

read_excel(path)
dta = read_excel(path)

#dta_row = dta[1:length(dta$X__1),1]
#is.na(dta_1)

# Check first column for TRUE/FALSE for NA
# Sum all TRUE for # of NA
# Set a variable to # of NA +1 for column start
row_start = sum(is.na(dta[,1]))
#print(row_start)

# Do same for rows
col_start = sum(is.na(dta[1,]))+1
#print(col_start)


len = nrow(dta)
#print(len)
wid = ncol(dta)
#print(wid)

# Read lower left portion
# Number values: dta[4:435,1:9]
dta[row_start:len+1,1:col_start]

# Read upper right portion
# Number values: dta[0:5,9:57]
dta[0:row_start+1,col_start:wid]

# Read label data
# Number values: dta[4:435,9:57]
dta[row_start:len+1,col_start:wid]

