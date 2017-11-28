# Readxl
library(readxl)
#readData = function(path = "E:\\Documents\\Programming\\R\\excel\\example.xlsx"){
  path = "E:\\Documents\\Programming\\R\\excel\\example.xlsx"
# package only processes excel
# may be able to subset in one line of code?
  
  tmp = readxl::read_excel(path, range = cell_limits(c(4, NA), c(NA, 2)), col_names = FALSE)
  
    #___INDEX_DATA
  indData <- readxl::read_excel(path, sheet = 1, range = cell_limits(c(NA,NA), c()))
  
  #___SA_DESC
  
  
  #___SA_DATA
  
#}
