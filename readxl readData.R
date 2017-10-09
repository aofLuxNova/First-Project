# Readxl
readData = function(path = "E:\\Documents\\Programming\\R\\First-Project\\example.xlsx"){
  
# package only processes excel
# may be able to subset in one line of code?

  
  #___INDEX_DATA
  index_Data <- readxl::read_excel(path, sheet = 1, range = cell_limits(,))
  
  #___SA_DESC
  
  
  #___SA_DATA
  
}