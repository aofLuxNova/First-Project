library(readxl)

#read_excel(path, sheet = "sub")

#read_excel(path, range = cell_cols("A:H"))

# goal: subset first column
path = "E:\\Documents\\Programming\\R\\First-Project\\example.xlsx"
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

