# Writing into xlsx/csv
source("reading xlsx.R")
my_list <- readData(path = "E:\\Documents\\Programming\\R\\excel\\example.xlsx")

write.csv(my_list$p, file = "p.csv", row.names = FALSE)
write.csv(my_list$f, file = "f.csv", row.names = FALSE)
write.csv(my_list$e, file = "e.csv", row.names = FALSE)


