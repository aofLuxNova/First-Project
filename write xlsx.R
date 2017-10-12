# Writing into xlsx/csv
source("read.xlsx readData.R")
my_list <- readData(path = "E:\\Documents\\Programming\\R\\excel\\example.xlsx")

eData <- cbind(rownames(my_list$e), my_list$e)
colnames(eData) = c(colnames(my_list$p)[1], my_list$p[[1]])

write.csv(my_list$p, file = "p.csv", row.names = FALSE)
write.csv(my_list$f, file = "f.csv", row.names = FALSE)
write.csv(eData, file = "e.csv", row.names = FALSE)