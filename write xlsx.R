# Writing into xlsx/csv
library(gtools, openxlsx)
source("t_test_foldchange_reading.r")
my_list <- readData(path = "E:\\Documents\\Programming\\R\\excel\\mx 368045 Jayoung Kim_human urine_09-2017_submit.xlsx")

e <- my_list$eData
colnames(e) = my_list$pData$comment
rgender <- colnames(e)


# t-test

sa = e[,which(rgender == "Male")]
sb = e[,which(rgender == "Female")]
sa = t(sa); sa = as.data.frame(sa)
sb = t(sb); sb = as.data.frame(sb)

sa.p = mapply(function(x,y){
  t.test(x,y)
},x=sa,y=sb)

list.p <- sa.p["p.value",]

# fold change (gtools)

sa.avg <- lapply(sa, mean)
sb.avg <- lapply(sb, mean)

# Fold change = Mean male peak intensity / Mean female peak intensity
sa.f = mapply(function(x,y){
  foldchange(x,y)
},x=sa.avg,y=sb.avg)

list.f <- as.list(sa.f)

result = data.frame(cbind(list.p,list.f), row.names = rownames(e)); colnames(result) = c("p-value","fold change")

openxlsx::write.xlsx(result, file = "result.xlsx", colNames = TRUE, rowNames = TRUE)
