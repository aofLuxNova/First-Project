# Writing into xlsx/csv
#library(gtools, openxlsx)
library(openxlsx)
source("t_test_foldchange_reading.r")
my_list <- readData(path = "E:\\Documents\\Programming\\R\\excel\\mx 368045 Jayoung Kim_human urine_09-2017_submit.xlsx")

# write.csv(my_list$eData, file = "e.csv", append = FALSE, qmethod = D, quote = TRUE, row.names = TRUE)
# write.csv(my_list$fData, file = "f.csv", append = FALSE, qmethod = D, quote = TRUE, row.names = FALSE)
# write.csv(my_list$pData, file = "p.csv", append = FALSE, qmethod = D, quote = TRUE, row.names = FALSE)

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
list.p.neglog = -log10(as.numeric(list.p))


# fold change

sa.avg <- lapply(sa, mean)
sb.avg <- lapply(sb, mean)

# Fold change = Mean male peak intensity / Mean female peak intensity
sa.f = mapply(function(x,y){
  x/y
},x=sa.avg,y=sb.avg)

sa.f.log = log2(sa.f)

#result = cbind(list.p, sa.f, list.p.neglog, sa.f.log)
#colnames(result) = c("p-value","fold change","Negative log10(p-value)","log2(fold change)")
#write.csv(result, file = "Kim_Result.csv", append = FALSE, qmethod = D, quote = TRUE)

par(mar = c(5,5,2,1), mgp = c(3.5, 2, 1), tck = -0.02)
plot(sa.f.log, list.p.neglog, xlim = c(-4,4), ylim = c(0,10), 
     main = "Male vs. Female \nUrine Metabolic Profile Differences", 
     xlab = "Log2 Fold Change", ylab = "-log10 p-value")
