# Student's t-test https://www.r-bloggers.com/two-sample-students-t-test-1/
# method of testing whether or not the means of two samples, extracted from a normally distributed population
# when the population standard deviation is unknown, has significant difference compared to the hypothesized
# population mean.

# Question: Difference between two sets of data negligible due to random fluctuations or significant?

sa = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
sb = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)

# variance test gives p = 0.2834 > 0.05, meaning two variances are homogeneous
# F = 2.1028
var.test(sa, sb)

# F-value in table
f_value = qf(0.975, 9, 9)
# homogeneous variance
isTRUE(f_value > 2.1028)

# p = 0.356 > 0.05
# Means of two samples are not significantly different
t.test(sa, sb, var.equal = TRUE, paired = FALSE)

t_value = qt (0.975, 9+9)
isTRUE(t_value > -0.94737)

# Result: Null hypothesis accepted

# Need two sample vectors for male and female
# subject both under t.test()

# https://statistics.berkeley.edu/computing/r-t-tests
# Calculate t-stat that follow a t-dist with n1+n2-2 degrees of freedom
sa = rnorm(20)
sb = rnorm(20)

t.test(sa,sb)


# t-test of xylose
path = "E:\\Documents\\Programming\\R\\excel\\Jayoung Kim one cmpd.xlsx"
d <- openxlsx::read.xlsx(path, sheet = 1, colNames = T)
d[d == ""] <- NA

rgender <- colnames(d)
sa = d[,which(rgender == "Male")]
sb = d[,which(rgender == "Female")]
t.test(sa, sb)

# report: A Student's t-test was performed to determine that the difference of xylose peaks 
# between male and female samples was statistically significant. A threshold p-value of 0.05
# was used for the test.

# t-test of xylose and xylonic acid isomer


