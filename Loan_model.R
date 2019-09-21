library(C50)
credit=read.csv('credit.txt')
str(credit)
# Entropy curve
# Default values of from and to are also 0 and 1
curve(-x * log2(x) - (1 - x) * log2(1 - x),from=0,to=1,col = "red", xlab = "x", ylab = "Entropy",lwd=5)

