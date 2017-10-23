










#name function lab1
#parameters of function are x and y
lab1 = function(x,y){



#first estimate coefficients, no need to change names
beta.hat = solve(t(X) %*% X) %*% t(X) %*% Y
beta.hat
  
#store it
result = as.data.frame(cbind(c("(Intercept)","sex","age", "rural"),beta.hat))
colnames(result) = c("Coeff.","Estimate" )
result
  
#calculate the standard errors
y.hat = beta.hat[1] + beta.hat[2]*bcs$sex+ beta.hat[3]*bcs$age + beta.hat[4]*bcs$rural
y.hat = X %*% beta.hat 
#epsilon = bcs$tcviolent - y.hat  
n = nrow(bcs)
k = ncol(X) 
vcov = 1 / (n-k) * as.numeric(t(epsilon) %*% epsilon) * solve(t(X) %*% X)
SE = sqrt(diag(vcov))

# calculate residual quartiles (5 needed for:
# minimum, 1. quantil, median, 3. quantil and maximun = 5)
#save it as result2
res.distr = quantile(epsilon,p=seq(0,1,length.out=5))
result2 = res.distr
result2

#calculate t-value
t = beta.hat / SE

#and p-values
p = 2*pt(abs(t), df=n-k,lower.tail= FALSE)

#calculate residual SE
residual.SE = sqrt(sum((y.hat-Y)^2)/(n-k))
residual.SE

#calculate adjusted r Squared via multiple r squared
mult.r.squared = sum((y.hat - mean(Y))^2) / sum((Y-mean(Y))^2) 
mult.r.squared

adj.r.squared = 1 - (1 - mult.r.squared)*(n-1) / (n-k)
adj.r.squared

#f-stats and overall p-value
f =  mult.r.squared/(k - 1) / ((1 - mult.r.squared) / (n-k))
f
f.p = pf(67.31, k-1, n-k, lower.tail=F)
f.p

#now we have all the values, it goes into presenting them
#like summary(model)
#result 2 = quantils, result = coefficients,
#build coefficient table
result$'Std. Error' = SE
result$'t value' = t
result$'Pr(>|t|)' = p
stars = ifelse(p<0.001,"***",ifelse(p<0.01,"**",ifelse(p<0.05,"*",ifelse(p<0.1,"."," "))))
result$' ' = stars
result

#build r squared line as result 3
result3=cbind(mult.r.squared,adj.r.squared)
result3

#build f-stats and p value line as result4
result4=cbind(f, f.p)
result4

#combine all
result=list(result2,result,result3,result4)
result

#cat(paste0) command gave me an unreadable output i couldnt solve. so i used list.
#return command doesnt achieve for me anything here it seems. 
#even return(result) doesnt give me the results directly, so
# i dont understand the function of the return command.

  }
  

    
  