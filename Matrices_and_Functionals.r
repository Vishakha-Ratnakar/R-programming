
## Matrices and functionals

set.seed(100)
N = 20
CX101 <- rnorm(N,45,8)
CX102 <- rnorm(N,65,8)
CX103 <- rnorm(N,85,10)
CX104 <- rnorm(N,45,10)
CX105 <- rnorm(N,70,1)

#Create the following matrix – rounding 
#of numbers is required, use the round() function

res <- round(matrix(c(CX101,CX102,CX103,CX104,CX105),ncol = 5),digits = 2)
colnames(res) <- c("CX101","CX102","CX103","CX104","CX105")
rownames(res) <- c("Student_1","Student_2","Student_3","Student_4"
                   ,"Student_5","Student_6","Student_7","Student_8",
                   "Student_9","Student_10","Student_11","Student_12",
                   "Student_13","Student_14","Student_15","Student_16"
                   ,"Student_17","Student_18","Student_19","Student_20")

res

#Write an apply function to convert any invalid values (outside of [0,100])
#to NA. This should produce the
#following. Note the invalid values in column CX103 have changed

res1 <- t(apply(res,1,function(x) replace(x,x<0 | x>100,NA)))
res1

#replace the missing values (for any column) with the mean of the 
#valid values in that column (this is just
#to show an example of imputation)

res2 <- apply(res1, 2, function(x) 
  ifelse(is.na(x),round(mean(x,na.rm=TRUE),digit = 2),x))
res2

#Min, SD, Mean and Rank foor each row is printed in matrix
# r contains only the matrix with min, Mean and SD
#it is then bind with rank separtely.

r <- t(apply(res2,1,function(x)
  round(c("Min"=min(x),"SD"=sd(x),"Mean"=mean(x)),digit = 2)))
k <- cbind(res2,r)

final <- cbind(k,"Rank"=rank(k[,"Mean"]))
final

#the line of code that will produce the following 
#(student with the highest average), making use of the
#which() function to get an index value.

final[which(final[,"Mean"]==max(final[,"Mean"])),,drop = F]

#summary statistics for each subject by adding
#new rows at the end of the cleaned data set.

r <- apply(res2,2,function(x)
  round(c("Subject_Mean"=mean(x),"Subject_Max"=max(x),"Subject_Min"=min(x)),digit = 2))
subj_summ <- rbind(res2,r)
subj_summ












