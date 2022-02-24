#1
x<-c(2,5,3)
rep(x,5)
rep(x,3,10)
rep(x,c(2,5,3))
#2
y=matrix(1:12, nrow=3)
colnames(y)<-letters[1:4]
rownames(y)<-letters[1:3]
y
#3
y1=y[,c(1,3)]
y2=y[,c(2,4)]
y1+y2
y1-y2
y1*y2
y1/y2
y1%*%y2#오류발생
y2%*%y1#오류
#4
z=matrix(1:9999, ncol=9)
z
z[c(1109:1111), c(8:9)]