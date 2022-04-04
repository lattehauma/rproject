prop.test(c(189,104),c(11034,11037), conf.level=0.95, correct=FALSE)


#풀이방법
pi.c=(189+104)/(11034+11037)
std.err=sqrt(pi.c*(1-pi.c)*(1/11034+1/11037))
p1=189/11034
p2=104/11037
z.stat=(p1-p2)/std.err
z.stat^2

1-pchisq(z.stat^2,df=1)


##피어슨 방법으로 검증하기
t=matrix(c(189,104,10845,10933),nrow=2,ncol=2)
chisq.test(t,correct=FALSE)


#CI
library(PropCIs)
diffscoreci(189,11034,104,11037,conf.level=0.95)
