##연습

pi.hat=400/893
z.score=(pi.hat-0.5)/sqrt(0.5*(1-0.5)/893)
pnorm(z.score)
2*pnorm(z.score)

moe=qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/893)
ci=c(pi.hat-moe,pi.hat+moe)


##세가지 방법 연습

#1wald
wald.stat=(0.9-0.5)/sqrt(0.9*0.1/10)
2*(1-pnorm(wald.stat))

#score
score.stat=(0.9-0.5)/sqrt(0.5*0.5/10)
2*(1-pnorm(score.stat))

##LR은 항상 one-sided
LRT.stat= 2*(9*log(9/5)+1*log(1/5))
1-pchisq(LRT.stat,1)




###연습문제

