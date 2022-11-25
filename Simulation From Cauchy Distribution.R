#Consistency-I
#Simulation from cauchy distribution
#Q.1 
#Based on the random samples generated from Cauchy(mu,1). Check the consistency of the estimators T1=xbar and T2=Sample Median for the parameter mu. Construct the following table.
rm(list = ls(all=TRUE))
mu=1.4; #mu
sigma=1; #var
eps=0.1; #epsilon
n=50; #no of samples
x=matrix(rcauchy(n*n,mu,sigma),n,n);
T_1=apply(x,1,mean);
T_2=apply(x,1,median);
A=abs(T_1-mu)<eps;
est.prob_1=sum(A)/n;
B=abs(T_2-mu)<eps;
est.prob_2=sum(B)/n;
cbind(n,est.prob_1,est.prob_2)


#OR

rm(list=ls(all=T))
mu=1.4;
sigma=1;
eps=0.1;
n=c(50,100,200,500,700,1000);
est_prob1=0;est_prob2=0;
for (i in 1:length(n))
{
  x=matrix(rcauchy(n[i]*n[i],mu,sigma),n[i],n[i])
  T1=apply(x,1,mean); #samples means
  T2=apply(x,1,median); #samples median
  est_prob1[i]=mean(abs(T1-mu)<eps)
  est_prob2[i]=mean(abs(T2-mu)<eps)
  
}
cbind(n,est_prob1,est_prob2)

#Sample mean is not Consistent for mu.
#Sample median is Consistent for mu.