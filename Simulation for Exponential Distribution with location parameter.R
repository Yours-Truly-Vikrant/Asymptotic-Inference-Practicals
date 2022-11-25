#Consistency-I
#Q6
#Let x be a r.v having exponential distribution with location parameter theta. Conduct simulation Study to demonstrate consistency of estimator of theta.
rm(list=ls(all=T))
n=c(50,100,250,500,750,1000);
theta=2.6;eps=0.05;
Y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]);
est.prop_1=0;Est.prob2=0;
for (i in 1:length(n)) 
{
  Y=matrix(runif(n[i]*n[i],0,1),n[i],n[i]); #CDF OF Uniform (0,theta) follow Uniform(0,1)
  x=theta-log(1-Y);
  T1=apply(x,1,min);
  T2=apply(x,1,mean);
  est.prop_1[i]=mean(abs(T1-theta)<eps);
  Est.prob2[i]=mean(abs(T2-theta)<eps);
 
}
cbind(n,est.prop_1,Est.prob2)
