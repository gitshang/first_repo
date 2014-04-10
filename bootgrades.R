##################################################################
###  BOOTSTRAP LSAT/GPA EXAMPLE WITH CONFIDENCE INTERVALS ########
##################################################################



data=matrix(c(576,3.39,635,3.30,558,2.81,578,3.03,
666,3.44,580,3.07,555,3.00,661,3.43,651,3.36,605,3.13,
653,3.12,575,2.74,545,2.76,572,2.88,594,2.96), byrow=T,ncol=2)

LSAT=data[,1]
GPA=data[,2]

# Pearson Correlation Coefficient is:

cor(LSAT,GPA)

# .776

# Use the bootstrap to find the standard error for
# the correlation coefficient.

T = cor(LSAT,GPA)
Tboot = c()
B=500
n=15

for(i in 1:B){
   boot.obs=sample(seq(1,15),n,replace=T)
   Xstar=data[boot.obs,]
   Tboot[i] = cor(Xstar[,1],Xstar[,2])
   }
se = sqrt(var(Tboot))

postscript("boot.corr.hist.ps")
hist(Tboot)
dev.off()

###########################################################
#### PRODUCE THE CONFIDENCE INTERVALS FOR CORRELATION #####
###########################################################

T = cor(LSAT,GPA)
Tboot = c()
B=500
n=15

for(i in 1:B){
   boot.obs=sample(seq(1,15),n,replace=T)
   Xstar=data[boot.obs,]
   Tboot[i] = cor(Xstar[,1],Xstar[,2])
   }
se = sqrt(var(Tboot))

# 95% CI's

Normal = c(T+qnorm(0.025)*se, T-qnorm(0.025)*se)
pivotal = c(2*T-quantile(Tboot,.975),2*T-quantile(Tboot,.025))
percentile = c(quantile(Tboot,.025),quantile(Tboot,.975))










