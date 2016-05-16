###############################################
# BUG example to run on HPC
# 1/06/2015
# code (to run simple BUG example) taken from
# http://rstudio-pubs-static.s3.amazonaws.com/1935_652d6fb17d7941c4b91bbbc5a22d4494.html

library(R2OpenBUGS)
library(ggplot2)
library(coda)
library(reshape2)

# generate data
linedata <- list(Y = c(1, 3, 3, 3, 5), x = c(1, 2, 3, 4, 5), N = 5, xbar = 3)
lineinits <- function() {
    list(alpha = 1, beta = 1, tau = 1)
}

## Uses default settings for n.burnin = n.iter/2; n.thin=1;
lineout <- bugs(data = linedata, inits = lineinits, parameters.to.save = c("alpha", 
    "beta", "sigma"), model.file = "BUG_exampleHPC_model.txt", n.chains = 2, n.iter = 10000,
	codaPkg=TRUE)

lout<- read.bugs(lineout)

# want to save the summary of the output (parameter estimates, etc)
# into a text file
out<- capture.output(summary(lout))
cat(out, file= "modelSummary.txt", sep="\n", append=TRUE)

mcmc<-lout
for (j in 1:2) { 
  mcmc[[j]] <- as.data.frame(mcmc[[j]])
  n <- dim(mcmc[[j]])[1]
  mcmc[[j]][,"id"] <- 1:n
  mcmc[[j]][,"chain"] <- rep(j,n)
}

mcmcs <- rbind(mcmc[[1]], mcmc[[2]])
chains<- melt(mcmcs, id.vars=c(5,6))  
no.param<- 4

dim(chains)
chains$chain <- factor(chains$chain)
this <- 1:dim(chains)[1]

#                       #### diagnostics ######
# TRACE PLOTS
trace.p<- ggplot(aes(x=id, y=value, colour=chain), data=chains[this,]) +      
  geom_line() +                   
  facet_wrap(~variable, ncol=2, scales = "free") +
  theme(legend.position="top") +  
  labs(x="MCMC iteration", y="Simulation from parameter's marginal posterior")
ggsave(file = "TracePlot.pdf", plot = trace.p)
dev.off()

# AUTOCORRELATION, for each parameter estimate
pdf("autocorrelation.pdf")
par(mfrow=c(2,2))
for (v in 1:no.param) { 
  acf.df <- acf(mcmcs[,v], plot=F) # $acf[,,1] 
  plot(acf.df, ask=T, xlab=dimnames(mcmcs)[[2]][v], main="", ylim=range(0,1))
}
dev.off()

# DENSITY
density.p<- ggplot(aes(x=value, colour=chain), # separate box for each chain
       data=chains) +                 # data must be in long format
  geom_density() +               # use density geometry
  facet_wrap(~variable, ncol=2,  # do a separate boxplot for each variable
             scales = "free")
ggsave(filename="densityPlot.pdf", plot = density.p)
dev.off()

# this is how we save a plot in qq-environment
#getwd()
#ggsave(filename=  "E:/PINKMEMSTIK/SpatioTemporal_attemp3/plot1.pdf", plot = p1)
