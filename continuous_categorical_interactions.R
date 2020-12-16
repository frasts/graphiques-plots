
#######################################################################################################################
################### how to plot results of mixed models with an interaction between continuous variable and a factor###

### dataset simulation ###
#100 ids with 100 reps
# y = continuous dependent variable
# x1 and x2 continuous covariates
# x3 categorical variable with two levels

    nobs = 100
    nind = 100
    indbt <- rnorm(n = nind, mean = 0, sd = 2)
    id <- rep(1:nind, each = nobs)
    x1 <- runif(min = 0, max = 1, n = nobs*nind)
    x2 <- runif(min = 0, max = 1, n = nobs*nind)
    y <- 2 + x1*3 + x2*(-2) + 0.5*x1*x2 + rep(indbt, each = nobs) + rnorm(sd = 2, n = nobs*nind)
    x3<- sample(c('A', 'B'), 10000, replace=TRUE)

###organizing the dataset ###

    data<-cbind(id,x1,x2,x3,y)
    data<-as.data.frame(data)
    str(data)

    data$x1<-as.numeric(as.character(data$x1))
    data$x2<-as.numeric(as.character(data$x2))
    data$y<-as.numeric(as.character(data$y))

### running the model ###

    library(lme4)
    library(arm)

#we use one continuous covariate, one categorical, and fit their interaction
#indvidual as random effect as there are repeated measures

    mod<-lmer(y ~   x1 +  x3  +  x1*x3 +(1|id), data=data)

# for more on results you can check the github section on bayesian inference in mixed model

   library(effects)
   library(ggplot2)


### preparing the df we will use for the graph ###
#documentation on effects: https://cran.r-project.org/web/packages/effects/effects.pdf

df<-(effect("x1:x3",mod))
df <- data.frame(df)

#we look at the data frame we'll use to build the graph
df

#fit is the model fitted value for the interaction of x1 and for each level of x3, with associated standard error and lower and upper confidence intervals

### making a very basic graph ###
#note that a lot of nice examples of graphs and code, to make these basic graphs much fancier are here: https://cran.r-project.org/web/packages/effects/vignettes/predictor-effects-gallery.pdf
#check the ggplot2 documentation for all the parameters you can change in the functions within

#basic plot
pl<-ggplot(df, aes(x=x1, y=fit, group=x3))+  #x is your continuous var and group your categorical var
geom_line(aes(linetype=x3))+geom_ribbon(aes(ymin=lower, ymax=upper, fill=x3, group=x3),alpha=0.2) +
xlab("x1")+ylab("y")+
theme_classic() + theme(text=element_text(size=18))

pl

#making thicker axes/labels
f<-pl + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black")) +
theme(axis.text = element_text(size = 18,colour =  'black'))+ theme(title =element_text(size=18,
colour= 'black'))+theme( axis.line = element_line(colour = "black", size = 1.1, linetype = "solid")) +
theme(axis.ticks = element_line(colour = "black", size=(0.9)))

f

#here you can change the colors, these are for publication in black and white and greyscale
yy<-f+scale_fill_manual(values=c("grey49", "black"))+scale_color_manual(name="x3",
values=c("black", "gray"))+theme(legend.title=element_text(size=1))#+
#ggtitle("title")

yy



