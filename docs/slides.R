#| include: false
library(np)


#| echo: true
#| eval: false
## ## Generate some data: sex (unordered categorical), income (ordered categorical),
## ## and height (numeric)
## n <- 100
## sex <- sample(c("Female","Male"),n,replace=TRUE,prob=c(.4,.6))
## income <- sample(c("Low","Middle","High"),n,replace=TRUE,prob=c(.3,.5,.2))
## height <- rnorm(n,mean=150,sd=20)
## ## Note - by default these variables may not be the data types we desire
## class(sex);class(income);class(height)
## ## income is already numeric(), but sex and height are character()
## ## sex is categorical and unordered, so cast as type factor()
## sex <- factor(sex)
## ## income is categorical and ordered, but we need to ensure intended order (it
## ## will assume alphabetical ordering otherwise). Suppose you ignore it - let's
## ## see what happens when we just cast as type ordered() using defaults
## income <- ordered(income)
## ## The levels are in alphabetical order, which we don't want
## levels(income)
## ## We shall reorder the ordered factor levels as intended using levels=...
## income <- ordered(income,levels=c("Low","Middle","High"))
## levels(income)
## ## Check data types again
## class(sex);class(income);class(height)
## ## Note that with integers the default ordered() works fine
## x <- sample(c(2,5,4,3,1),n,replace=TRUE)
## x <- ordered(x)
## levels(x)


#| eval: true
#| echo: true
## Let's simulate a random numeric sample from the normal distribution
set.seed(42)
n <- 1000
x <- rnorm(n)
## Let's sort the data so we can graph x versus dnorm(x,...) using lines (type="l")
x <- sort(x)
## Since we simulated the data, let's plot the true, known, parametric density
## (we can't do this with actual data because the density of such data is, in
## general, unknown)
plot(x,dnorm(x,mean=mean(x),sd=sd(x)),type="l",ylab="Parametric Density Estimate",xlab="X")



pander::pander(shapiro.test(x))



data(faithful)
with(faithful,pander::pander(shapiro.test(eruptions)))



plot(density(faithful$eruptions),main="")
rug(faithful$eruptions)



plot(density(faithful$eruptions),main="")
with(faithful,lines(density(eruptions)$x,dnorm(density(eruptions)$x,mean=mean(eruptions),sd=sd(eruptions)),col=2,lty=2))
rug(faithful$eruptions)
legend("topleft",c("Nonparametric","Parametric"),lty=c(1,2),col=c(1,2),bty="n")



hist(faithful$eruptions,prob=TRUE,main="",xlab="Eruptions",breaks=20,xlim=c(1.25,5.5))
with(faithful,lines(density(eruptions)$x,fitted(npudens(tdat=eruptions,edat=density(eruptions)$x))))
rug(faithful$eruptions)


#| echo: true
library(np)
data(faithful)
fhat <- npudens(~eruptions,data=faithful)
plot(fhat,neval=250,plot.errors.method="bootstrap")


#| echo: true
library(np)
data(faithful)
fhat <- npudens(~eruptions+waiting,data=faithful)
plot(fhat,theta=330,xtrim=-0.05,neval=75,view="fixed",main="")


#| echo: true
n <- 250
set.seed(42)
sex <- sample(c("Female","Male"),n,replace=TRUE,prob=c(.4,.6))
sex <- factor(sex)
phat <- npudens(~sex)
plot(phat,plot.errors.method="bootstrap")


#| echo: true
n <- 250
set.seed(42)
income <- sample(c("Low","Middle","High"),n,replace=TRUE,prob=c(.3,.5,.2))
income <- ordered(income,levels=c("Low","Middle","High"))
phat <- npudens(~income)
plot(phat,plot.errors.method="bootstrap")


#| label: tbl-wage1mixedtable
#| tbl-cap: Counts of number of dependants present in 526 households by cell
library(np)
library(plot3D)
data(wage1)
knitr::kable(with(wage1,t(data.frame(numdep=sort(unique(numdep)),counts=as.numeric(table(numdep))))),
             booktabs=TRUE,
             linesep="")


#| echo: true
#| eval: false
## library(np)
## library(plot3D)
## data(wage1)
## bw <- npudensbw(~lwage+ordered(numdep),data=wage1)
## numdep.seq <- with(wage1,sort(unique(numdep)))
## lwage.seq <- with(wage1,seq(min(lwage),max(lwage),length=50))
## wage1.eval <- expand.grid(numdep=ordered(numdep.seq),lwage=lwage.seq)
## fhat <- fitted(npudens(bws=bw,newdata=wage1.eval))
## ## Hack since scatter3D converts ordered 0-6 to numeric 1-7
## scatter3D(as.numeric(wage1.eval[,1])-1,wage1.eval[,2],fhat,
##           ylab="Log-Wage",
##           xlab="Number of Dependants",
##           zlab="Joint Density",
##           ticktype="detailed",
##           angle=15,
##           box=TRUE,
##           type="h",
##           grid=TRUE,
##           col="blue",
##           colkey=FALSE)


#| label: fig-wage1mixeddensity
#| fig-cap: Mixed-data bivariate kernel density estimate for the joint PDF of lwage (numeric) and numdeps (ordered)
data(wage1)
bw <- npudensbw(~lwage+ordered(numdep),data=wage1)
numdep.seq <- with(wage1,sort(unique(numdep)))
lwage.seq <- with(wage1,seq(min(lwage),max(lwage),length=50))
wage1.eval <- expand.grid(numdep=ordered(numdep.seq),lwage=lwage.seq)
fhat <- fitted(npudens(bws=bw,newdata=wage1.eval))
## Hack since scatter3D converts ordered 0-6 to numeric 1-7
scatter3D(as.numeric(wage1.eval[,1])-1,wage1.eval[,2],fhat,
          ylab="Log-Wage",
          xlab="Number of Dependants",
          zlab="Joint Density",
          ticktype="detailed",
          angle=15,
          box=TRUE,
          type="h",
          grid=TRUE,
          col="blue",
          colkey=FALSE)


#| echo: true
library(np)
set.seed(42)
n <- 1000
x <- sort(runif(n))
dgp <- cos(2*pi*x)
y <- dgp + rnorm(n,sd=0.25*sd(dgp))
ghat <- npreg(y~x)
plot(x,y,cex=0.5,col="grey",xlab="X",ylab="Y")
lines(x,dgp)
lines(x,fitted(ghat),col=2)
abline(ghat.ols <- lm(y~x),col=3)
legend("top",c("DGP","Kernel","OLS"),col=1:3,lty=1,bty="n")



pander::pander(summary(ghat.ols))


#| echo: true
plot(ghat,gradients=TRUE,neval=250)
lines(x,-2*pi*sin(2*pi*x),col=2)
abline(h=coef(ghat.ols)[2],col=3)
legend("topleft",c("Kernel ME","DGP ME","Linear ME"),col=1:3,lty=1,bty="n")


#| echo: true
library(np)
data(wage1)
ghat <- npreg(lwage ~ female + married + educ + exper + tenure, data=wage1, regtype="ll")
summary(ghat)


#| echo: true
## We run out of graph axis dimensions with > 2 predictors, so it is common to
## construct partial plots that plot the fitted model versus each predictor
## separately holding the off-axis predictors at, say, their median value (you
## can change this - see ?npplot and the argument xq)
par(mfrow=c(2,3))
plot(ghat,plot.errors.method="bootstrap")


#| echo: true
par(mfrow=c(2,3))
plot(ghat,gradients=TRUE,plot.errors.method="bootstrap")


#| echo: true
ghat.ols <- lm(lwage ~ female + married + educ + exper + tenure, data=wage1)
summary(ghat.ols)


#| echo: true
npsigtest(ghat)


#| echo: true
attach(wage1)
df <- data.frame(female = factor("Male", levels=levels(female)),
                 married = factor("Notmarried", levels=levels(married)),
                 educ = median(educ),
                 tenure = median(tenure),
                 exper = median(exper))
head(df)
predict(ghat, newdata=df)
## Or you could use ghat <- npreg(...,newdata=df) and fitted(ghat) 
## ghat <- npreg(lwage ~ female + married + educ + exper + tenure, 
##               data=wage1,
##               regtype="ll", 
##               newdata=df)
## fitted(ghat)

