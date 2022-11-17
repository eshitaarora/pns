#assignment2
data <-iris
# str(data)
# iris $ Sepal.Length

#1.1
#sample(c('gold','silver','bronze'),10,replace=T,prob=c(20/100,30/100,50/100))
chest<- c(rep('gold',20),rep("silver",30),rep("bronze",50))
sample(chest,size=10)

#1.2
sample(c('success','failure'),10,replace=T,prob=c(90/100,10/100))

#2
birthday<-function(m){
  return(p<-(1-choose(365,m)*factorial(m)/365^m))
}
n=1000;
count=0;
for (i in 1:n){
  print(birthday(20))
  a=as.integer(any(duplicated(sample(1:365,m,replace=T)))) 
  #elements of vector are duplicates(logical vector)
  #any of the given search terms (logical vector)
  #random 365 samples from a given population with replacement(number is replaced after it is selected,repeated)
  count=count+a;
}
prob=(count/n); #prob of fav upon total outcomes
print(prob);

for(i in 1:365){
  prob=birthday(i)
  if(prob>0.5){
    print(i); 
    break;
  }
}
#3
p_rain<-0.2
p_cloudy<-0.4
p_cloudyRain<-0.85
BayesTheorem<-function(p_rain,p_cloudy,p_cloudyRain)
{
  p_rainCloudy<-p_rain*p_cloudyRain/p_cloudy;
}
prod= BayesTheorem(p_rain,p_cloudy,p_cloudyRain)
print(prod)


#4
head(iris) 
str(iris)
range(data$Sepal.Length)
mean(iris$Sepal.Length) 
median(iris$Sepal.Length)
quantile(data$Sepal.Length,0.25)
quantile(data$Sepal.Length,0.75)
IQR(data$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)
#we have to analyse the in built datasets

#5
arr<-c(11,18,19,21,29,46,21)
mode<-function(v){
  a <- unique(v)
  a[which.max(tabulate(match(v,a)))]
}
result<-mode(arr)
print(result) 





#assinment3
pbinom(9,size=12,1/6)-pbinom(6,12,1/6)
dbinom(7,12,1/6)+dbinom(8,12,1/6)+dbinom(9,12,1/6)

#2
pnorm(100,72,15.2)-pnorm(84,72,15.2)

#3
dpois(0,5)
ppois(50,50)-ppois(47,50)

#4
dhyper(3,17,233,5)

#5.1
pnorm(31,0.447)
#5.2
x<-0:31
y=dbinom(0:31,31,0.447)
plot(x,y)
#5.3
z=pbinom(0:31,31,0.447)
plot(x,z)
#5.4
mean(pbinom(0:31,31,0.447))
sd(pbinom(0:31,31,0.447))
var(pbinom(0:31,31,0.447))



#3
p_rain<-0.2
p_cloudy<-0.4
p_cloudyRain<-0.85
BayesTheorem<-function(p_rain,p_cloudy,p_cloudyRain)
{
  p_rainCloudy<-p_rain*p_cloudyRain/p_cloudy;
}
prod= BayesTheorem(p_rain,p_cloudy,p_cloudyRain)
print(prod)


#4
head(iris) 
str(iris)
range(data$Sepal.Length)
mean(iris$Sepal.Length) 
median(iris$Sepal.Length)
quantile(data$Sepal.Length,0.25)
quantile(data$Sepal.Length,0.75)
IQR(data$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)
#we have to analyse the in built datasets

#5
arr<-c(11,18,19,21,29,46,21)
mode<-function(v){
  a <- unique(v)
  a[which.max(tabulate(match(v,a)))]
}
result<-mode(arr)
print(result) 


#assignment4
x<-c(0,1,2,3,4)
p<-c(0.41,0.37,0.16,0.05,0.01)
a<-sum(x*px)
b<-weighted.mean(x,p)
c<-c(x%*%p) #matrix multiplication
print(a)
print(b)
print(c)

#2
f=function(T){T*0.1*exp(-0.1*T)}
E<-integrate(f,lower=0,upper=Inf)
print(E$value)

#3
x<-c(0,1,2,3)
p<-c(0.1,0.2,0.2,0.5)
sum(((12*x)+(3-x)*2-(3*6))*p)

#4
f1<-function(x){x*0.5*exp(-abs(x))}
m1<-integrate(f1,lower=1,upper=10)
print(m1$value)
f2<-function(x){x*x*0.5*exp(-abs(x))}
m2<-integrate(f2,lower=1,upper=10)
print(m2$value)
f3<-function(m1,m2){return(m2-m1^2)}
var<-f3(m1$value,m2$value)
print(var)

#5
func<-function(x){return((3/4)*(1/4)^(x-1))}
x<-c(1,2,3,4,5)
print(func(3))
mean<-sum((x^2)*func(x))
var<-sum((x^4)*func(x))-(mean^2)
print(mean)
print(var)



#assignment5
#1
d=punif(45,min=0,max=60,lower.tail=FALSE)
print(d)

a=punif(30,min=0,max=60,lower.tail=FALSE)
b=punif(20,min=0,max=60,lower.tail=FALSE)
print(b-a)

#2
s=dexp(3,rate=1/2,log=FALSE)
print(s)

x=seq(0,5,by=0.1)
px=dexp(x,rate=1/2,log=FALSE)
plot(px)

a=pexp(3,1/2)  #p means cdf,d means pdf
print(a)

y=seq(0,5,by=0.1)
cpx=pexp(y,rate=1/2,log=FALSE)
plot(cpx)

a=rexp(1000,1/2)
plot(a)

#3
E=pgamma(1,shape=2,scale=1/3,lower.tail=FALSE)
print(E)

print(qgamma(0.70,shape=2,scale=1/3))




#assignment6
install.packages('pracma')
library('pracma')
f = function(x,y) 2*(2*x+3*y)/5

I = integral2(f,xmin=0,xmax=1,ymin=0,ymax=1)
print(I$Q)
#returns a list with Q the integral and error the error term
print(I$error)
#use $ COMMAND to extract either Q or error

##function of y only - marginal function of y 
g_x<-function(y) f(1,y);
x2<-integral(g_x,xmin=0,xmax=1);
print(x2)

#function of x only - marginal function of x
h_y<-function(x) f(x,0);
x3<-integral(h_y,xmin=0,xmax=1);
print(x3);

#expectation of xy
f_xy<-function(x,y){
  f(x,y)*x*y;
}
x4 <- integral2(f_xy,xmin=0,xmax=1,ymin=0,ymax=1)
print(x4)

#Q2
x <- c(0,1,2,3)
y <- c(0,1,2)
f <- function(x,y)(x+y)/30
M1 <- matrix(c(f(0,0:2),
               f(1,0:2),
               f(2,0:2),
               f(3,0:2)),nrow=4,ncol=3,byrow=T)
print(M1)
sum(M1)
gx = apply(M1,1,sum) 
print(gx)
#1  mean row sum when x = 0:3 
hy = apply(M1,2,sum)
print(hy)
M1[1,2]
hy[2]
fx = M1[1,2]/hy[2]
print(fx)

Ex = sum(x*gx)
print(Ex)  

Ey = sum(y*hy)  
print(Ey)

Ex2 = sum((x^2) * gx)
print(Ex2)

varX = Ex2 - (Ex)^2 
print(varX)

Ey2 = sum((y^2)*hy)
varY = Ey2 - (Ey)^2
print(varY)

#covariance of XY
#SIGMA (xy) = (E(xy)-E(x)(y))/(sqrt(varX)(sqrt(varY)))
f1 <- function(x,y) x*y*(x+y)/30
M2 = matrix(c(f1(0,0:2),f1(1,0:2),f1(2,0:2),f1(3,0:2)),nrow=4,ncol=3,byrow=T)
M2
Exy = sum(M2)
print(Exy)
cov_xy = Exy - Ex*Ey
print(cov_xy)
r_xy = cov_xy/(sqrt(varX)*sqrt(varY))
print(r_xy)



#assignment7
#1
ans=rt(100,99)
hist(ans)

#2
rchisq(100,2)
rchisq(100,10)
rchisq(100,25)

#3
x=seq(-6,6,length=100)
x
v=c(1,4,10,30)
colour=c("black","orange","green","yellow")
for(i in v){
  print(dt(x,i))
}

plot(x,dt(x,v[4]),type="l",xlab="t-value",ylab="Density",main="Comparison of t-distribution",col=colour[4])
for(i in 1:3){
  lines(x,dt(x,v[i]),type="l",col=colour[i])
}

#4(i)
ans=qf(0.95,10,20)
ans

#4(ii)
x=1.5
df1=10
df2=20
pf(x,df1,df2,lower.tail=TRUE)
pf(x,df1,df2,lower.tail=FALSE)

#4(iii)
q=c(0.25,0.5,0.75,0.999)
df1=10
df2=20
for(i in q){
  print(qf(i,df1,df2))
}

#4(iv)
ans=rf(1000,df1,df2)
ans
hist(ans)



#assignment8
# Assignment 8

# a) importing the dataset
data <- read.csv(file.choose())

# b) validating the import
print(dim(data))
print(data[seq(10),1])

# c) population mean and plotting a histogram
# m <- sum(data$Wall.Thickness)/dim(data)[1]
m <- mean(data$Wall.Thickness)
m

hist(data$Wall.Thickness , xlab = "Wall Thickness" , ylab = "Frequency")

# d) marking the mean in the plot
abline(v=m , col = "Red")

# data is not in bell shape, hence it is not normally distributed

#-------------------------------------------------------------------------------

# a) sample sizes of 10 -> calculating means of each and then plotting the means
drawHistOfMeans = function(samplesize){
  i <- 1
  means <- c()
  while (i<=9000) {
    x <- c(data[seq(i,i+samplesize-1) , 1])
    means <- c(means , mean(x))
    i <- i+samplesize
  }
  meanofmeans <- mean(means)
  meanofmeans
  hist(means)
  abline(v=meanofmeans , col="Red")
}

drawHistOfMeans(10)
# curve is bell shaped but not normally distributed becoz max value does not lie 
# on mean

# b) sample size 50
drawHistOfMeans(50)

# b) sample size 500
drawHistOfMeans(500)

# b) sample size 9000
drawHistOfMeans(9000)
