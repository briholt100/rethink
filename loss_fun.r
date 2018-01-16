#the moral of this work below is that this code

#loss<-sapply(p_grid,function(d) sum(posterior*abs(d-p_grid)))

##creates a matrix of differences of p_grid, multiplies each element in this matrix by corresponding posterior to weight each of those items, then sums them by coloum to create a vector of loss estimates for each value of p_grid.  




p_grid <-seq(0,1,length.out=1000)
prior<- rep(1,1000)
likelihood<-dbinom(6,9,prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
table(posterior)

samples<-sample(p_grid,1e4,prob=posterior,replace=T)
plot(density(samples))
  sum(posterior*abs(0.47-p_grid))


sum(data_frame('diff'=head(posterior*abs(.5-p_grid))))

posterior[which(p_grid>=.640 & p_grid<=.651)]

loss<-lapply(p_grid,function(d) sum(posterior*abs(d-p_grid)))
loss.a<-sapply(loss,sum)
p_grid[which.min(loss.a)]
p_grid[which.min(loss)]
median(samples)
mean(samples)
function(d) sum(posterior*abs(d-p_grid))  # how make this a vector to place in column? 
df.1<-data_frame(p_grid,posterior)

sum(df.1[,3])


library(tidyr)
library(dplyr)
length(samples)

sapply(1:6,function(d) sum(1-6*abs(d-7:12)))







############

x<-seq(0,1, length.out=500)
set.seed(100)
y<- dbinom(6,9,prob=x)
loss.1<-sapply(x, function(d) y*abs(d-x))
loss.1
x[which.min(loss.1)]
