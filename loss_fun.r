#the moral of this work below is that this code

#loss<-sapply(p_grid,function(d) sum(posterior*abs(d-p_grid)))

##creates a matrix of differences of p_grid, multiplies each element in this matrix by corresponding posterior to weight each of those items, then sums them by coloum to create a vector of loss estimates for each value of p_grid.  

library(tidyr)
library(dplyr)

p_grid <-seq(0,1,length.out=1000)
prior<- rep(1,1000)
likelihood<-dbinom(6,9,prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

samples<-sample(p_grid,1e4,prob=posterior,replace=T)
plot(posterior)

loss<-sapply(p_grid,function(d) sum(posterior*abs(d-p_grid)))
lines(loss/1000,col='red')
p_grid[which.min(loss)]
median(samples)
mean(samples)

length(samples)


############

p_grid <-seq(0,1,length.out=5)
prior<- rep(1,5)
likelihood<-dbinom(6,9,prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)

Calculate_loss<-function(d) {
  for (i in 1:length(d)) {
    return((abs(d[i]-p_grid)))   #this creates a matrix of p_grid differences; diagnol == zero
  } 
  }

loss<-sapply(p_grid,Calculate_loss) #this creates The matrix
posterior   #for output comp
loss  #for output comp
posterior*loss  #notice that the vector posterior multiplies column by col, not by row, so transpose in head,
loss.1<-apply(loss*posterior,1,sum) # collapses grid into vector, the final desired output
#note the above if you change from 2 to 1 (col to row) eval, you flip the loss curve


plot(posterior,x=p_grid,type='b',col='blue',ylim=c(-.1,1.4))
lines(loss.1,x=p_grid,type='b',col='red')
text(y=loss.1+.1,x=p_grid,labels=round(loss.1,3))


#the moral of the story.  The posterior curve is mirror/flipped, transposed so that each posterior score is fed through the columsn of the P_grid difference matrix.  This amplifies the deviation of guess to correct when big, but if the deviation of guess from correctis small, or zero, the posterior score will have very little, if any effect on the loss.  