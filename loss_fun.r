#the moral of this work below is that this code

#loss<-sapply(p_grid,function(d) sum(posterior*abs(d-p_grid)))

##creates a matrix of differences of p_grid, multiplies each element in this matrix by corresponding posterior to weight each of those items, then sums them by coloum to create a vector of loss estimates for each value of p_grid.

library(tidyr)
library(dplyr)
library(ggplot2)
library(latticeExtra)
library(gridExtra)

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
n=5
p_grid <-seq(0,1,length.out=n)
prior<- rep(1,n)
likelihood<-dbinom(6,9,prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
unst.posterior<-likelihood*prior

Calculate_loss<-function(d) {
  for (i in 1:length(d)) {
    return((abs(d[i]-p_grid)))   #this creates a matrix of p_grid differences; diagnol == zero
  }
  }

loss<-sapply(p_grid,Calculate_loss) #this creates The matrix
unst.posterior   #for output comp
loss  #for output comp
posterior*loss  #notice that the vector posterior multiplies column by col, not by row, so transpose in head,

#the following code basically plots the absolute deviation of guesses from possible outcomes
wireframe(loss,drape=T,ylab = list('Y axis\nYour guesses\ncolumns 1\n through n\n\n',rot=0),xlab = list('X axis\npossible \ntrue values \n\n',rot=0),main="difference of guess from reality",screen = list(z =-90 , x = -70, y=0))
#but we need to start adding back the components of the function, starting with finding the absolute differences because we are really only interested in magnitude of loss, not the direction (how do you have negative loss?)
loss.1<-apply(loss,2,sum) # collapses grid into vector, the final desired output
#note the above if you change from 2 to 1 (col to row) eval, you flip the loss curve

loss.2<-sapply(p_grid,function(d) sum(posterior*abs(d-p_grid))) #original formula from book

plot(posterior,x=p_grid,type='b',col='blue',main = paste("length.out  = ",length(p_grid)))
lines(loss.2/10,x=p_grid,type='b',col='red')
abline(v=p_grid[which.min(loss.2)],col='red')
graph_text<-paste("low P_grid",round(p_grid[which.min(loss.2)],3),"----->")
#text(y=.0005,x=.75,labels=graph_text)
legend ("topleft",paste("Loss minimized\nat P_grid =",round(p_grid[which.min(loss.2)],3)))




#the moral of the story.  The posterior curve is mirror/flipped, transposed so that each posterior score is fed through the columsn of the P_grid difference matrix.  This amplifies the deviation of guess to correct when big, but if the deviation of guess from correctis small, or zero, the posterior score will have very little, if any effect on the loss.

#using ggplot and original data of n=100

df<-data_frame(p_grid,posterior)
p<-ggplot(df, aes(p_grid,posterior))
P<-p+geom_line()+geom_line(aes(y=loss.2/10),color='red')+geom_rug(aes(x=p_grid,y=loss.2/10))


par(mfrow=c(1,2))
loss.df<-as_data_frame(loss)
colnames(loss.df)<-paste0("diff",seq(1,n))
tidy.loss.df<-loss.df %>% gather(difference,value)
tidy.loss.df<-cbind(p_grid,tidy.loss.df)
#loss.p<-ggplot(tidy.loss.df,aes(x=value))
#loss.p+geom_bar()+facet_wrap(~difference)



#the following wireframe shows a diagonal splitting
#the posterior curve on the right side with
#the loss function on the left side, the highpoint of which is the min

 wf<-wireframe(posterior*loss,
           drape=T,
           main="loss function before \ncolapsed by sum",
           #light.source = c(0,10,10),
           screen = list(z =-105 , x = -70, y=0),
           xlab = list("possible\n probabilities",rot=0),
           ylab = list('Your guesses',rot=0),
           zlab = list("differences between\n guess and actual",rot=90),
           zoom=.9,
           scales = list(arrows = FALSE,
                         x=list(draw=F),
                         y=list(draw=F),
                         z=list(draw=F),
                         #col = "black",
                        # font = 1,
                         #tck = c(0.8, 0.6, 0.4),
                         distance =c(1.2,.8, 1.45)),
           col.regions = colorRampPalette(c("blue", "red"))(100)
           )

 loss.p<-xyplot(loss.2/10~p_grid,col='red')

 lattice.options(
   layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
   layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
 )
 grid.arrange(wf, P, ncol=2, top = "Loss function over\n Posterior Distribution")

 #see countourplot for density
 cloud(posterior*loss,
       panel.3d.cloud=panel.3dbars,
       col.facet='grey',
       xbase=0.4,
       ybase=0.4,
       #      scales=list(arrows=FALSE, col=1),
       screen = list(z = 48, x = -90, y=0),
       xlab = "possible probabilities",
       ylab = 'Your guesses',
       zlab = "differences",
       par.settings = list(axis.line = list(col = "transparent"))
 )




 print(wf,split=c(1,1,1,1),more=T)
 print(loss.p,split=c(1,1,1,1),more=T)
