library(rethinking)
library(tidyr)
library(dplyr)
df.full<-data.frame(c(1,0,1,1,1,0,1,0,1))
nrow(df)
names(df.full)<-"water"
n=9
df<-df.full %>% slice(1:n)
m1.1 <- map(
  alist(
    water ~ dbinom(1, p) ,
    p ~ dunif( 0 , 1 )
  ) ,
  data=df) 
precis(m1.1)
post <- extract.samples( m1.1 , n=20 )

plot(density(df$water), post,
      #xlim=range(df$water) , 
       xlab="water" , ylab="density" )
mtext(concat("N = ",n))

# plot the lines, with transparency
for ( i in 1:20 )
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )

