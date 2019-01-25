library(rethinking)
df<-data.frame(c(1,0,1,0,1,1,0,1,0))
nrow(df)
names(df)<-"water"
m1.1 <- map(
  alist(
    water ~ dbinom(2, p) ,
    p ~ dunif( 0 , 1 )
  ) ,
  data=list(water=1) )
precis(m1.1)
post <- extract.samples( m1.1 , n=20 )

plot(density( df$water)) , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))

# plot the lines, with transparency
for ( i in 1:20 )
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )

