d<-function(n,s,x){
    res=(n-1)/(n+s-1)
    a=s+1
    b=n+s
    if(x>0){
        for(i in 1:x){
            res=res*a/b
            a=a+1
            b=b+1
            }
        
        }
res}
x=y=seq(1,100)
for(i in 1:100) {y[i]=
d(349,1820,i-1)}

plot(x,y)