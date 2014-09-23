Rtesis
======
OBS=matrix(c(1,0,0,0,1,2,0,0,1,0,1,0,
1,0,0,1,0,0,0,0,0,1,0,2,
1,0,0,0,2,1,0,0,0,0,0,1,
3,0,2,0,0,0,1,0,1,3,1,1,
2,1,1,1,1,1,1,1,1,1,1,0,
2,0,0,0,1,0,0,0,0,0,0,0,
2,0,2,1,0,0,0,0,1,1,1,2,
0,0,0,3,0,0,1,0,0,1,0,2,
0,0,0,1,1,0,0,0,0,0,1,0,
1,1,0,2,0,0,1,0,0,1,1,0,
0,1,1,1,2,0,0,2,0,1,1,0,
0,1,1,0,0,0,1,0,0,0,0,0), byrow=T,nrow=g)

P=OBS/n


m=1000
F=0
n=200
J=0

for(a in 1:m){
    S=sample(1:144,n,replace=TRUE)
    X=matrix(1:144,ncol=12)*0
    for(k in 1:n){
        j=((S[k]-1)%%12)+1
        i=((S[k]-j)/12)+1
        X[i,j]=1+X[i,j]
        }
sum(X)
J[a]=sum(X)


y.obs=rowSums(X)
x.obs=colSums(X)
q.obs=0
y.obs
x.obs


for(r in 1:12){
    for(s in 1:12){
        q.obs=q.obs+((X[r,s]-(((x.obs[r]*y.obs[s])/n)^2))                /(x.obs[r]*y.obs[s])/n))
        }
    }
q.obs

    F[a]=q.obs
}
F

plot(ecdf(F))





H=rchisq(m, 121)
lines(ecdf(H))
