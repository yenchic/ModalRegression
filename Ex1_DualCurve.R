source("ModalRegression.R")



### Section 1: Generate Data
n=500
dis=0.25

X1 = runif(n)
X2 = runif(n)
X=c(X1,X2)

Y1 =rnorm(n,1.5+0.5*sin(X1*3*pi),sd=dis)
Y2 = rnorm(n,0.5*sin(X2*3*pi),sd=dis)
Y =c(Y1,Y2)

r_XY = sd(Y)/sd(X)
	#scale ratio

Y.s = Y/r_XY
	#scaled Y: just make it easier

h0 = 0.075
	#smoothing parameter (non-optimized)

plot(X,Y.s)



### Section 2: Modal regression
G.x0 = seq(from=min(X), to=max(X), length.out = 100)
G.y0 = seq(from = min(Y.s), to=max(Y.s), length.out=5)
G0 = expand.grid(G.x0,G.y0)
	#creating mesh points: 100*5

y.MS = RegMS1d(X,Y.s,G0[,1],G0[,2],h.x=h0,h.y=h0)
	#modal regression


par(mar=c(4,4,1,1))
plot(X,Y.s,xlab="X",ylab="Y")
points(G0[,1],y.MS, cex=0.7,pch=19,col="dodgerblue")


### Section 3: Representing by curves 
x.mesh = c(G0[1:100,1],G0[401:500,1])
y.MS.mesh = c(y.MS[1:100],y.MS[401:500])
	#note : we can do this because the first 100 and the last 100 mesh points correspond to the two curves

plot(X,Y.s, cex=0.8,xlab="X",ylab="Y")
lines(x.mesh[1:100],y.MS.mesh[1:100], lwd=5, col="blue")
lines(x.mesh[1:100],y.MS.mesh[101:200], lwd=5, col="blue")


### Section 4: Prediction set (non-optimized)

MS_fit = RegMS1d(X,Y.s,X,Y.s,h.x=h0,h.y=h0)
	#predicted Y on the modal curves of each data point

alpha=0.9

MS_ps = quantile(abs(MS_fit-Y.s), alpha)
	# the quantile of loss; this is larger than the Hausdorff distance so that the prediction set has proper coverage



par(mar=c(4,4,1,1))
plot(X,Y.s, cex=0.8,xlab="X",ylab="Y")
lines(x.mesh[1:100],y.MS.mesh[1:100], lwd=4, col="blue")
lines(x.mesh[1:100],y.MS.mesh[101:200], lwd=4, col="blue")
lines(x.mesh[1:100],y.MS.mesh[1:100]+ MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[101:200]+ MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[1:100]- MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[101:200]- MS_ps, lwd=3, col="dodgerblue")
legend("bottomleft",c("Modal Regression", paste(100*alpha,"% PS, Modal", sep="")),lwd=c(5,5), col=c("blue","dodgerblue") )



### Section 5: Comparison to local regression

span_seq = seq(from=0.1, to=0.9, length.out=100)
loc_PS_seq = rep(NA,100)

for(i in 1:100){
	fit_loc_tmp = loess(Y.s~X,span=span_seq[i])
	loc_PS_seq[i] = quantile(fit_loc_tmp$res, alpha)
}
s_opt = span_seq[which(loc_PS_seq==min(loc_PS_seq))]
	#pick the optimal span

fit_loc = loess(Y.s~X,span=s_opt)


# Regression comparison
par(mar=c(4,4,1,1))
plot(X,Y.s, cex=0.8,xlab="X",ylab="Y")
lines(X[order(X)], fit_loc$fitted[order(X)], lwd=5, col="red")
lines(x.mesh[1:100],y.MS.mesh[1:100], lwd=5, col="blue")
lines(x.mesh[1:100],y.MS.mesh[101:200], lwd=5, col="blue")
legend("bottomleft",c("Local Regression","Modal Regression"), lwd=c(5,5), col=c("red","blue"), cex=1)



# Prediction set
par(mar=c(4,4,1,1))
plot(X,Y.s, cex=0.8,xlab="X",ylab="Y")
lines(X[order(X)], fit_loc$fitted[order(X)], lwd=4, col="red")
lines(X[order(X)], fit_loc$fitted[order(X)]+quantile(fit_loc$res,alpha), lwd=3, col="orange")
lines(X[order(X)], fit_loc$fitted[order(X)]-quantile(fit_loc$res,alpha), lwd=3, col="orange")
legend("bottomleft",c("Local Regression",paste(100*alpha,"% PS, Local", sep="")),lwd=c(5,5), col=c("red","orange") )



# Comparison on prediction sets
par(mar=c(4,4,1,1))
plot(X,Y.s, cex=0.8,xlab="X",ylab="Y")
lines(X[order(X)], fit_loc$fitted[order(X)], lwd=4, col="red")
lines(X[order(X)], fit_loc$fitted[order(X)]+quantile(fit_loc$res,alpha), lwd=3, col="orange")
lines(X[order(X)], fit_loc$fitted[order(X)]-quantile(fit_loc$res,alpha), lwd=3, col="orange")
lines(x.mesh[1:100],y.MS.mesh[1:100], lwd=4, col="blue")
lines(x.mesh[1:100],y.MS.mesh[101:200], lwd=4, col="blue")
lines(x.mesh[1:100],y.MS.mesh[1:100]+ MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[101:200]+ MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[1:100]- MS_ps, lwd=3, col="dodgerblue")
lines(x.mesh[1:100],y.MS.mesh[101:200]- MS_ps, lwd=3, col="dodgerblue")
legend("bottomleft",c("Modal Regression", "Local Regression", paste(100*alpha,"% PS, Modal", sep=""),paste(100*alpha,"% PS, Local", sep="")),lwd=c(5,5,5,5), col=c("blue","red","dodgerblue","orange") )
