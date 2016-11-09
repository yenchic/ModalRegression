source("ModalRegression.R")

### Section 1: Generate Data
n1=100
n2=100
n3=100

sd_y = 0.2

x1 = runif(n1,0,0.4)
x2 = runif(n2,0.3, 0.7)
x3 = runif(n3,0.6,1.0)


y1 = rnorm(n1,3,sd=sd_y)
y2 = rnorm(n2,2,sd=sd_y)
y3 = rnorm(n3,1,sd=sd_y)


X = c(x1,x2,x3)
Y = c(y1,y2,y3)

plot(X,Y)


### Section 2: Modal Regression
h_x =0.075	#arbitrary choice
h_y = h_x*sd(Y)/sd(X)

RM_Y = RegMS1d(X,Y,G.x = X, G.y = Y,h.x=h_x, h.y=h_y)

plot(X,Y, cex=0.4)
points(x=X, y=RM_Y, col="blue", pch=19, cex=0.6)



### Section 3: Bandwidth selection via minizming prediction set
alpha=0.95

x_n = 50	
y_n = 20

x_gr =seq(from=0,to=1, length.out=x_n)
y_gr = seq(from=0.5, to=3.5, length.out=y_n)
	# mesh points

k_RS = rep(NA, x_n)
	# number of clusters

h_seq = seq(from=0.02, to=0.20, length.out = 100)
	# test a series of h: from 0.02 to 0.20
	
RM_list = list()
PI_seq = rep(NA, length(h_seq))

for(i_tmp in 1:length(h_seq)){
	h_x =h_seq[i_tmp]
	h_y = h_x*sd(Y)/sd(X)

	RM_Y = RegMS1d(X,Y,G.x = X, G.y = Y,h.x=h_x, h.y=h_y)
	RM_list[[i_tmp]] = RM_Y
	

	for(i in 1:x_n){
		pt_tmp = cbind(rep(x_gr[i],y_n), y_gr)
			# initialize points at each x grid
		
		rm_y_tmp = RegMS1d(X,Y, G.x= rep(x_gr[i],y_n), G.y=y_gr, h.x= h_x, h.y=h_y)	
			# fininding conditional local modes
	
		y_tmp = round(rm_y_tmp, digit=5)
		k_RS[i] = length(unique(y_tmp))
			# number of local mode at the i-th mesh point
	}

	q_RS = quantile(abs(RM_Y-Y), alpha)
		# width of prediction set
		
	PI_seq[i_tmp] = mean(k_RS)*q_RS
		# area of prediction set
}

plot(x=h_seq, y= PI_seq, lwd=3, type="l", ylab="Size of prediction set", xlab="h", main=paste("Size of ",100*alpha,"% Prediction interval", sep=""))



plot(x=h_seq, y= PI_seq, lwd=3, type="l", ylab="Size of prediction set", xlab="h", main=paste("Size of ",100*alpha,"% Prediction interval", sep=""))
abline(v= h_seq[which(PI_seq==min(PI_seq))], col="limegreen", lwd=2)



#### Section 4: Optimal result

h_x =h_seq[which(PI_seq==min(PI_seq))]
h_y = h_x*sd(Y)/sd(X)

Grids = as.matrix(expand.grid(seq(from=0,to=1, length.out=100), c(1,2,3)))


RM_fine_opt = RegMS1d(X,Y,G.x = Grids[,1], G.y= Grids[,2],h_x,h_y)

plot(X,Y, cex=0.4 , main=paste("h=",round(h_x, digits=2), ", the minimal prediction set"))
points(x= Grids[,1], y=RM_fine_opt, col="blue", pch=19, cex=0.6)


###
h_tmp = hclust(dist(cbind(Grids[,1],RM_fine_opt)))
	# hierachical clustering over the modal regression points
lab_tmp = cutree(h_tmp,h=1)
	# clustering

clusters = list()
for( i in 1:max(lab_tmp)){
	w_tmp = which(lab_tmp==i)
	clusters[[i]] = cbind(Grids[w_tmp,1], RM_fine_opt[w_tmp])
}
	# each list element is the modal points for that cluster


plot(X,Y, cex=0.4 , main=paste("h=",round(h_x, digits=2), ", the minimal prediction set"))
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
}




#### Section 5: Prediction set
alpha=0.9
q_RS_opt = quantile(abs(RM_list[[which(PI_seq==min(PI_seq))]]-Y), alpha)


plot(X,Y, cex=0.4 , main=paste("h=",round(h_x, digits=2), ", the minimal prediction set"))
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]+q_RS_opt, col="dodgerblue", lwd=3)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]-q_RS_opt, col="dodgerblue", lwd=3)
	}
legend("topright",c("Modal Regression", paste(alpha,"% PI", sep="")),lwd=c(5,5), col=c("blue","dodgerblue") )




#### Section 6: Local regression

span_seq = seq(from=0.1, to=0.9, length.out=100)
loc_PS_seq = rep(NA,100)

for(i in 1:100){
	fit_loc_tmp = loess(Y~X,span=span_seq[i])
	loc_PS_seq[i] = quantile(fit_loc_tmp$res, alpha)
}
s_opt = span_seq[which(loc_PS_seq==min(loc_PS_seq))]
	#the optimal one

loess_fit  = loess(Y~X,span=s_opt)




plot(x=h_seq, y= PI_seq, lwd=3, type="l", ylab=paste("Size of ",100*alpha,"% Prediction Sets", sep=""), xlab="h", main="")
abline(v= h_seq[which(PI_seq==min(PI_seq))], col="limegreen", lwd=2)
abline(h= quantile(abs(loess_fit$res),alpha), col="red", lwd=3)
legend("topright", c("Modal Regression","Local Regression", "Optimal h"), col=c("black","red","limegreen"), lwd=c(3,3,3))


	# comparison
plot(X,Y, cex=0.4 , main="Local Regression VS Modal Regression")
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)], col="red", lwd=4)
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
}
legend("topright",c("Modal Regression", "Local Regression"),lwd=c(5,5), col=c("blue","red") )


	# prediction set
plot(X,Y, cex=0.4 , main="Local Regression")
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)], col="red", lwd=4)
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)]+quantile(abs(loess_fit$res),alpha), col="orange", lwd=3)
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)]-quantile(abs(loess_fit$res),alpha), col="orange", lwd=3)
legend("topright",c("Local Regression", paste(alpha,"% PI", sep="")),lwd=c(5,5), col=c("red","orange") )


	# comparison of prediction set
plot(X,Y, cex=0.4 , main=paste("Comparison of ", 100*alpha, "% PS", sep=""))
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)], col="red", lwd=4)
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)]+quantile(abs(loess_fit$res),alpha), col="orange", lwd=3)
lines(loess_fit$x[order(loess_fit$x)],loess_fit$fitted[order(loess_fit$x)]-quantile(abs(loess_fit$res),alpha), col="orange", lwd=3)
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]+q_RS_opt, col="dodgerblue", lwd=3)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]-q_RS_opt, col="dodgerblue", lwd=3)
		}
legend("topright",c("Modal Regression", "Local Regression", paste(100*alpha,"% PS, Modal", sep=""),paste(100*alpha,"% PS, Local", sep="")),lwd=c(5,5,5,5), col=c("blue","red","dodgerblue","orange") )





##### Section 7: Mixture regression
library(mixtools)

for(i in 1:10000){
	mix_tmp = regmixEM.lambda(Y,X, k=3)
	if(mix1$loglik>mix_tmp$loglik)
	{
	mix1 = mix_tmp	
	}
}	#find the optimalEM over several runs


	# comparison
plot(X,Y, cex=0.8)
for(i_k in 1:3){
	abline(a=mix1$beta[1,i_k], b=mix1$beta[2,i_k], lwd=4, col="red")
}
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
}


	# comparison of prediction sets
plot(X,Y, cex=0.8)
for(i_k in 1:3){
	abline(a=mix1$beta[1,i_k], b=mix1$beta[2,i_k], lwd=4, col="red")
	abline(a=mix1$beta[1,i_k]+1.96*mix1$sigma[i_k], b=mix1$beta[2,i_k], lwd=3, col="orange")
	abline(a=mix1$beta[1,i_k]-1.96*mix1$sigma[i_k], b=mix1$beta[2,i_k], lwd=3, col="orange")
}
for(i in 1:max(lab_tmp)){
	lines(clusters[[i]][order(clusters[[i]][,1]),], col="blue", lwd=4)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]+q_RS_opt, col="dodgerblue", lwd=3)
	lines(x=clusters[[i]][order(clusters[[i]][,1]),][,1], y=clusters[[i]][order(clusters[[i]][,1]),][,2]-q_RS_opt, col="dodgerblue", lwd=3)
		}

legend("topright",c("Modal Regression", "Mixture Regression", paste(100*alpha,"% PS, Modal", sep=""),paste(100*alpha,"% PS, Mixture", sep="")),lwd=c(5,5,5,5), col=c("blue","red","dodgerblue","orange") ,bg="white")

