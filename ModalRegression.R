## 1d modal regression
RegMS1d = function(X, Y, G.x=X, G.y=Y, h.x, h.y, iter=100, tolerance=1e-8){
	x0 = G.x
	y0 = G.y
	#initialize points
	
	m=length(y0)
	#size of grid
	
	i0=0
	tol0=1
	while(i0<iter && tol0> tolerance){
		y0.tmp = y0
		y0 = sapply(1:m, function(x){
				y0.tmp2 = sum(Y*dnorm(y0[x]-Y, sd=h.y)*dnorm(x0[x]-X,sd=h.x))/sum(dnorm(y0[x]-Y, sd=h.y)*dnorm(x0[x]-X,sd=h.x))
				return(y0.tmp2)
			}
		)
		i0=i0+1
		tol0 = max(abs(y0.tmp-y0))
	}	# Partial mean shift
	
	return(y0)
}

### 2d modal regression
RegMS2d = function(X,Y, G.x=X, G.y=Y, h.x, h.y, iter=100,tolerance=1e-8){
	x0 = G.x
	y0 = G.y
	#initialize points
	
	m=length(y0)
	#size of grid

	i0=0
	tol0=1
	while(i0<iter && tol0> tolerance){
		y0.tmp = y0
		y0 = sapply(1:m, function(x){
				y0.tmp2 = sum(Y*dnorm(y0[x]-Y, sd=h.y)*dnorm(x0[x,1]-X[,1],sd=h.x)*dnorm(x0[x,2]-X[,2],sd=h.x))/sum(dnorm(y0[x]-Y, sd=h.y)*dnorm(x0[x,1]-X[,1],sd=h.x)*dnorm(x0[x,2]-X[,2],sd=h.x))
				return(y0.tmp2)
			}
		)
		i0=i0+1
		tol0 = max(abs(y0.tmp-y0))
	}  # Partial mean shift
	return(y0)
}
