dhat_shift <- function(x, y) {
	min_d = scale_d(x, y)
	L = length(y)
	range = -5:5
	for(shift in range) {
	  print("shift is:")
	  print(shift)
		if(shift<0) {
			if(-shift+1 > ncol(y)) {
				yshift = matrix(0, nrow = 1, ncol=ncol(y))
			} else {
				yshift <- matrix(nrow = 1, ncol = ncol(y))
				len <- length(y[,(-shift+1):length(y)])
				yshift[,1:len] <- y[,(-shift+1):length(y)]
				yshift[,(len+1):length(yshift)] <- 0
			}
		} else {
		 	yshift <- matrix(nrow = 1, ncol = ncol(y))
		 	if(shift < ncol(y)) {
		 		yshift[, (shift+1):length(yshift)]<-y[,1:(length(y)-shift)]
		 		if(shift!=0) {
		 			yshift[, 1:shift] <- 0
		 		}
		 	} else {
		 		yshift[, 1:ncol(yshift)] <- 0
		 	}
		}
		cur_d = scale_d(x, yshift)
		if(cur_d <= min_d) {
			optshift = shift
			opty = yshift
			min_d = cur_d
		}
		print("min_d is")
		print(min_d)
	}
	dist = min_d
	return(list(dist=dist, optshift=optshift, opty=opty))
}

scale_d <- function(x, y) {
	alpha = sum(x*y)/sum(y*y)
	if(!is.nan(alpha)) {
	  dist = norm(x - alpha * y, type="2") / norm(x, type="2")
	} else {
	  return(Inf)
	}
	return(dist)
}