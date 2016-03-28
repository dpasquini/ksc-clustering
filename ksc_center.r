ksc_center <- function(mem, A, k, cur_center) {
	a <- cbind()
	for(i in 1:length(mem)){
		if(mem[i] == k) {
			if(sum(cur_center)==0) {
				opt_a = matrix(A[i,], nrow=1)
			} else {
				list<-dhat_shift(cur_center, A[i,]);
				tmp<-list$dist
				tmps<-list$optshift
				opt_a <- list$opty
			}
			a <- rbind(a, opt_a)
		}
	}
	if(nrow(a) == 0) {
		ksc = matrix(0, nrow=1, ncol=ncol(A))
		return(ksc)
	}
	b <- a/matrix(rep(sqrt(rowSums(a^2)), times=ncol(a)), nrow=nrow(a))
	M <- t(b)%*%b -  nrow(a) * diag(ncol(a))
	eig <- eigen(M)
	V <- eig$vectors
	D <- eig$values * diag(ncol(M))
	ksc <- matrix(V[,ncol(V)], nrow=1)
	ksc
	if(sum(ksc) < 0) {
		ksc = -ksc
	}
	return(ksc)
}
