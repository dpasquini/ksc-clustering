# K-Spectrum Centroid Clustering

# Compute K-SC Clustering for a set of time series. 
# A: a set of time series. Each row vector A[i,] corresponds to each time series
# K: Number of clusters
# mem represents membership for each time series (mem[i] is the cluster index that time series i belongs to
# cent is a set of cluster centroids. Each row vector cent[i, :] corresponds to each cluster centroids. 
ksc_toy <- function(A, K){
	m = nrow(A)
	mem <- round(sample(1:K,m, replace=T))
	cent <- matrix(0, nrow = K, ncol = ncol(A))
	D <- matrix(0,nrow = m, ncol=K)
	for(iter in 1:100) {
		prev_mem <- mem
		for(k in 1:K) {
			cent[k, ] = ksc_center(mem, A, k, cent[k, ])
		}
		for(i in 1:m) {
			x = matrix(A[i, ], nrow=1)
			for (j in 1:K) {
				y = matrix(cent[j, ], nrow=1)
				dist = dhat_shift(x, y)
				D[i, j] = dist$dist
			}
		}
		val = apply(D, 1, min)
		mem = apply(D, 1, function(x) which(x==min(x)))
		if(norm(prev_mem-mem, type="2") == 0) {
			break
		}
	}
	return(list(mem = mem, cent = cent))
}