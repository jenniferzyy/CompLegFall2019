k = DTM[c(25,50,75),]
k1 = DTM[1:31,]
k2 = DTM[32:63,]
k3 = DTM[64:94,]
k = c(k1,k2,k3)

cen1 = colMeans(k1)
cen2 = colMeans(k2)
cen3 = colMeans(k3)

dis1 = (k1-cen1)^2
dis2 = (k2-cen2)^2
dis3 = (k3-cen3)^2


for (i in 1:nrow(DTM)) {
  r = DTM[i,]
  d1 = (r - cen1)^2
  d2 = (r - cen2)^2
  d3 = (r - cen3)^2
  d = min(d1,d2,d3)
  if(d == d1) {
    k1 = rbind(k1,r)
    cen1 = colMeans(k1)
  }
}

## jeff code
# sample centers
centers <- x[sample(nrow(x),k)]
# iterate for set number of times
for (i in 1:n_iter) {
  # calculate distance of each word from center
  distances_to_centers <- eudlidean_destance(x,centers)
  # assign to closest cluster
  clusters <- apply(distances_to_centers, 1, which.min)
  # reupdate where the center is
  centers <- apply(x,2,tapply, clusters, mean)
  # save each cluster and center for enxt
  clusterHistory[[i]] = clusters
  centerHistory[[i]] = centers
}
list(clusters = clusterHistory, centers = centerHistory)