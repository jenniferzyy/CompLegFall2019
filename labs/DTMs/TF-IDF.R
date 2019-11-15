tf <- as.matrix(t(DTM))
IDF_j <- log(ncol(tf)/(rowSums(tf)))
idf_j <- diag(IDF_j)
tf_idf <- crossprod(tf, idf_j)
colnames(tf_idf) <- rownames(tf)
final <- tf_idf/sqrt(rowSums(tf_idf^2))

test <- dfm_tfidf(as.dfm(tf), scheme_tf = "count", scheme_df = "inverse", base=10)

freq <- colSums(DTM)
IDF = log(94/freq)
for (i in names(freq)) {
  count = sum(DTM[,i]!=0)
  IDF[i]=log(94/count)
}
idf <- diag(IDF)
dis <- freq %*% idf

