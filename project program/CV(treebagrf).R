#
# Written by:
# -----
# Rhyme Yuan
#
# 2015-07-04
# -----

set.seed(38)

# 
# cross-validation labels
# 
Fold = function(Z=10, v, D)
  mm = Fold(10, v, 11)
CV1 = function(n, Z){
  z = rep(1:Z, ceiling(n/Z))[1:n]
  z=sample(z,n)
  mm = list(); for(i in 1:Z) mm[[i]]=(1:n)[z==i]
return(mm)}
Z = 10; mm = CV1(nrow(blocks),Z)
n=nrow(blocks); k=10; mm=CV1(n,k)

#
# decision-tree CV
#
E0d= rep(0,10); E1d = E0d
for(i in 1:10){
  m = mm[[i]]
  n0 = n-length(m);n1 = length(m)
  a = tree( as.factor(class_num) ~ ., data=blocks[-m,] )
  E0d[i]=sum(blocks[-m,11]!=predict(a,blocks[-m,],type="class"))/n0
  E1d[i]=sum(blocks[m,11]!=predict(a,blocks[m,],type="class"))/n1
  ME0d = mean(E0d);ME1d=mean(E1d)
}
ME1d

#
# bagging CV
#

E0b= rep(0,10); E1b = E0b
for(i in 1:10){
  m = mm[[i]]
  B = randomForest( as.factor(class_num) ~ ., data=blocks[-m,], mtry=p, ntree=500, importance=TRUE )
  n0 = n-length(m);n1 = length(m)
  E0b[i]=sum(blocks[-m,11]!=predict(B,blocks[-m,],type="class"))/n0
  E1b[i]=sum(blocks[m,11]!=predict(B,blocks[m,],type="class"))/n1
  ME0b = mean(E0b);ME1b=mean(E1b)
}
ME1b

#
# random forests CV
#
E0r= rep(0,10); E1r = E0r
for(i in 1:10){
  m = mm[[i]]
  rf = randomForest( as.factor(class_num) ~ ., data=blocks[-m,], mtry=p/3, ntree=500, importance=TRUE )
  n0 = n-length(m);n1 = length(m)
  E0r[i]=sum(blocks[-m,11]!=predict(rf,blocks[-m,],type="class"))/n0
  E1r[i]=sum(blocks[m,11]!=predict(rf,blocks[m,],type="class"))/n1
  ME0r = mean(E0r);ME1r=mean(E1r)
}
ME1r

