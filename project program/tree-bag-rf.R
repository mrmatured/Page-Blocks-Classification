#
# Written by:
# -----
# Rhyme Yuan
#
# 2015-06-29
# -----
# rm(list=ls())

# Part (1)
# -----
# using decision-tree
# 

library(tree)
set.seed(38)

# data reading
n = nrow(blocks)
p = ncol(blocks)-1
train_row = sample(1:n, 2*n/3)
test_row = (1:n)[-train_row]
train = blocks[train_row,]
test = blocks[test_row,]
train$class_num = as.factor(train$class_num)
test$class_num = as.factor(test$class_num)

# classification
rtree.blocks = tree( class_num ~ ., data=train )

# result output
summary(rtree.blocks)
plot(rtree.blocks)
text(rtree.blocks,pretty=0)
print( rtree.blocks )

# test error rate 
y_hat.test = predict( rtree.blocks, newdata=test, type="class" )
ERt = sum(y_hat.test!= test$class_num)/nrow(test)
print( "the test error rate of decision-tree is:" )
print( ERt )

# Use cross-validation to determine the optimal of tree complexity:

cv.blocks = cv.tree( rtree.blocks )
names( cv.blocks )

print( cv.blocks )

plot( cv.blocks$size, cv.blocks$dev, type="b") # plot the tree size

# Pick the size of the tree you want to prune to:
# It looks like k=10 is the smallest tree with an error close to the minimum.

prune.blocks = prune.tree( rtree.blocks, best=10 ) 

summary(prune.blocks)
plot(prune.blocks)
text(prune.blocks,pretty=0)
print(prune.blocks)

# test error rate 
y_hat.prune = predict( prune.blocks, newdata=test, type="class" )
ERp = sum(y_hat.prune!= test$class_num)/nrow(test)
print( "the test error rate of pruned decision-tree is:" )
print( ERp )

# Part (2)
# -----
#
# Use bagging
# 
library(randomForest)
blocks.bag = randomForest( class_num ~ ., data=train, mtry=p, ntree=500, importance=TRUE )
blocks.bag

# test error rate 
y_hat.bag = predict( blocks.bag, newdata=test, type="class" )
ERb = sum(y_hat.bag!= test$class_num)/nrow(test)
print( "the test error rate of bagging is:" )
print( ERb )

plot(blocks.bag)

ibag = importance( blocks.bag )
print( ibag[ order( ibag[,1] ), ] )

# Part (3)
# -----
#
# Use random forests
#
blocks.rf = randomForest( class_num ~ ., data=train, mtry=p/3, ntree=500, importance=TRUE )
blocks.rf

# test error rate 
y_hat.rf = predict( blocks.rf, newdata=test, type="class" )
ERr = sum(y_hat.rf!= test$class_num)/nrow(test)
print( "the test error rate of random forest is:" )
print( ERr )

plot(blocks.rf)

irf = importance( blocks.rf )
print( irf[ order( irf[,1] ), ] )

varImpPlot(blocks.rf)
