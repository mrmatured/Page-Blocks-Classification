#
# Written by:
# -----
# Rhyme Yuan
#
# 2015-06-29
# -----

set.seed(38)

y_pred.rf = predict( blocks.rf, newdata=train, type="class" )
blocks1train_row = which(y_pred.rf==1, arr.ind=TRUE)
blocks1test_row = which(y_hat.rf==1, arr.ind=TRUE)

blocks1_train = train[-blocks1train_row,]
blocks1_test = test[-blocks1test_row,]

blocks1.rf = randomForest( class_num ~ ., data=blocks1_train, mtry=p/3, ntree=500, importance=TRUE )
blocks1.rf

# test error rate 
y_hat_1.rf = predict( blocks1.rf, newdata=blocks1_test, type="class" )
ERr1 = sum(y_hat_1.rf!= blocks1_test$class_num)/nrow(blocks1_test)
print( "the test error rate of random forest is:" )
print( ERr1 )