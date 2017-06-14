#
# Written by:
# -----
# Rhyme Yuan
#
# 2015-06-29
# -----

testpred = read.csv("F:/testing.csv",header=T,col.names=c('height','lenght','area','eccen','p_black','p_and','mean_tr','blackpix','blackand','wb_trans'))

testpredresult = predict( blocks.rf, newdata=testpred, type="class" )
testpredresult

write.table(testpredresult,file="F:/result.csv")
