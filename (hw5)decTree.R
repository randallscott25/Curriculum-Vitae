

library(caret)
library(rpart)
library(rpart.plot)

## import dataset
df = read.csv('fedPapers85.csv')

## preprocess data: remove author + filename columns
df = df[, -c(2)]

## training keys
train_keys = which(df$author != 'dispt')
test_keys = which(df$author == 'dispt')

## train + test set
train = df[train_keys, ]
test = df[test_keys, ]

##
## default tree
##
fit.default = rpart(
  author ~ .,
  data = train,
  method = 'class'
)

## visualize default tree
png('hw5/visualization/default_tree.png')
rpart.plot(fit.default)
dev.off()

## default tree summary
sink('hw5/visualization/default_tree_analysis.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.default)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.default.pred = table(predict(fit.default, type='class'), train$author)
1-sum(diag(fit.default.pred))/sum(fit.default.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
predict(fit.default, test, type = 'prob')
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
predict(fit.default, test, type = 'class')
sink()

##
## tuned tree
##
## @maxdepth, john jay has 5 articles, indicated by the default tree
##     on the second level. Using one level elminates the 'john jay'
##     leaf-node branch.
##
fit.tuned = rpart(
  author ~ .,
  data = train,
  method = 'class',
  control = list(maxdepth = 1)
)

## visualize default tree
png('hw5/visualization/tuned_tree.png')
rpart.plot(fit.tuned)
dev.off()

## tuned tree summary
sink('hw5/visualization/tuned_tree_analysis.txt')
cat('===========================================================\n')
cat(' Note: the "root node error" is the error rate for a single\n')
cat(' node tree, if the tree was pruned to node 1. It is useful\n')
cat(' when comparing different decision tree models. measures of\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
printcp(fit.tuned)
cat('\n\n')
cat('===========================================================\n')
cat(' resubstitution error rate, computed on training sample\n')
cat(' predictive performance. \n')
cat('===========================================================\n')
fit.tuned.pred = table(predict(fit.tuned, type='class'), train$author)
1-sum(diag(fit.tuned.pred))/sum(fit.tuned.pred)
cat('\n\n')
cat('===========================================================\n')
cat(' cross validation performance \n')
cat('===========================================================\n')
xpred.rpart(fit.tuned, xval=10)
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (probability) \n')
cat('===========================================================\n')
predict(fit.tuned, test, type = 'prob')
cat('\n\n')
cat('===========================================================\n')
cat(' test prediction (class) \n')
cat('===========================================================\n')
predict(fit.tuned, test, type = 'class')
sink()