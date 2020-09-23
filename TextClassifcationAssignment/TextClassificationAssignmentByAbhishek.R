rm(list = ls(all = TRUE))

library(tm)
library(DMwR)

medicine = Corpus(x = DirSource("/Users/shwetabhat/Desktop/DataScience/TextClassifcationAssignment/sci.med"), readerControl = list(language = "en_US"))
space = Corpus(x = DirSource("/Users/shwetabhat/Desktop/DataScience/TextClassifcationAssignment/sci.space"), readerControl = list(language = "en_US"))
#Splitting test and train
numberOfDocMedicine = length(medicine)
splitRangeMedicine = as.factor(c(rep("Test", ceiling(numberOfDocMedicine/7)), rep("Train", numberOfDocMedicine-ceiling(numberOfDocMedicine/7))))
split_medicine = split(medicine, splitRangeMedicine)
train_medicine = split_medicine$Train
test_medicine = split_medicine$Test

numberofDocSpace = length(space)
splitRatioSpace = length(space)
splitRangeSpace = as.factor(c(rep("Test", ceiling(numberofDocSpace/7)), rep("Train", numberofDocSpace-ceiling(numberofDocSpace/7))))
split_space = split(space, splitRangeSpace)
train_space = split_space$Train
test_space = split_space$Test

#Creating corpus
corpusAll = c(train_medicine, train_space, test_medicine, test_space)
corpusAll = Corpus(VectorSource(unlist(corpusAll)))

#Removing unwanted data 
corpusAll = tm_map(corpusAll,removePunctuation)
corpusAll = tm_map(corpusAll,removeNumbers)
corpusAll = tm_map(corpusAll,tolower)
corpusAll = tm_map(corpusAll, removeWords, stopwords("english"))
corpusAll = tm_map(corpusAll, stripWhitespace)

#tfidf
tfidf_matrix = DocumentTermMatrix(corpusAll, control = list(weighting = weightTfIdf, minWordLength = 5, minDocFrequency = 5))
tfidf_matrix
inspect(tfidf_matrix[1:10,1:10])

#Sparsity
tfidf_matrix = removeSparseTerms(tfidf_matrix, 0.8)
tfidf_matrix

#svd
svd = svd(x = as.matrix(tfidf_matrix))
matrix = svd$u

data = as.data.frame(matrix)
data = data[apply(X = data, MARGIN = 1, FUN = function(x) !all(x == 0)),]

#Y values to data
targetVariable = as.factor(c(rep("medicine",857), rep("space",857),rep("medicine",143), rep("space",143)))
data = cbind(targetVariable, data)

#Train and test from corpus
train_rows = sample(nrow(data), ceiling(1714))
test_rows = (1:nrow(data))[- train_rows]
train_set = data[train_rows, ]
test_set = data[test_rows,]

#Knn prediction
pred = kNN(targetVariable~.,train_set,test_set)

#Accuracy
confusionMatrix = table("Predictions" = pred, Actual = test_set[,"targetVariable"])
confusionMatrix
accuracy = (sum(diag(confusionMatrix))/sum(confusionMatrix)* 100)
accuracy
