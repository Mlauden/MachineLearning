library(caret)   #Machine learning package
library(rattle) #Fancy tree diagram
library(GGalley)
#(rpart)
#library(rpart.plot)

#####################################################################################
######Project Goal######
#The goal of your project is to predict the manner in which they did the exercise. 
#This is the "classe" variable in the training set. 
#You may use any of the other variables to predict with. 
#You should create a report describing how you built your model, 
#how you used cross validation, what you think the expected out of sample error is, 
#and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases.
#####################################################################################



if(exists("Training")==FALSE){ 
  Training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
  Test<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
}
  
Training[Training==""] <- NA
trainingData<-Training[,8:160]
trainingData<-trainingData[,is.na(trainingData[1,])==FALSE] #Remove columns that have no data

#Break Data down into training/test sets (70/30%)
testIndex = createDataPartition(trainingData$classe, p = 0.70,list=FALSE)
trainingSet = trainingData[testIndex,]
testingSet = trainingData[-testIndex,]

modFit<-train(classe~.,method="rpart",data=trainingSet, na.action = na.omit) #"train" is predictive model from caret program
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
fancyRpartPlot(modFit$finalModel)
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

print(modFit$finalModel)

pred<- predict(modFit$finalModel, newdata=Test)
VariableImp<-varImp(modFit)
#Ranked<-VariableImp[order(-VariableImp$Overall),,drop=FALSE]
#Ranked[1:9,,drop=FALSE]

#Plot relationship between terms
ggplot(trainingSet,aes(x=pitch_forearm,y=roll_forearm,color=classe))+geom_jitter()

#determine most important variables and plot graphical pairs
Imp<-rownames(VariableImp$importance[1:10,,drop=FALSE])
ImpTrainSet<-trainSet[,Imp]
#ggpairs(ImpTrainSet)



classepredict=predict(modFit,testingSet,data = na.omit(data))
#confusionMatrix(testingSet$Classe,classepredict) #Does not work because "D" is never predicted workaround below
table(factor(classepredict, levels=min(testingSet$classe):max(testingSet$classe)), factor(testingSet$classe, levels=min(testingSet$classe):max(testingSet$classe)))
u = union(classepredict, testingSet$classe)
t = table(factor(classepredict, u), factor(testingSet$classe, u))
confusionMatrix(t)

