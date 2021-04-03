
library(Rborist)
library(party)
library(randomForest)
library(rpart)
library(rpart.plot)

AllData = read.table("updated_1_2020.csv", header=TRUE, sep = ",")
formula <- default ~ CLTV + DTI + FICO + FRM + NLD + LMP + LoanTerm + Occupancy

ml_results = function(lt) {
  #Prep both test data files
  TestDataE <- AllData[which(AllData$Market==lt),c("loan_id","CLTV", "FICO","DTI","FRM","NLD","LMP","LoanTerm","Occupancy","default")]
  TestData <- AllData[which(AllData$Modeler==1 & AllData$Market==lt & !is.na(AllData$default)),]
  
  #Run Random Forest Model
  print("Start RBorist")
  print(Sys.time())
  RB_All <- Rborist(TestData[,c("CLTV", "FICO","DTI","FRM","NLD","LMP","LoanTerm","Occupancy")], 
                    TestData[,c("default")], nTree = 500, predFixed = 3, minNode = 200)
  
  #Run CART Model
  print("Start CART")
  print(Sys.time())
  CART_final <- rpart(formula, data = TestData, 
                      cp = 0, xval = 20, minbucket = 500)
  CPrel = CART_final$cptable[which.min(CART_final$cptable[,"xerror"]),"xerror"] + CART_final$cptable[which.min(CART_final$cptable[,"xerror"]),"xstd"]
  cp_new = CART_final$cptable[which(CART_final$cptable[,"xerror"] <= CPrel),"CP"][1]
  pCART_final1 <- prune(CART_final, cp= cp_new)
  
  #Predict and Return Results
  print("Start Prediction")
  print(Sys.time())
  pred <- predict(RB_All, TestDataE[,c("CLTV", "FICO","DTI","FRM","NLD","LMP","LoanTerm","Occupancy")])
  TestDataE$RF <- pred$yPred
  TestDataE$CART1 <- predict(pCART_final1,TestDataE)
  TestDataE <- TestDataE[,c("loan_id","RF","CART1")]
  return(TestDataE)
  print("End")
  
  rm(TestData, RB_All, CART_final, CPrel, cp_new, pCART_final1, pred)
  gc()
} 

TestDataE2 <-ml_results("P")
TestDataE2 <-rbind(TestDataE2,ml_results("R"))
TestDataE2 <-rbind(TestDataE2,ml_results("C"))

write.csv(TestDataE2, file=paste0("Results_Final2020.csv"))
rm(AllData, TestDataE2, formula)
