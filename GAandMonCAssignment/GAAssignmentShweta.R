
#################Genetic Algorithm.
# Genetic Algorithm (TSP)
# Write the code to solve using GA for the following problem :
# You have 10 cities and need to cover all the cities minimizing the distance travelled. 
# The distance between the cities are given as a CSV file.

rm(list=ls(all=TRUE))

# Initital population generation
fnGenerateInitPop = function(dataSet, initPopSize){
  
  initPop = as.data.frame(matrix(nrow = 0, ncol = nrow(costMatrix),
                                 dimnames = list(NULL, names(costMatrix))))
  
  for (i in 1:initPopSize){
    individual = sample(1:10,10,replace=FALSE)
    initPop[i,]= individual
  }
  
  rm(i, individual)
  
  return(initPop)
}

# Define the Objective function as follows.
fnEvaluate = function(individual){
  totCost=0
  for (i in 1:(length(individual)-1)) {
    frmIdx=as.numeric(individual[i])
    toIdx=as.numeric(individual[i+1])
    cat(frmIdx,toIdx,costMatrix[frmIdx,toIdx],'\n')
    totCost=totCost+costMatrix[frmIdx,toIdx]
  }
    return(totCost)
}

# Mutation : Pick one position and change the value to 0 or 1 as the case may be
fnMutate = function(individual){
  
  index = sample(1:length(individual), 2,replace = FALSE)
  print(individual)
  print(index)
  temp=individual[index[1]];
  individual[index[1]]=individual[index[2]];
  individual[index[2]]=temp

  return(individual)
}

# Crossover : Randomly select a point and swap the tails
fnCrossOver = function(parent1, parent2){
  idx = sample(2:(length(parent1)-2), 1)
  for(i in idx:length(parent1))
  {
    if (parent1[i]!=parent2[i]){
      idx=i
      print(as.numeric(parent1[idx])==as.numeric(parent2[idx]))
      break
    }
    print(i)
  }
  print(parent1[idx]);print(parent2[idx])

  child1 = c(parent1[1:idx], parent2[(idx+1):length(parent2)])
  child2 = c(parent2[1:idx], parent1[(idx+1):length(parent1)])
  return(list(child1, child2))
}

# Execute the genetic algorithm
fnRunGeneticAlgo = function(initPop, mutStartProb,
                            elitePercent, maxIterations){
  
  counter = 0   # Is used for stopping criteria
  
  cat("Max iterations = ", maxIterations, "\n")
  
  # How many winners from each generation?
  
  origPopSize = nrow(initPop)
  topElite = round(elitePercent*origPopSize, 0)
  
  initPop$fitN = apply(initPop, 1, fnEvaluate)##Row wise calculating the fitness as 1 is 2nd argument.
  initPop = initPop[order(initPop$fitN, decreasing = F),]
  currentFitN = initPop$fitN[1]##Removing the parent with heighest fitness value.
  initPop$fitN = NULL
  
  newPop = initPop  
  
  for (i in 1:maxIterations){
    
    cat("Iteration : ", i, "\n")
    
    elitePop = newPop[1:topElite,]
    
    newPop = newPop[0, ] # newPop = newPop[-(1:nrow(newPop)), ]
    
    mut = mutStartProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(newPop) < origPopSize) {
      
      # Mutation
      if (runif(1,0,1) < mut){
        idx = sample(1:topElite, 1)#Generate at what position or index we will be flipping.
        newPop[nrow(newPop)+1,] = fnMutate(elitePop[idx,])
        
        if (nrow(newPop) == origPopSize){break()}
      }
      else { #Crossover
        idx1 = sample(1:topElite, 1)
        idx2 = sample(1:topElite, 1)
        
        ls = fnCrossOver(elitePop[idx1,], elitePop[idx2, ])
        
        newPop[nrow(newPop)+1,] = ls[[1]]
        newPop[nrow(newPop)+1,] = ls[[2]]
        
        if (nrow(newPop) == origPopSize){break()}
      }
    }
    
    newPop$fitN = apply(newPop, 1, fnEvaluate)
    newPop = newPop[order(newPop$fitN, decreasing = T),]
    
    prevFitN = currentFitN
    currentFitN = newPop$fitN[1]
    
    newPop$fitN = NULL
    
    # stopping criteria 
    if(prevFitN == currentFitN){
      #If prevFit is same as CurrentFit and iteration is max Iteration break.
      counter = counter+1
      if(counter==5){break()}
    }else{
      counter=0
    }
    
    cat("Total survival points in iteration ", i, " = ", currentFitN, "\n")
  }
  
  return(newPop[1,])
}


fnExecuteMain = function(dataSet, mutStartProb,
                         elitePercent, maxIterations){
  
  set.seed(1234)
  
  initPopSize = 100
  
  initPop = fnGenerateInitPop(dataSet, initPopSize)
  
  solution = fnRunGeneticAlgo(initPop, mutStartProb, 
                              elitePercent, maxIterations)
  
  finalSolution = as.numeric(solution)
  selectedItems = dataSet[finalSolution == 1, ]
  
  # solution vs available
  cat(paste(finalSolution,"\n"))
  
  return(selectedItems)
}

costMatrix = read.csv("/Users/shwetabhat/Desktop/DataScience/Assignments/GAandMonCAssignment/distanceinfo1.csv",header = FALSE)
colnames(costMatrix)=c(paste("C",rep(1:10,1),sep = ''))
rownames(costMatrix)=c(paste("C",rep(1:10,1),sep = ''))
# Stopping criteria If there is no much change for last n iterations you may stop

mutStartProb  = 0.5
elitePercent  = 0.2
maxIterations = 10

result = fnExecuteMain(costMatrix, mutStartProb, elitePercent,
                       maxIterations)
# sumOfCost=matrix(data=0,nrow = dim(costMatrix)[1] )
# for(i in 1:dim(costMatrix)[1]){
#   print(costMatrix[i,]);print(sum(costMatrix[i,]))
#   sumOfCost[i,]=sum(costMatrix[i,])
# }
#rownames(sumOfCost)=rownames(costMatrix)
# generateDataSet=function(simulations){
#   for (x in 1:length(simulations)) {
#     sim=simulations[x]
#     cat("\nSimulations:",sim,'\n')
#     data=data.frame()
#     for (i in 1:sim) {
#       dataSet=sample(1:10,10,replace=FALSE)
#       dataSet=t(dataSet)
#       data=rbind(data,dataSet)
#     }
#     print(data)
#   }
# }
# 
# 
# dataSet=generateDataSet(5)