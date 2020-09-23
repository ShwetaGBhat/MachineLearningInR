# Monte Carlo Assignment

# There is a 90% and 10% chance of winning full and part projects
# respectively. Full project will have 20-24 modules and part project will 10-12 modules.
# There are about 50-100 tasks per modules and each task will take 5-10 days. The
# resources vary between 3 to 7. What is the most likely time to complete the project?

##Clear Envt.
rm(list=ls(all=TRUE))

timeToCompleteProject=function(simulations){
  for (x in 1:length(simulations)) {
    sim=simulations[x]
    cat("Simulations:",sim,'\n')
    for (i in 1:sim) {
      partOrfull=runif(1,0,1)
      fullOrPartime=''
      if(partOrfull<=0.1){  
        #PartTime project.
        fullOrPartime='PartTime'
        totalModules=sample(10:12,1)
      }else{  #Full projet.
        fullOrPartime='FullTime'
        totalModules=sample(20:24,1)
      }
      #Two vectors created Randomly to represent for tasks & people per moule.
      tsksPerModules=sample(50:100,totalModules,replace = T)
      TotalTasks=sum(tsksPerModules)
      #Compute time for all tasks.Each task can take anything b/w 5 to 10 hrs.
      timeToDoTasks=sum(sample(5:10,TotalTasks,replace = T))
      #Number of recources.
      rec=sum(sample(3:7,1,replace = T))
      #Time needed.
      timeNeeded=timeToDoTasks/rec
      cat(fullOrPartime,timeNeeded,'\n')
    }
  }   
}

simulations=c(10,15,100,100000)
timeToCompleteProject(simulations)
