#Simple r parallel example
if(!require(foreach)){
  install.packages("foreach")
  library(foreach)
}

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParallel)
}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}



#define a worker method that will work on your subset of data
#This is a very silly example but it's showing the concept
#rather than doing anything useful !
worker <- function(dataTable,waitingTime){
  
  
  #Its important to have a try catch 
  #otherwise you will not see any errors 
  #that the method produces
  #Any errors will be printed to the debug file
  result = tryCatch({
    
    eruption.lm = lm(eruptions ~ waiting, data=dataTable)
    
    #Adding the new columns based on the results
    dataTable[,waiting := waitingTime]
    dataTable[,pedictedTime := predict(eruption.lm, waiting)]
    
    
    return(dataTable)
    
  }, warning = function(w) {
    print(paste0('Warning detected ',as.character(w)))
  }, error = function(e) {
    print(paste0('Error detected ',as.character(e)))
  }, finally = {
    #This branch will alaways run even if no error detected
  })
  
  
  
  
}


faithDt <- data.table(faithful,stringsAsFactors = FALSE)
splitlimit  <- nrow(faithDt) %/% 2
waitingTime  <-  80
#append the split point coloum
#This needed as there is no direct easy split point
faithDt[1:splitlimit,split := 1]
faithDt[splitlimit + 1:nrow(faithDt),split := 2]


if(file.exists("parallel_debug.txt")){
  file.remove("parallel_debug.txt")
}

#Create a cluster number of nodes,timeout seconds linux only, debugfile
#any print messages from the cluster will be printed to the file
cl <- makeCluster(2,timeout = 3600,outfile = "parallel_debug.txt")
#Register the function with doparallel
registerDoParallel(cl)

#Export the libary into the cluster nodes
clusterEvalQ(cl,library(data.table))

my_data <- foreach(i = 1:2) %dopar% worker(faithDt[,split == i],waitingTime)

#now my_data is a list use the rbindlist to combine into one data.table
faithDt <-  rbindlist(my_data)







