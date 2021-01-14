#Functions used in skylineProper.r


#Checks the dataset for rows containing NA values
#It will add the index of these rows in a vector and will
#return the dataset with those rows removed

sanitizeData <- function(dataset){
  rowsToDelete <- c()

  for (i in 1:nrow(dataset)){
    if (anyNA(dataset[i,1:ncol(dataset)])){
      rowsToDelete <- c(rowsToDelete, i)
    }
  }

  #Delete the faulty rows
  if (length(rowsToDelete) > 0){
    dataset <- dataset[-c(rowsToDelete),]
  }

  return(dataset)
}

#Will remove all the identical pareto rows
#from the dataset. This will return a smaller dataset
#to be used in subsequent pareto calculations.
recalculate <- function(dataset,pareto){
  rowsToDelete <- c()
  for (i in 1:nrow(pareto)){
    for (j in 1:nrow(dataset))
    {
      if (identical(dataset[j,1:ncol(dataset)], pareto[i,1:ncol(pareto)])){
        rowsToDelete <- c(rowsToDelete,j)
      }
    }
  }
  #Delete the rows from the dataset
  dataset <- dataset[-c(rowsToDelete),]

  return(dataset)
}


#This function will check the dataset for any values in specified
#dimension that are outside (lower,upper) and will delete the rows
#containing these values
setConstraint <- function(dataset,dimension,lower,upper){
  rowsToDelete <- c()
  for (i in 1:ncol(dataset)){
    if (identical(dimension,colnames(dataset)[i])){
      d <- i
      break
    }
  }
  for (i in 1:nrow(dataset)){
    if ((dataset[i,d] < lower) || (dataset[i,d] > upper)){
      rowsToDelete <- c(rowsToDelete,i)
    }
  }

  #Delete the rows that contain out of bounds values
  if (length(rowsToDelete) > 0){
    dataset <- dataset[-c(rowsToDelete),]
  }

}

#Check the dataset for any points that are better
#than the candidate. If there is a better point, return
#true

existsBetter <- function(candidate,pareto,preferences){
  if (length(pareto) > 0){
    for (s in 1:nrow(pareto)){
      if (betterInAll(candidate,pareto[s,1:ncol(pareto)],preferences) &&
          betterInAtLeastOne(candidate,pareto[s,1:ncol(pareto)],preferences)) {
        return(TRUE)
      }

    }
  }

  return(FALSE)
}

#Similar to existsBetter. Used in strict dominance cases
existsBetterStrict <- function(candidate,pareto,preferences){
  if (length(pareto) > 0){
    for (s in 1:nrow(pareto)){
      for (d in 1:ncol(pareto)){
        if (preferences[d] == "low"){
          if (candidate[1,d] >= pareto[s,d]){
            return (TRUE) #return true if candidate is STRICTLY worse
          }
        }else if (preferences[d] == "high"){
          if (candidate[1,d] <= pareto[s,d]){
            return (TRUE)
          }
        }
      }
    }
  }

  return(FALSE)
}


#If the 2nd point is worse than the candidate, return
#false. If not, return true to indicate that point is better
#than the candidate
betterInAll <- function(candidate,newCandidate,preferences){
  for (d in 1:ncol(newCandidate)){
    if(preferences[d] == "low"){
      if(newCandidate[d] > candidate[d]) return(FALSE)
    }
    else if(preferences[d] == "high"){
      if(newCandidate[d] < candidate[d]) return(FALSE)
    }
  }
  return(TRUE)
}

#Compare 2 points. If the new point is better in at least one
#dimension, return tru. Else, return false to indicate that the
#2nd point is not better in any dimensions
betterInAtLeastOne <- function(candidate,newCandidate,preferences){
  for (d in 1:ncol(newCandidate)){
    if(preferences[d] == "low"){
      if(newCandidate[d] < candidate[d]) return(TRUE)
    }
    else if(preferences[d] == "high"){
      if(newCandidate[d] > candidate[d]) return(TRUE)
    }
  }
  return(FALSE)
}

#Compare 2 points. If the new candidate is NOT better than the
#candidate, return false. Else, return TRUE to indicate that the
#new candidate is STRICTLY better that the candidate
betterStrict <- function(candidate,newCandidate,preferences){
  for (d in 1:ncol(newCandidate)){
    if(preferences[d] == "low"){
      if (newCandidate[d] >= candidate[d]) return(FALSE)
    }
    else if(preferences[d] == "high"){
      if(newCandidate[d] <= candidate[d]) return(FALSE)
    }
  }
  return(TRUE)
}

#Checks to see if candidate is already in pareto
#Used to eliminate duplicate entries
alreadyInPareto <- function(candidate,pareto){
  if (length(pareto) > 0){
    for (i in 1:nrow(pareto)){
      if (identical(candidate, pareto[i,1:ncol(pareto)])){
        return (TRUE)
      }
    }
  }

  return (FALSE)
}

calculatePareto <- function(dataset, preferences, strict=FALSE){
  rows <- nrow(dataset)
  dim <- ncol(dataset)

  pareto <- c()

  for (i in 1:rows){
    j <- i

    candidate <- dataset[j,1:dim]

    if(strict){
      if (isTRUE(existsBetterStrict(candidate,pareto,preferences))) next
    }
    else{
      if (isTRUE(existsBetter(candidate,pareto,preferences))) next
    }

    for (j in i:rows){
      if (j != i){
        if(strict){
          if(isTRUE(betterStrict(candidate,dataset[j,1:dim],preferences))){
            candidate <- dataset[j,1:dim]
          }
        }
        else{
          #compare candidate to new item. TRUE means the new point is better
          if (isTRUE(betterInAll(candidate, dataset[j,1:dim], preferences)) &&
              isTRUE(betterInAtLeastOne(candidate, dataset[j,1:dim],preferences))){
            candidate <- dataset[j,1:dim]
          }
        }
      }
    }
    if (!alreadyInPareto(candidate,pareto)){
      pareto <- rbind(pareto,candidate)
    }

  }


  return(pareto)

}

