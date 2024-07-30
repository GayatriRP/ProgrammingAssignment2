## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(matlib)
library (MASS)
makeCacheMatrix <- function(x = matrix()) {
  
  #create inverse variable and assign as null
  invrse <- NULL
  set <- function(y) {
    x <<- y
    invrse <<- NULL
  }
  get <- function() x
  #here we are setting the inverse using inverse function
  setInverse <- function(inverse) invrse <<- inverse
  getInverse <- function() invrse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## method/function to get the inverse from cache and return inverse
## The code works for matrix(1:4, 2, 2), it works only on sqaured matrix even though library is installed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrse <- x$getInverse()
  print (invrse)
  #check whether the invrse variable is null, return when its not null
  if (!is.null(invrse)) {
    message("cached data..")
    return(invrse)
  }
  #if the inverse is not created in above we set it via setInverse function
  matrixCreated <- x$get()
  invrse <- solve(matrixCreated)
  print (invrse)
  x$setInverse(invrse)
  invrse
}
