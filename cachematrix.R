## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  if (!class(x) == "matrix") {
    stop(c("Attempting make from non matrix data:", x))
  }
  
  #Initialization
  ix <- NULL
  
  ## Sets the data of the cached matrix object
  set <- function(y) {
    
    if (!class(y) == "matrix") {
      stop(c("Attempting to set non matrix data:", y))
    }
    
    x  <<- y
    ix <<- NULL
  }
  
  ## Gets the data of the cached matrix object
  get <- function() {
    return(x)
  } 
  
  ## Sets the inverse of the cahced matrix oject
  setinverse <- function(inverse) {
    
    if (!class(inverse) == "matrix") {
      stop(c("Attempting to set non matrix inverse data:", inverse))
    }
    
    ix <<- inverse
  }
   
  ## Gets the inverse of the cached matrix object
  ## NOTE: Returns null when not set
  getinverse <- function() {
    return(ix)
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

runTests <- function() {
  library('RUnit')
  source('cachematrix.R')
  
  test.suite <- defineTestSuite("Cached Matrix",
                                dirs = file.path("tests"),
                                testFileRegexp = '*.R')
  
  test.result <- runTestSuite(test.suite)
  printTextProtocol(test.result)
}
