## These two functions toghether provide the necessary functionallity to do fast 
## cached matrix inversions. The firs of these provide a means of constructing 
## an object capable of holding the necessary state w.r.t. the matrix, its inverse, 
## and whether or not the cache has become stale.

## This function creates a special "matrix" object that can cache its inverse.
## Not that execution will be stopped if a non matrix object is passed as argument.
makeCacheMatrix <- function(x = matrix()) {
    
    if (!class(x) == "matrix") {
        stop(c("Attempting make from non matrix data:", x))
    }
    
    #Initialization
    ix <- NULL
    
    ## Sets the data of the cached matrix object and invalidates the cache.
    set <- function(y) {
        
        if (!class(y) == "matrix") {
            stop(c("Attempting to set non matrix data:", y))
        }
        
        x  <<- y
        ix <<- NULL
    }
    
    ## Gets the data of the cached matrix object.
    get <- function() {
        return(x)
    } 
    
    ## Sets the inverse of the cached matrix oject.
    setinverse <- function(inverse) {
        
        if (!class(inverse) == "matrix") {
            stop(c("Attempting to set non matrix inverse data:", inverse))
        }
        
        ix <<- inverse
    }
    
    ## Gets the inverse of the cached matrix object.
    ## NOTE: Returns null when not set.
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
    
    currentInverse <- x$getinverse()
    
    ## Return cached value if available
    if(!is.null(currentInverse)) { 
        return(currentInverse)
    }
    
    ## Calculate inverse and cache it before returning
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    
    return(inverse)
}

## Function for running unit tests.
runTests <- function() {
    library('RUnit')
    source('cachematrix.R')
    
    test.suite <- defineTestSuite("Cached Matrix",
                                  dirs = file.path("tests"),
                                  testFileRegexp = '*.R')
    
    test.result <- runTestSuite(test.suite)
    printTextProtocol(test.result)
}
