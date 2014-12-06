## Implements unit tests for cached matrix solution

## Test initialization 
test.makeCacheMatrixInitialization <- function() {
  
  ## makeCacheMatrix should throw exception if non matrix argument is given
  checkException(makeCacheMatrix(NULL))
  
  ## makeCacheMatrix should be initalized to contain empty matrix if no argument given
  checkEquals(matrix(), makeCacheMatrix()$get())
  
  ## makeCacheMatrix should be correctly initialized when argument is specified
  a <- matrix(1:20, 4, 5)
  checkEquals(a, makeCacheMatrix(a)$get())
  
  ## makeCacheMatrix should initialize the inverse to NULL
  checkEquals(NULL, makeCacheMatrix()$getinverse())
}

## Test behavior of set function
test.makeCacheMatrix_set <- function() {
  
  ## set function should throw exception if non matrix argument is given
  a <- makeCacheMatrix()
  checkException(a$set(NULL))
  
  ## set function should correctly set data if argument is valid
  b <- matrix(1:20, 4, 5)
  a$set(b)
  checkEquals(b, a$get())
  
  ## set function should set inverse value to NULL
  c <- matrix(20:40, 4, 5)
  a$setinverse(c)
  a$set(b)
  checkEquals(NULL, a$getinverse())
}

## Test behavior of setinverse function
test.makeCacheMatrix_setinverse <- function() {
  
  ## setinverse function should throw exception if non matrix argument is given
  a <- makeCacheMatrix()
  checkException(a$setinverse(NULL))
  
  ## setinverse function should correctly set inverse matrix value if argument is valid
  b <- matrix(1:20, 4, 5)
  a$setinverse(b)
  checkEquals(b, a$getinverse())
}

## Test behavior of getinverse function
test.makeCacheMatrix_getinverse <- function() {
  
  ## getinverse function should return NULL in no inverse matrix set
  a <- makeCacheMatrix()
  checkEquals(NULL, a$getinverse())
  
  ## getinverse function should return inverse matrix if set
  b <- matrix(1:20, 4, 5)
  a$setinverse(b)
  checkEquals(b, a$getinverse())
}