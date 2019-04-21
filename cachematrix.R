## Assignment: Caching the Inverse of a Matrix (R Programming Coursera Week 3)
## It contains 2 functions which are:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


##============================================================================================================

## Part 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Declaring an inverse matrix with the initial value = NULL 
  
  im <- NULL 
  
  ## method to set the value of the original matrix
  
  set <- function(matrix) 
  {
    x <<- matrix
    im <<- NULL
  }
  
  ## Method to get the value of the original matrix
  
  get <- function() 
  {
    x
  }
  
  ## Method to set the inverse of the original matrix
  
  setInverse <- function(inverse)
  {
    im <<- inverse
  }
  
  ## Method to get the inverse matrix
  
  getInverse <- function()
  {
    im
  }
  
  ## Return the list of the methods
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##============================================================================================================
## Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  
  ## get the inverse matrix
  
  im <- x$getInverse()
  
  
  ## get the value of the inverse matrix (if inverse matrix is not null)
  
  if( !is.null(im) ) 
  {
    message("getting cached data")
    return(im)
  }
  
  
  ## get the data from the original matrix (if the inverse matrix is null)
  
  data <- x$get()
  
  
  ## Use the solve function to inverse the matrix
  
  im <- solve(data,...)
  
  ## Set the inverse matrix 
  
  x$setInverse(im)
  
  ## Return the inverse matrix
  
  im
}
