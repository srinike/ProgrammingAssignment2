## This file contains two functions, makeCacheMatrix(..) and cacheSolve(..).
## The function 'makeCacheMatrix(..)' is used for creating an special object that store 
## the matrix and caches it inverse value. The function 'cacheSolve(..)' is used for finding
## the inverse of the matrix.

## makeCacheMatrix() - This function creates a object that store a matrix and its inverse and has functions 
## to set and get the matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  #xInverseCache is the variable that caches the inverse of the given matrix 'x'.
  xInverseCache <<- NULL
  
  # This function store the value of the given matrix. At this time, the xInverseCache that stores the
  # inverse matrix is reset to NULL value.
  setMatrix <- function(y) {
    x <<- y
    xInverseCache <<- NULL
  }
  
  # This function returns the matrix
  getMatrix <- function() {
    x
  }
  
  # This function assigns the value of the inverse of the matrix to the caches variable 'xInverseCache'.
  setInverseMatrix <- function(invMatrix) {
    xInverseCache <<- invMatrix
  }
  
  # This function returns the cached value of the inverse of matrix stored in xInverseCache
  getInverseMatrix <- function() {
    xInverseCache
  }
  
  # Retuning the list of the functions to get and set the matrix and its inverse.
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}

## cacheSolve() - This function returns the inverse of the matrix. 
## If the inverse of the matrix is already cached, it will return the cached value. 
## If the inverse of the matrix is not cached, it will compute the inverse of the
## matrix and cache it using the setInverseMatrix()

cacheSolve <- function(x, ...) {
  
  # Get the cached inverse of the given matrix.
  inverseM <- x$getInverseMatrix()
  
  # If the cached inverse is not null, return it.
  if(!is.null(inverseM)) {
    message("Getting cached data")
    return(inverseM)
  }
  
  # If the cached inverse of the matrix is NOT set, compute the inverse and set it to the cache.
  matrixData <- x$getMatrix()
  inverseM <- solve(matrixData)
  x$setInverseMatrix(inverseM)
  
  # Return the inverse of the matrix
  inverseM

}
