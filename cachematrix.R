## Assignment: Caching the Inverse of a Matrix
##
## Created by mtsukanov 20/09/2014
##
## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.This assignment is to write a pair of functions that cache the inverse of a matrix.
##
## You can test with mtrx <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2,byrow = TRUE)
##
## This function creates a special "matrix" object that can cache its inverse.

## Function is declared
makeCacheMatrix <- function(x = matrix()) {
  
  #flushes the value of cache
  cache <- NULL
  
  #creates new matrix, flushes cache
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    cache <<- NULL
  }
  
  #loads existing matrix
  getMatrix <- function() {
    x
  }
  
  #inverts matrix
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  #loads inverted matrix
  getInverse <- function() {
    cache
  }
  
  #a list of avaliable functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Function is declared
cacheSolve <- function(y, ...) {
    #loads inversed matrix 
    inverse <- y$getInverse()
 
    #checkes if inversed matrix exists
    if(!is.null(inverse)) {
      message("getting cached data") #will display a message that cached matrix is used
      return(inverse) #standard return for cached matrix
    }
    else {
    #tbd if there is no cached matrix
    newMatrix <- y$getMatrix()  #extracts matrix
    inverse <- solve(newMatrix) #loads function name
    y$cacheInverse(inverse) #creates inversed matrix

    inverse #prints the output 
    }
}
# EoF