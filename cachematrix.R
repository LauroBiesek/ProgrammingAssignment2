## These functions will calculate and cache the inverse of a matrix. If the same matrix's inverse is requested
## the function will return the already calculated inverse matrix stored in the cache

## Creates a special matrix that can cache the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  #This variable will store the inverse matrix
  inverse <- NULL
  #This function will store the matrix in the  enviromment
  setMatrix <- function(matrix_input){
    x <<- matrix_input
    inverse <<- NULL
  }
  #This function will return the matrix stored
  getMatrix <- function() x
  #This function will store the matrix in the environment
  setInverse <-function(Inverse_matrix) inverse <<- Inverse_matrix
  #And this will return the matrix stored
  getInverse <-function() inverse
  
  #Returns a list with all functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of the matrix, if it's inverse has already beeen calculated, it returns 
## the inversed matrix stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  a <- x$getMatrix()
  inverse <- solve(a)
  x$setInverse(inverse)
  inverse
}
