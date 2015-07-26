## The following functions can work together to allow efficient 
## calculation of the inverse of a matrix. 
## In order to mitigate the potentially time 
## consuming operation of inversing a matrix, 
## the inverse of the matrix value will be cached for 
## reuse on future executions within the same session

## The makeCacheMatrix is a function that returns a list of functions
## set : sets the matrix
## get : gets the matrix
## setinverse : sets the inverse of the matrix
## getinverse : gets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the given matrix x. 
## x is a matrix that has to be built using the makeCacheMatrix funciton
## The result of the calculation will be cached 
## so future executions of the function for the same matrix will 
## not need to compute the calculation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
