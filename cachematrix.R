##This function caches the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  ## setting a the value of a matrix using another function.
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  ## get the value of the matrix.
  get <- function(){x}
  
  ## setting the value of an inverse matrix.
  setInverse <- function(inverse){inve <<- inverse}
  
  ## getting the value of an inverse matrix.
  getInverse <- function() {inve} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  ## inverse matrix
  mat <- x$get()
  inve <- solve(mat,...)
  x$setInverse(inve)
  inve
}
