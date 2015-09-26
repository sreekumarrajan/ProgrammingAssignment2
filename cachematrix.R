## These functions create a special vector that caches the inverse 
## of matrix and retrieves the cached result if available

## creates special vector that caches the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function()x
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  getInverse <- function()inverse
  
  setInverse <- function(inv)inverse <<- inv
  
  list(get=get,set=set,getInverse=getInverse, setInverse=setInverse)
}


## Returns a matrix that is inverse of x, if already calculated
## else calculates fresh

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
  
}
