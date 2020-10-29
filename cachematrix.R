## The following functions create a special matrix with four helper functions and then calculate 
## the inverse of the matrix by utilizing cache to speed up time


## Function the creates a special matrix with 4 helper functions
makeCacheMatrix <- function(x = matrix()) {
  # initialize cache as null
  cache <- NULL
  # set matrix value 
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  # return matrix value
  get <- function() x
  #invert matrix and stroe in cache
  setInverse <- function(inverse) cache <<- inverse
  # return stored cache value
  getInverse <- function() cache
  # return list of functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function that calculates the inverse of a martix use cache
cacheSolve <- function(x, ...) {
  # get the stored inverse of matrix
  cache <- x$getInverse()
  
  # if exists, retrive from cache, otherwise compute a new one
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  
  # compute a matrix
  matrix <- x$get()
  
  # compute inverse and save in cache
  cache <- solve(matrix, ...)
  x$setInverse(cache)
  
  cache  
}
