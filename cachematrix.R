### programming assignment 2 for R Programming on Coursera course ###
###
## Put comments here that give an overall description of what your functions do
### Inverse function is time-consuming to calculate, especially when it's used in a loop function.
### It's possible to save time if cache is used instead of calculate repeatedly.

## Write a short comment describing this function
### In new programming, it just replace mean in makeVector and cacheMean with inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
