### programming assignment 2 for "R Programming" course on Coursera ###
###
## comments that give an overall description of what my functions do!
### Inverse function is time-consuming to calculate, especially when it's used within a loop function.
### It's possible to save time if it's to cache the value instead of calculate repeatedly.

## short comment describing this function
### In new functions, it just replaces "mean" in makeVector and cacheMean with "inverse" in core parts.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
  
}

## short comment describing this function
### If cached value exists, it's returned. Otherwise, newly calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  ## If there is no cashed data, then use the new data
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
