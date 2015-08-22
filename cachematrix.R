## Put comments here that give an overall description of what your
## functions do

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## the 'makeCacheMatrix' function comprises the following functions:
##  setMatrix - set the value 
##  getMatrix - get the value
##  cacheInverse - get the cached value (i.e., the inverse of the matrix)
##  getInverse - get the inverse

makeCacheMatrix <- function(x = matrix()) {
# holds cached value. NULL as initial
  m <- NULL
  
  #setMatrix
  setMatrix <- function(newValue) {
    x <<- newValue
    m <<- NULL
  }
  
  #getMatrix
  getMatrix <- function() {
    x
  }
  
  # Cache
  cacheInverse <- function(solve) {
    m <<- solve
  }
  
  # Get Cache
  getInverse <- function() {
    m
  }
  
  #return list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
  ## Get cached value
  inverse <- y$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)  
  }
  # if no cache, get Matrix
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  inverse
}

## Create matrix to allow for solve
a <- makeCacheMatrix()
summary(a)

a$setMatrix( matrix(c(1,2,30,40), nrow = 2, ncol = 2))
a$getMatrix()

cacheSolve(a)


