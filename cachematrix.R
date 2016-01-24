## The makeCacheMatrix will store the inverse matrix that was computed by cacheSolve function
## and allow cacheSolve function to reuse the inverse matrix many times without any further computation.

## This function act as a cache to store an original matrix and inverse matrix
## IT contained setter and getter methods that allow other function to read and write to it's local variable.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    ## if this 'set' function was called,
    ## set a new matrix to x and reset inv(Cache).
    set <- function(y = matrix()) {
      x <<- y
      inv <<- NULL
    }
    # return an original matrix
    get <- function(){
      x
    }
    # set inverse matrix
    setInverse <- function(invMatrix = matrix()){
      inv <<- invMatrix
    }
    # return inverse matrix
    getInverse <- function(){
      inv
    }
  # return a list of functions that use to access the cache
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function will compute an inverse matrix and store it via setInverse function of makeCacheMatrix()
## If this function was repeatly called many times, it will pull the inverse matrix via getInverse function of makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## if the inverse matrix was computed return it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## else, get an original matrix, compute an inverse matrix then store to the cache
  origin <- x$get()
  inv <- solve(origin)
  x$setInverse(inv)
  inv
}


## ------------------------ this section is for testing ------------------------
m <- makeCacheMatrix(x = matrix(data = c(4,2,7,6),nrow = 2))
## compute inverse matrix for the first time.
cacheSolve(m)
## calling inverse matrix from cache.
cacheSolve(m)
