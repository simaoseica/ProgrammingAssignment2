## Assignment2: Caching the Inverse of a Matrix

## `makeCacheMatrix` function returns a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(get = get, 
       set = set, 
       getinverse = getinverse, 
       setinverse= setinverse)
}


## `cacheSolve` function returns the inverse of a square matrix.
## It receives a special cache matrix function to make use of cache that stores previous matrix inversions 

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  
  data <- x$get()
  message("Computing inversed matrix...")
  i <- solve(data,...)
  x$setinverse(i)
  i
}
