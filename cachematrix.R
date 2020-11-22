# calcluate the inverse of a matrix and cache the inverse and 
# return the cached inverse on next function call for the same matrix


# below functions caches the inverse of a matrix for the matrix supplied as argument
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# below function checks for the inverse of a matrix for the given matrix 
# and if not present calculates the inverse of matrix supplied as argument
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data!")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


# Test Case
M <- matrix(c(1,2,3,4), 2, 2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
cacheSolve(M1)
