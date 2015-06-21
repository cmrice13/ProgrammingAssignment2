## Programming assignment two.... functions that get and use a matrix inversion object.

## Takes a matrix and returns an object that holds the original matrix and the inverse of the matrix.  Getter and setter for each.

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


## Takes the object created by the function makeCacheMatrix and calculates the inverse if it doesn't exist.  Otherwise it uses the prev
## calculated value for the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  library(MASS)
  m <- ginv(data)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
