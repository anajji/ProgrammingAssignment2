## Create a special matrix that can contain its inverse.

## Create a list of function which contains the matrix input (x) and that can contain its inverse.
makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Compute the invere of the matrix. If the inverse is alrady computed, no calculation necessary;
## else, it will compute the inverse of the matrix using the solve function.
cacheinverse <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

