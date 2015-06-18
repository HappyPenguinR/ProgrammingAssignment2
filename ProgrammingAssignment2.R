makeCacheMatrix <- function(x = matrix()){
  #creates a special object that can cache its inverse
  inv <- NULL
  set <- function(y) {
    #1 set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #2 get the matrix
  setinverse <- function(inverse) inv <<- inverse
  #3 set the inverse
  getinverse <- function() inv
  #4 get the inverse
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #computes inverse of matrix created by makeCacheMatrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    #check to see if the inverse has been calculated
    message ("getting cached data")
    return(inv)
    #retrieves the inverse from the cache
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}

