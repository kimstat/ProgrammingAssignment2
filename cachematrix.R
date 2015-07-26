## The functions below create an object that stores a numeric matrix and caches its inverse

# makeCacheMatrix creates a list that contains a function to
# 1) set the value of a matrix
# 2) get the value of a matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve calculates the inverse of the list created with the above function
# It first checks to see if the inverse has already been calculated
# If so, it gets the inverse from the cache and skips the computation
# Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via
# the setinverse function.

cacheSolve <- function(x, ...) {
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
