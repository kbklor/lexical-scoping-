makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y) {
    x <<- y
    b <<- NULL
  }
  get <- function() x
  seti <- function(inverse) b <<- inverse
  geti <- function() b
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  c <- x$geti()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$seti(c)
  return(c)
}
