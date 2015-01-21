makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(data=NA)
  set <- function(y) {
    x <<- y
    m <<- NA
  }
  get <- function() x
  setinverted <- function(inverted) m <<- inverted
  getinverted <- function() m
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}

cachesolve <- function(x=matrix(), ...) {
  m<-matrix(data=NA)
  m <- x$getinverted()
  if(all(!is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverted(m)
  m
}