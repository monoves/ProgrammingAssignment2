##The purpose of this is to: 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
## modeled after the mean example, in order to return a reverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  s.inv <- function(inverse) inv <<- inverse 
  g.inv <- function () inv
  list(set = set, 
       get = get, 
     s.inv = s.inv, 
     g.inv = g.inv)
}
## calculates the inverse matrix using the cache
cacheInverse <- function(x, ...) {
  inv <- x$g.inv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #the data of the matrix is called in order to then find the matrix
  inv <- solve(data, ...)
  x$s.inv(inv) ##where the made inverse is then called back
  return(inv) ##and returned, printing
}
