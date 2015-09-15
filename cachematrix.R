## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then the cachesolve should
##   retrieve the inverse from the cache.

## by Jianglin Huang @ Coursera R prog by J Hopkins U
## jianhuang7-c@my.cityu.edu.hk

## create a function which starts with a null matrix argument
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMat <- function(solve) m <<- solve
  getMat <- function() m
  
  list(set = set, get = get,
       setMat = setMat,
       getMat = getMat)
}


## used to get the cache of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMat()
  if(!is.null(m)) {
    message("getting cached Inverse Matrix")
    return(m)
  }
  #if the inverse if not there, first it is calculated and then retrieved.
  data <- x$get()
  m <- solve(data, ...)
  x$setMat(m)
  m
}
