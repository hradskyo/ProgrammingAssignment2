## A pair of functions that cache the inverse of a matrix

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y 
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setmatice <- function(matice) m <<- matice #set the value of the iverse matrix
  getmatice <- function() m #get the value of the iverse matrix
  list(set = set, get = get,
       setmatice = setmatice,
       getmatice = getmatice)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatice()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatice(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
