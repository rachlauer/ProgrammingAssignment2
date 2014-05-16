## Given an invertible matrix, "makeCacheMatrix" and "cacheSolve" create a special matrix object
## that stores a matrix (Which must be invertible) and cache's its inverted matrix.

## The "makeCacheMatrix" matrix function creates a matrix, which is really a list containing a function
## to do the following:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverted matrix
## 4. gets the 


makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_matrix <<- solve
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The "cacheSolve" function calculates the inverse of the matrix (created in the above function).
## It first checks to see if the matrix has been inverted and stored in the cache. If it has then the
## the function gets the inverted matrix from the cache.If not,the function inverts the matrix and sets
## the inverted matrix in the cache using the setinv function.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinv(inv_matrix)
  inv_matrix
}
  
