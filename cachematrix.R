# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## The resulting list contains functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix())
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, 
       setinv=setinv,
       getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## It assumes that the Matrix is squared and invertible

cacheSolve <- function(x, ...)
{
  i <- x$getinv()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
