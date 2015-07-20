## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function will create a special "matrix" object
## that can cache its inverse and the dimension. Note
## that all invertible matrices must have nrows = ncols.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
               x <<- y
               invx <<- NULL
  }
  get <- function() x
  setinv <- function(c)  invx <<- c
  getinv <- function() invx
  list(set=set,get=get, setinv=setinv, 
         getinv=getinv)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invx <- x$getinv()
  if (is.null(invx)){
    matrix <- x$get()
    invx <- solve(matrix)
    x$setinv(invx)
    print ("Creating Cache")
  }
  else { 
      print ("Using cached inverse") 
  }
#Both need to return invx so do it once
    return (invx)
}
