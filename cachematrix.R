## Put comments here that give an overall description of what your
## functions do
        ## makeCacheMatrix function to returns a list containing functions to
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse
        ## 4. get the inverse
        ## the list is used as the input for cacheSolve()
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
      set = function(y) {
        x <<- y
        inv <<- NULL
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
        ## cacheSolve returns inverse of the original matrix input from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv = x$getinv()

      if (!is.null(inv)){
        return(inv)
      }
      # else, calculates the inverse 
      mat.data = x$get()
      inv = solve(mat.data, ...)
      x$setinv(inv)
      return(inv)
}
