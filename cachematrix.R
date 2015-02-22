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

#Test Run
x <- matrix(runif(9, 1, 10), 3, 3)
test = makeCacheMatrix(x)
#get the matrix
test$get()
#         [,1]     [,2]     [,3]
#[1,] 7.403411 2.695785 8.251071
#[2,] 8.599819 8.361539 7.445455
#[3,] 7.247558 1.161857 4.442682

#get the inverse
test$getinv()
#            [,1]        [,2]        [,3]
#[1,] -0.17362511  0.01456131  0.29805812
#[2,] -0.09599152  0.16394988 -0.09648416
#[3,]  0.30834670 -0.06663099 -0.23591437

#inverse of original matrix from cache 
cacheSolve(test)
#            [,1]        [,2]        [,3]
#[1,] -0.17362511  0.01456131  0.29805812
#[2,] -0.09599152  0.16394988 -0.09648416
#[3,]  0.30834670 -0.06663099 -0.23591437
