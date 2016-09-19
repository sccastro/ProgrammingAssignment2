## Put comments here that give an overall description of what your
## functions do

## I have two functions, makeCacheMatrix and cacheSolve. 

##makeCacheMatrix:

# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(inverse) m <<- inverse
  getm <- function() m
  list(set=set, get=get, setm=setm, getm=getm)
}


## Write a short comment describing this function

# This function always attempts to invert the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setm(m)
  m
}

#Test Run
B <- matrix(c(2, -4, -4, 2), nrow=2, ncol=2)
B
x <- makeCacheMatrix(B)
x$get()
#        [,1] [,2]
# [1,]    2   -4
# [2,]   -4    2
cacheSolve(x)
#         [,1]       [,2]
# [1,] -0.1666667 -0.3333333
# [2,] -0.3333333 -0.1666667



