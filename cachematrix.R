## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## Function takes a matrix as an input
  s <- NULL
  set <- function(y) {   ## set function saves the matrix in variable x and assigns NULL to Inversed Matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x    ## get function returns the saved matrix
  setSolve <- function(solve) s <<- solve     ## setSolve assigns the Inversed Matrix to s
  getSolve <- function() s    ## returns the cached Inversed Matrix
  list(set = set, get = get,  ## list having all 4 functions
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve Function

cacheSolve <- function(x, ...) {   ## here x is object of makeCacheMatrix function
  s <- x$getSolve()   ##  Checks if the x obj has the s value defined
  if(!is.null(s)) {   ##  If cachesolve is called again then s will not be NULL and if results in TRUE
    message("getting cached data")   ##  Returning the saved value of Inverse Matrix i.e. 's'.
    return(s)
  }
  data <- x$get()    ##  If cachesolve is called for the very first time then this part solves for 
  s <- solve(data, ...)    ## Inverse Matrix value of 'x' and sets value to 's' variable
  x$setSolve(s)
  s
}

#  Example Solution ---- 1
# mat <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2))
# cacheSolve(mat)
####
####      [,1] [,2]
####[1,]   -2  1.5
####[2,]    1 -0.5
# cacheSolve(mat)
####[getting cached data
####[      [,1] [,2]
####[[1,]   -2  1.5
####[[2,]    1 -0.5

#  Example Solution ---- 2
# mat1 <- makeCacheMatrix()
# mat1$set(matrix(c(1,2,3,4), nrow = 2))
# mat1$get()
####      [,1] [,2]
####[1,]    1    3
####[2,]    2    4
# cacheSolve(mat)
####
####      [,1] [,2]
####[1,]   -2  1.5
####[2,]    1 -0.5
# cacheSolve(mat)
####[getting cached data
####[      [,1] [,2]
####[[1,]   -2  1.5
####[[2,]    1 -0.5
