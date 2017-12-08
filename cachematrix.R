#' Cache result into the object
#' 
#' @parameter x Matrix 

makeCacheMatrix <- function(x = matrix()) {
  imCache <- NULL
  set <- function (yCache) {
    x <<- yCache
    imCache <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) imCache <<- inverse
  getinverse <- function () imCache
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#' Compute the inverse of a matrix
#'
#' @details We will assume that every matrix has it inverse
#' @parameter x makeCacheMatrix
#' @example
#' 
#' Declare a matrix   
#' #a <- matrix(rnorm(9), nrow = 3, ncol = 3)
#' texample <- makeCacheMatrix(a)
#' cacheSolve(texample)

cacheSolve <- function(x, ...) {
  inverseM <- x$getinverse()
  if (!is.null(inverseM)) {
    print("Getting cached data")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setinverse(inverseM)
  inverseM
}
