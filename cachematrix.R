# These 2 functions are useful for inverting large matrices where the inverse matrix may be used multiple
# times.

# makeCacheMatrix creates a list from a matrix which contains functions for getting/setting the data,
# based on the input matrix supplied, and getting/setting the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

  if (nrow(x) != ncol(x) | length(dim(x)) != 2) {
    stop('The input passed in is probably not invertable. Check inputs before proceeding')
  }
  inv <- NULL

  result <- list()

  result$get <- function() x
  result$set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  result$getInverse <- function() inv
  result$setInverse <- function(z) inv <<- z
  return(result)
}


# cacheSolve accepts either a list from makeCacheMatrix or a matrix itself, and outputs a list
# like one outputted by makeCacheMatrix, with the original data and the inverse cached.

cacheSolve <- function(x, ...) {
  listattrs <- c('get', 'set', 'getInverse', 'setInverse')
  if (!all(listattrs %in% names(x))) { # list must have these attributes if this is to work
    if (nrow(x) == ncol(x) & length(dim(x) == 2)) {  # probably invertable
      x <- makeCacheMatrix(x)
    } else {
      stop('The input passed in is probably not invertable. Check your inputs before proceeding.')
    }
  }
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message('using cached data')
    return(inv)
  }
  allData <- x$get()
  inv <- solve(allData)
  x$setInverse(inv)
  return(x)
}

# tests --------------------------
#testdata <- matrix(c(1.2,3.4,5.0,2.1,4.9,6.7,8.0,.5,9.9), nrow = 3)
#solve(testdata)  # start by making sure it works in this case!
#testCachedMatrix <- makeCacheMatrix(testdata)
#str(testCachedMatrix)
#cacheSolve(testCachedMatrix)  # inverts properly
#cacheSolve(testCachedMatrix)  # seems to work - yields message

## try a non-invertable data.frame
#testdf <- data.frame(a=1:3, b=6:8)
#testCachedMatrix <- makeCacheMatrix(testdf)  #gives an error.

#testCachedMatrix <- makeCacheMatrix(1:10)  # unhandled exception
