## Put comments here that give an overall description of what your
## functions do

## Overall, given a matrix in the makeCacheMatrix, the first function (makeCacheMatrix) 
## creates placeholders and provides a list of functions that will be used in cacheSolve 
## function to calculate then cache the inverse of the matrix provided in makeCacheMatrix

## Write a short comment describing this function
## Generally speaking makeCacheMatrix holds creates placeholders and provides functions 
## that will be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()   ## Creates a placeholder matrix with 1 row and 1 column with NA in it
    set <- function(y) {    ## This function allows us to reset our x (via x$set()) and replace 
          x <<- y           ## any cached m with an empty matrix
          m <<- matrix()    
    }
    get <- function() x     ## Function that returns x
    setmatrix <- function(matrix) m <<- matrix ## function that caches the value specified in the 
                                               ## setmatrix function (which is done in the cacheSolve function)
    getmatrix <- function() m ## Function that returns m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) ## provides a list of functions
}

## Write a short comment describing this function
## This function looks to see if our matrix m has been cached (by something other than our placeholder)
## If it has been, it returns m, if not it gets data from
## the get function in makeCache then solves the inverse of the matrix.
## The inverse matrix is then cached, and then printed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix() ## sets m to the value specified in getmatrix function above which is an empty matrix
    if(!is.na(m)) {    ## checks to see if matrix is empty/na, if not, returns m 
          message("getting cached data")
          return(m)
    }
    data <- x$get()    ## sets object data to x, or our matrix provided in argument above
    m <- solve(data, ...)  ## saves inverse of our matrix in object m
    x$setmatrix(m)       ## caches the inverse of our matrix in makeCacheMatrix
    m                   ## prints m, the inverse of our matrix
}
