## Put comments here that give an overall description of what your
## functions do

## ===========================================
## makeCacheMatrix
## ===========================================
## function will create a matrix and four functions
## -------------------------------------------
## set(y)
## set function takes an argument and stores it as the matrix
## -------------------------------------------
## get()
## returns a matrix.  the same matrix as created by the set function
## -------------------------------------------
## setinv(inverse)
## setinv function takes an argument and stores it as the matrix
## this function is meant to store the inverse of the mtrix created by the set function
## -------------------------------------------
## getinv()
## returns a matrix.  The same matrix as created by the setinv function
## -------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    # default the inverse of the matrix to NULL
    inv <- NULL

    # set function takes an argument and assigns (sets) it to the X variable
    # defaults inv to null
    set <- function(y) {
            x <<- y
            inv <<- Null
    }

    # get function returns x
    get <- function() x

    # setinv function stores a matrix in the inv value
    # - meant to be used to set the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse

    # getinv function returns inv value
    # - meant to return the inverse of a matrix
    getinv <- function () inv

    # return value of the makeCacheMatrix function
    # list of the four functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## ===========================================
## cacheSolve(x, ...)
## Function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## ===========================================

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    # call the getinv function to get the inverse of the matrix
    inverse <-x$getinv()

    # if the inverse has already been calculated (not null) then return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # since the inverse has not been calculated (null) we need to calculate it.
    # get the matrix, calculate the inverse using the solve method, store the inverse and return the inverse
    data<-x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## ===========================================
## testing commands for this
## ===========================================
# ## create a simple square matrix and store its value
# test <- matrix(data=c(7,3,-2,5), nrow=2, ncol=2)
## ##
## ## view the matrix
# test
## ##
## ## assign our matrix using our make function and store the result
# special <- makeCacheMatrix(test)
## ##
## ## view
# special
## ##
## ## view the matrix using the get funcion
# special$get()
## ##
## ## view the inverse of the matrix (should be null since it has not yet been set)
# special$getinv()
## ##
## ## call cacheSolve to get the inverse of the matrix
## ## no message should be displayed indicating the reults was calculated and not returned from cache
# cacheSolve(special)
## ##
## ## call getinv() function to see that inverse has been cached
# special$getinv()
## ##
## ## call cacheSolve to get the inverse of the matrix
## ## a message should be displayed indicating the reults was retrieved from cache
# cacheSolve(special)

