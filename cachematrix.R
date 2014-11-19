# This file implements two functions: makeCacheMatrix and cacheSolve.
# Together they provide a way to construct a matrix object that caches
# its inverse computation, provinding faster access to it.


# makeCacheMatrix creates an object that represents a matrix that the
# inverse operation is cached for faster computations.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(x_new) {
        x <<- x_new
        x_inv <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(x_inv_new) {
        x_inv <<- x_inv_new
    }
    get_inverse <- function() {
        x_inv
    }
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

# cacheSolve solves the inverse of the matrix x and caches its results
# so that later requests are not calculated again
cacheSolve <- function(x, ...) {
    xi <- x$get_inverse()
    if(!is.null(xi)) {
        message("getting cached inverse matrix")
        return(xi)
    }
    x_data <- x$get()
    xi <- solve(x_data)
    x$set_inverse(xi)
    xi
}
