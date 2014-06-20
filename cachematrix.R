## Put comments here that give an overall description of what your
## functions do
##
## Calculate inverse of matrix NxN using adjoint method

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(matrix) {
        x <<- matrix
        im <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse_matrix <- function(inverse_matrix) {
        im <<- inverse_matrix
    }
    get_inverse_matrix <- function() {
        im
    }
    list(set = set, 
         get = get, 
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$get_inverse_matrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    matrix <- x$get()
    im <- calculate_inverse_matrix(matrix, ...)
    x$set_inverse_matrix(m)
    im
}

calculate_inverse_matrix <- function(matrix) {
    t(matrix)
}
