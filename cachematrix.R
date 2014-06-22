## Put comments here that give an overall description of what your
## functions do
##
## Calculate inverse of matrix NxN using adjoint method

#  det(A)= sum(aij x det(Aij)) = a11 x det(A11) - a12 x det(A12)  + - + ann x det(Ann)
#  sign(i,j) = -1 ^ (i+j)

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
    rows <- dim(matrix)[1]
    cols <- dim(matrix)[2]
    if (rows != cols) {
        stop("non square matrix!")
    }
    
    inverse <- diag(rows)
    
    for(col in 1:cols) {
        for(row in 1:rows){
            if (row == col && matrix[row,col] != 1) {
                matrix <- transform(matrix, row, col, 1)
            }
            if (row != col && matrix[row,col] != 0) {
                matrix <- transform(matrix, row, col, 0)
            }
        }
    }
    
    matrix
}

transform <- function(matrix, row, col, value) {
    if (value == 1) {
        t <- matrix[row, col]
        if (t == 0) {
            non_zero_row <- next_non_zero_row(matrix, row, col)
            #print(paste("row=", row, "col=", col, "value=", matrix[row,col]))
            #print(matrix)
            #print(non_zero_row)
            matrix[row,] <- matrix[row,] + (non_zero_row * (1/non_zero_row[col]))
        } else {
            matrix[row,] <- matrix[row,] * (1/t)
        }
    }
    if (value == 0) {
        non_zero_row <- next_non_zero_row(matrix, row, col)
        t <- matrix[row,col]
        matrix[row,] <- matrix[row,] - (non_zero_row * (t/non_zero_row[col]))
    }
    matrix
}

next_non_zero_row <- function(matrix, actual_row, actual_col) {
    result <- NULL
    rows = dim(matrix)[1]
    for(row in 1:rows) {
        if (row != actual_row) {
            if (matrix[row, actual_col] != 0) {
                result <- matrix[row,]
            }
        }
    }
    if (is.null(result)) {
        stop("non invertible matrix!")
    }
    result
}