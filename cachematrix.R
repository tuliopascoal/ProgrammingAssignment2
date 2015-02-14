## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
           m <- NULL
           set <- function(y) {
                   
                   x <<- y
                   m <<- NULL
           }
           get <- function() x
           set_inv_matrix <- function(solve) m <<- solve
           get_inv_matrix <- function() m
           list (set = set, get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv_matrix()
        if (!is.null(m)) {
                message("cached data detected")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
}