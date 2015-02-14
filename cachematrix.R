## Functions to create a cache of a matrix object in order
## to cache a inverse matrix. After, consult if a cache exists,
## if so, returns that inverse matrix directly consulting
## the cache; otherwise, recalculates the inverse matrix
## and return it.



## makeCacheMatrix() creates a matrix object in
## order to cache its inverse

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


## cacheSolve() returns a inverse of a matrix 
## consulting, firstly, if there is a cache containing a
## inverse matrix, if so, the function gets this inverse 
## matrix directly in cache. Otherwise, generates the inverse
## matrix and return it.

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