## There are two functions: makeCacheMatrix and cacheSolve. The first function
## is to create a special "matrix" object that can cache its inverse. The second
## function is to retrieve the inverse from the cache if it has already been 
## calculated. If the inverse is not in the cache, the cacheSolve function will 
## calculate the inverse and put it into the cache.


## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    set_inv_matrix <- function(inv_matrix) im <<- inv_matrix
    get_inv_matrix <- function() im
    list(set = set, get = get, 
         set_inv_matrix = set_inv_matrix, 
         get_inv_matrix = get_inv_matrix)
}


## Compute the inverse of the special "matrix" created by makeCacheMatrix 
## function. Retrieve the inverse from the cache if the inverse has already 
## been calculated.
cacheSolve <- function(x, ...) {
    im <- x$get_inv_matrix()
    if (!is.null(im)) {
        message("getting cached data")
        return (im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$set_inv_matrix(im)
    im
}


