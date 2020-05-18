## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL # create a placeholder for inverse of matrix x
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_x <<- inverse # creating a global variable for inverse matrix
        getInverse <- function() inv_x # calling a global variable for inverse matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
        inv_x <- x$getInverse() # call an inverse matrix and assign its value to inv_x
        if(!is.null(inv_x)) {
                # If the value is not NULL, the show the message about cached data 
                message("getting cached data")
                # And return the cached value
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...) #Calculate the inverse of matrix x
        x$setInverse(inv_x)
        inv_x # return the value of the inverse matrix
}
