## These functions give a method to compute the inverse of invertible square
## matrix. If the entered matrix is first time proceed we pass throu all needed
## computation and we cache the calculated matrix. In the case of the second 
## time processing we just return the cached inverse.
## So we write the following two functions.


## This function creates a special "matrix" object which is a list of four 
## functions used for getting and setting the matrix itself and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function has as argument the special "matrix" object and return its 
## inverse if this is the first time and return the cached matrix if it was 
## already calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
