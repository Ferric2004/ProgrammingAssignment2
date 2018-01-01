## These two function calculate the inverse of a matrix and cache its values, avoiding to repeat
## computations that would take too long to be executed each time. 

## The first function, makematrix, takes an invertible matrix and creates a list with 4 functions.
## These functions store the values of the matrix and the values of the inverse matrix


makematrix <- function(z = matrix()) {
    m <- NULL
    set <- function(y) {
        z <<- y
        m <<- NULL
    }
    
    get <- function() {z}
    setsolve <- function(solve) {m <<- solve}
    getsolve <- function() {m}
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The second functions takes the list created as an argument, check if the inverse of the matrix
## has been calculated and then proceed to calculate it. If the inverse has yet been stored, a message
## is returned and the inverse is retrieved and returned.

cachemean <- function(z, ...) {
    m <- z$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- z$get()
    m <- solve(data, ...)
    z$setsolve(m)
    m
}
