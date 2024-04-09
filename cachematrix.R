## This two functions first generate the inverse of a matrix
## but only recalculate it if it has not done before


## The makeCacheMatrix function generates a list o 4 functions where s is the 
## solve function that is reset to NULL and x is a matrix argument
## set assigns the matrix to set and resets the s to NULL
## get retrieves the matrix X
## setsolve assigns the solve to s
## getsolve retrieves the s

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The cacheSolve function takes the list from makeCacheMatrix and 
## looks if the value is already there
## If the values is there retrieves the value as a result, 
## if the values is not there it calculates the solve() on the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

# Testing the functions:
my_matrix <- matrix(c(11,35,13,56,22,23,66,32,33),nrow=3,ncol=3, byrow=TRUE)
cachematrix_ex1 <- makeCacheMatrix(my_matrix)
cachesolve_ex1 <- cacheSolve(cachematrix_ex1)
# If correct the multiplication of the matrices will result in the identity matrix
round(my_matrix %*% cachesolve_ex1, digits =3)
