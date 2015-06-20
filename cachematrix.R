# makeCacheMatrix is a function which creates a "matrix" object that can cache its inverse.

# set is a function that changes the matrix stored in the main function.
# get is a function that returns the matrix x stored in the main function.
# setiverse and getinverse functions store the value of the input in a variable m into the main function makeCacheMatrix (setinverse) and return it (getinverse).
# list stores the 4 functions in the function makeCacheMatrix, so that when makeCacheMatrix is assigned to an object, the object has all the 4 functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function which calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 

# It checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}