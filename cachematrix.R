## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix

# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to

        #set the value of the matrix
        #get the value of the matrix
        #set the value of the inverse matrix
        #get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

##cacheSolve

# This function calculates the inverse  of the matrix created with the above
# function. However, it first checks to see if this inverse has already been found.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the input matric and sets the value of the 
# inverse in the cache using the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
