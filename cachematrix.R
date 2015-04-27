##Author: Michael Hunt

## makeCacheMatrix

# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to

        #set the value of the matrix
        #get the value of the matrix
        #set the value of the inverse matrix
        #get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        #this variable stores the cached inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #return the wrapped matrix
        get <- function() x
        
        
        #set the alue of the inverse matrix
        setinv <- function(solve) m <<- solve
        
        #get the value of the inverse matrix
        getinv <- function() m
        
        #return a list with the get and set functions for the 
        #matrix and for the inverse matrix
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
        
        #check to see if inverse has already been found
        if(!is.null(m)) {
                # if yes...
                message("getting cached data")
                return(m)
        }
        # if no, calculate it
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

##Example

#> x<-matrix(1:4,2,2)
#> x
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> xc<-makeCacheMatrix(x)
#> xc$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> xinv<-cacheSolve(xc)
#> xinv
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> xinv<-cacheSolve(xc)
#getting cached data
#> xinv
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#check that xinv really is the inverse of x
#> xinv %*% x
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1