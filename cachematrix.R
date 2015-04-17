## R Programming course
## Week 2 programming assignment
## Using R scoping rules to preserve state inside an R object


## ---------------- code used to test of the functionality
#
#   Test data and results without caching
#
#       mtx <- matrix(rnorm(9, 5, 2), nrow=3, ncol=3)
#       mtx2 <- matrix(rnorm(16, 5, 2), nrow=4, ncol=4)
#       solve(mtx)
#       solve(mtx2)
#
#
#   Build the object my_mtx and call the function twice to check that the 2nd time the message is shown
#
#       my_mtx <- makeCacheMatrix(mtx)
#       cacheSolve(my_mtx)
#       cacheSolve(my_mtx)
#
#   With the same object my_mtx, store a new matrix (4 x 4 this time), and invoke cachedSolve twice
#
#       my_mtx$set(mtx2)
#       cacheSolve(my_mtx)
#       cacheSolve(my_mtx)


## makeCacheMatrix returns a list with four function definitios
##    set(y):           preserves the value of the matrix y into makeCacheMatrix::x
##    get():            retrieves the value of the matrix preserved in makeCacheMatrix::x
##    setinverse(slv):  preserves the value of the inverse(x) into makeCacheMatrix::cachedSolve
##    getinverse():     retrieves the value of the inverse preserved in mkaeCacheMatrix::cachedSolve
##
makeCacheMatrix <- function(x = matrix()) {
    
    cachedSolve <- NULL      # when makeCacheMatrix is invoked, cachedSolve varialble is set to NULL (cache is empty)
    
    # sets the value of the matrix
    set <- function(y) {
        x <<- y              # the value of makeCacheMatrix::x is replaced by a nue matrix
        cachedSolve <<- NULL # with the new matrix, the previous computation of the inverse is not valid anymore
    }
    
    get <- function() x      # returns makeCacheMatrix::x (the matrix)
    
    setinverse <- function(slv) cachedSolve <<- slv # preserves the inverse of the matrix (slv)
    getinverse <- function() cachedSolve            # retrieves the cached inverse (cachedSolve)
        
    list (set = set,  
          get = get,  
          setinverse = setinverse, 
          getinverse = getinverse) 
}




## Returns the inverse of a matrix x
##    The first time the function is executed, the inverse of x is calculated and the result is preserved
##    Subsequent calls to cacheSolve don't performe the computation, but just retrieve the cached inverse
##
cacheSolve <- function(x, ...) {
    
    s <- x$getinverse()     # retrieves makeCachedMatrix::cachedSolve, which is NULL the first invocation
    if (is.null(s)) {       # the first call s is NULL because the inverse has not been calculated yet
        data <- x$get()             # First we need to get the matrix x preserved in makeCacheMatrix::x 
        inv <- solve(data, ...)     # Then we calculate the inverse of x
        x$setinverse(inv)           # Fnally the inverse is preserved into makeCacheMatrix::cachedSolve for subsequent calls
        inv                         # this line is not needed, but added for clarity
    }
    else                    # getinverse didn't return TRUE means the inverse was already computed and preserved
    {
        message("getting cached data")
        s                       # returns the inverse preserved in makeCachedMatrix::cachedSolve
    }
}




