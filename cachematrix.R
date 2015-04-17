## R Programming course
## Week 2 programming assignment
## Using R scoping rules to preserve state inside an R object

## makeCacheMatrix returns a list with four function definitios
##    set the value of the matrix y into makeCacheMatrix::x
##    get the value of the matrix makeCacheMatrix::x
##    set the value of the inverse makeCacheMatrix::s
##    get the value of the inverse mkaeCacheMatrix::s


makeCacheMatrix <- function(x = matrix()) {
    # when the matrix is created, the cachedSolve varialble is null
    cachedSolve <- NULL
    
    # sets the value of the matrix
    set <- function(y) {
        x <<- y              # the value of makeCacheMatrix::x is replaced by a nue matrix
        cachedSolve <<- NULL # the previous computation of the inverse is not valid anymore with the new matrix
    }
    
    # gets the value of the array
    get <- function() x # returns makeCacheMatrix::x (contains the matrix)
    
    setinverse <- function(slv) cachedSolve <<- slv # recieves the inversed matix and stores into cachedSolve
    getinverse <- function() cachedSolve            # returns the value of cachedSolve
        
    list (set = set,  # f(y) -> makeCacheMatrix::x <- y
          get = get,  # f() -> makeCachMatrix::x
          setinverse = setinverse, # f(slv) -> makeCacheMatrix::cachedSolve <- slv
          getinverse = getinverse) # f() -> cachedSolve
}




## Returns the inverse of a matrix x; after the first computation, the result is chaced and returned in subsquent calls

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse() # value of the inverse(x) stored in makeCachedMatrix::cachedSolve (null the first time)
    if (is.null(s)) {   # the first call, the inverse has not been calculated and the function returns NULL
        data <- x$get()    # value of the matrix x stored in makeCacheMatrix::x 
        inv <- solve(data, ...) # calculate the inverse(x) 
        x$setinverse(inv)    # store the inverse into makeCacheMatrix::cachedSolve for subsequent calls
        inv # this line is not needed, just added for clarity
    }
    else # not the first time, the inverse(x) is already computed and stored into s
    {
        message("getting cached data")
        s # returns the matrix stored in makeCachedMatrix::cachedSolve
    }
    
}



## test data
# mtx <- matrix(rnorm(9, 5, 2), nrow=3, ncol=3)
# mtx2 <- matrix(rnorm(16, 5, 2), nrow=4, ncol=4)
# solve(mtx)
# solve(mtx2)
# my_mtx <- makeCacheMatrix(mtx)
# cacheSolve(my_mtx)
# cacheSolve(my_mtx)
# my_mtx$set(mtx2)
# cacheSolve(my_mtx)
# cacheSolve(my_mtx)
