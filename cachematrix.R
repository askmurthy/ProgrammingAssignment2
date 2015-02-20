## Matrix inversion is a rutime expensive operation. The most efficient methods such 
## LU decomposition is of the O(n^2) for a nxn square matrix. So, for large n the 
## runtime increase is quadratic. The following functions help cache the "inverse" of
## if a matrix once it is computed. Future calls to get the inversion of the same matrix
## wil return the cached value. The functions can be easily extended to cache a vector of
## matrices and its corresponding cached inverses


## makeCacheMatrix implements the methods for caching an input matrix and its inverse.
## Given an input matrix, makeCacheMatrix returns the following list of functions:
## set() - storesinput matrix if it is different from current matrix
## get() - returns current matrix
## setinv() - stores the inverse of current matrix
## getinv() - returns the inverse of current matrix

makeCacheMatrix <- function(x = matrix()) {
    myinv <- NULL
    set <- function(y) {
        if (!identical (x, y)) {
           x <<- y
           myinv <<- NULL
        }
    }
    get <- function() x
    setinv <- function(inv) myinv <<- inv
    getinv <- function() myinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve is used to compute the inverse of a matrix
## the function takes a list of functions 
## consisting of accesors to a matrix and its cached inverse as its argument.
## if the inverse is unavailable from the cache (is null), it will compute the inverse
## and store it in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")  
        return (inv)
    }
    data <- x$get()
    inv <- solve (data)
    x$setinv (inv)
    inv
}


## test cacheSolve
## first test with a random 1000x1000 matrix
## next create new matrix and retest. 
## The inverse of the 2nd matrix should be different from that of the first

testCache <- function () {
  
    # generate matrix, and the inverse of the matrix.
    size <- 1000
    mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
    inv0 <- solve(mymatrix)
    
    # now solve the matrix via the cache-method
  
    cache <- makeCacheMatrix(mymatrix)
 
    start.time = Sys.time()
    # this should take long, since it's the first go
    inv1 <- cacheSolve(cache)
    t = Sys.time() - start.time
    print(t)
    
    # this should be lightning fast
    start.time = Sys.time()
    inv2 <- cacheSolve(cache)
    t = Sys.time() - start.time
    print(t)
    # check if all solved matrices are identical
    r0 <- identical(inv0, inv1) & identical(inv1, inv2)
    print(paste0("Results matching: ", r0))
    
    # next test case when matrix changes
    mymatrix2 <- matrix(rnorm(size^2), nrow=size, ncol=size)
    inv3 <- solve(mymatrix2)
    
    #Then modify the contents of the the special matrix using the 'set' function
    cache$set(mymatrix2)
    
    #Now use cacheSolve. 
    #You should NOT see 'getting cached data' in your console
    start.time = Sys.time()
    # this should take long, since it's the first go
    inv4 <- cacheSolve(cache)
    t = Sys.time() - start.time
    print(t)
    r1 <- identical(inv0, inv4)
    print(paste0("Results different between inverse for 1st and 2nd matrices: ", !r1))
    
    # this should be lightning fast
    start.time = Sys.time()
    inv5 <- cacheSolve(cache)
    t = Sys.time() - start.time
    print(t)
    # check if all solved matrices are identical
    r2 <- identical(inv3, inv4) & identical(inv4, inv5)
    print(paste0("Results matching: ", r2))
    
       
}