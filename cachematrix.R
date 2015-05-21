## R source code to perform squared matrix inversion with caching
## CPU consuming operations are minimized (i.e. matrix inversion)
## Information from first matrix inversion is cached and reused
## NB: Package assumes input is square invertible matrix

## Function built to associate to input matrix additional info, in
## particular value of inverse matrix (by defaul NULL)
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInvM <- function(invMatrix) invM <<- invMatrix
    getInvM <- function() invM
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


## Smart matrix inverter. Inversion computation happens only
## if not already computed and stored in memory
cacheSolve <- function(x, ...) {
    tempInv <- x$getInvM()
    ## Check if inverse matrix already in memory, if yes return it
    if(!is.null(tempInv)) {
        message("getting cached data")
        return(tempInv)
    }
    dataM <- x$get()
    ## If inverse is NULL, compute it with R built in function
    tempInv <- solve(dataM)
    x$setInvM(tempInv)
    return(tempInv)
}
