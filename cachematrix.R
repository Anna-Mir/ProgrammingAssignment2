## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {            ## if user want to reset matrix 
        x <<- y                     ## reassign "new" matrix to x 
        m <<- NULL                  ## reinitialize m to NULL
    }
    get <- function() x
    setInvmatrix <- function(InvMatrix) m <<- InvMatrix
    getInvmatrix <- function() m
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)

}
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getInvmatrix()              
    if(!is.null(m)) {           
        return(m)               
    }
    data <- x$get()             
    m <- solve(data, ...)       
    x$setInvmatrix(m)           
    m                            
}

