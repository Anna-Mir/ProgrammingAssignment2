makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {            
        x <<- y                      
        m <<- NULL                  
    }
    get <- function() x
    setInvmatrix <- function(InvMatrix) m <<- InvMatrix
    getInvmatrix <- function() m    
	list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    n <- x$getInvmatrix()              
    if(!is.null(m)) {           
        return(m)               
    }
    data <- x$get()             
    m <- solve(data, ...)       
    x$setInvmatrix(n)           
    m                            
}
