## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    MatRaw <- x  # Make my on copy of the input matrix, with a memorable name
    cachedMatInv <- NULL  # Will be used to store cached inverse
    
    # Create get function 
    #   Purpose   - Retrieve input matrix for makeCacheMatrix object
    #   Arguments - None
    #   Returns -   Matrix passed as an argument to makeCacheMatrix when it was
    #               called to create the object with this function as a method
    get <- function () { MatRaw }  
    set <- function () { MatRaw }  
    
    # Create setmatinv function 
    #   Purpose   - Cache calculated inverse matrix for object (superassigment)
    #   Arguments - Inverse matrix to be stored (Really any data type)
    #   Returns -   not used
    setmatinv <- function(xMatInv) {
        cachedMatInv <<- xMatInv
    }
   
    # Create getmatinv function 
    #   Purpose   - Retrieve cached inverse matrix for object
    #   Arguments - None
    #   Returns -   Cached inverse matrix (saved by earlier cal to setmatinv)
    getmatinv <- function() {
        cachedMatInv
    }
    
    list( set=set, 
          get = get, 
          setmatinv = setmatinv,
          getmatinv = getmatinv )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    matInv <- x$getmatinv()
    if (!is.null(matInv)) {
        message("getting cached inverse")
        return(matInv)
    }
    matRaw <- x$get()
    matInv <- solve(matRaw)
    x$setmatinv(matInv)
    matInv
    
}
