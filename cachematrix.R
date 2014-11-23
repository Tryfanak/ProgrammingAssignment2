## These functions calculate the inverse of a matrix, cache it, and retrieve it
## if the inverse of the same matrix is requested again
##   makeCacheMatrix - Creates matrix object and get/set methods
##   cacheSolve - Calculates and caches inverse, or retrieves cached inverse

## makeCacheMatrix - Function to create matrix object and get/set methods

makeCacheMatrix <- function(xMat = matrix()) {
    cachedInv <- NULL  # Will be used to store cached inverse
    
    # Create set function 
    #   Purpose   - Changes input matrix for an array object that already exists
    #   Arguments - New input matrix
    #   Returns -   not used
    set <- function (yMat) {
        xMat <<- yMat
        cachedInv <<- NULL
    }

    # Create get function 
    #   Purpose   - Retrieve input matrix for makeCacheMatrix object
    #   Arguments - None
    #   Returns -   Matrix passed as an argument to makeCacheMatrix when it was
    #               called to create the object with this function as a method
    get <- function () { xMat }  
    
    # Create setmatinv function 
    #   Purpose   - Cache calculated inverse matrix for object (superassigment)
    #   Arguments - Inverse matrix to be stored (Really any data type)
    #   Returns -   not used
    setmatinv <- function(xMatInv) {
        cachedInv <<- xMatInv
    }
   
    # Create getmatinv function 
    #   Purpose   - Retrieve cached inverse matrix for object
    #   Arguments - None
    #   Returns -   Cached inverse matrix (saved by earlier cal to setmatinv)
    getmatinv <- function() {
        cachedInv
    }
    
    list( set=set, 
          get = get, 
          setmatinv = setmatinv,
          getmatinv = getmatinv )
}


## Write a short comment describing this function

cacheSolve <- function(xObj, ...) {
    matInv <- xObj$getmatinv()
    if (!is.null(matInv)) {
        message("Found cached inverse")
        return(matInv)
    }
    matRaw <- xObj$get()
    matInv <- solve(matRaw)
    xObj$setmatinv(matInv)
    matInv
    
}
