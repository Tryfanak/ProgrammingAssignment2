# Functions to calculate the inverse of a matrix, cache it, and retrieve it
#  if the inverse of the same matrix is requested again
#     Functions - makeCacheMatrix, cacheSolve

# Function makeCacheMatrix - Create matrix object and get/set methods
#   Arguments - Matrix to turn into a matrix object
#   Returns -   List of methods for matrix object (functions)

makeCacheMatrix <- function(xMat = matrix()) {
    cachedInv <- NULL  # Will be used to store cached inverse
    
    # Function set - Change value of input matrix for an existing matrix object
    #   Arguments - New input matrix
    #   Returns -   not used
    set <- function (yMat) {
        xMat <<- yMat
        cachedInv <<- NULL
    }

    # Function get - Retrieve input matrix for matrix object
    #   Arguments - None
    #   Returns -   Matrix passed as an argument to makeCacheMatrix when it was
    #                called to create the object with this function as a method
    get <- function () { xMat }  
    
    # Function setmatinv - Cache calculated inverse matrix (superassigment)
    #   Arguments - Inverse matrix to be stored (Really any data type)
    #   Returns -   not used
    setmatinv <- function(xMatInv) {
        cachedInv <<- xMatInv
    }
   
    # Function getmatinv - Retrieve cached inverse matrix for object
    #   Arguments - None
    #   Returns -   Cached inverse matrix (saved by earlier call to setmatinv)
    getmatinv <- function() {
        cachedInv
    }
    
    # Return a list with the four methods that have been created 
    list( set=set, 
          get = get, 
          setmatinv = setmatinv,
          getmatinv = getmatinv )
}


# makeCacheMatrix - Calculates/caches inverse, or retrieves cached inverse
#   Arguments - Matrix object (from makeCacheMatrix) to invert
#   Returns -   Inverse matrix

cacheSolve <- function(xObj, ...) {
    # Check if there is a cached inverse for object
    matInv <- xObj$getmatinv()
    if (!is.null(matInv)) {
        message("Found cached inverse")
    }
    
    # If not, retrieve matrix for this object, invert it and cache the result
    else {
        matRaw <- xObj$get()
        matInv <- solve(matRaw)
        xObj$setmatinv(matInv)
    }
    
    # Return the inverse matrix
    matInv
}
