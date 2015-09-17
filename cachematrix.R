## This pair of functions allows for the caching of a matrix and its
## inverse.  makeCacheMatrix generates the list/object that contains 
## the matrix values and functions.  cacheSolve lets you interact with
## the cached matrix


## This function creates a list of functions that operate as a 
## sort of object- they store the values of the matrix and inverse 
## in the global environment with the <<- operator and allow
## getting and setting the matrix and its inverse.
##
## Parameters
## ----------
## x : matrix (optional)
##     The matrix you want to store the inverse of
##
## Returns
## -------
## cacheMatrix : list
##     This is a list of four functions- get(), which returns the
##     matrix; set(), which changes the matrix and resets the inverse;
##     get_inverse(), which returns the current value of the inverse;
##     and set_inverse(), which stores the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    # Make sure that there aren't any guys in the local environment
    # with the name that we want for the inverse
    x_inv <- NULL
    
    #Function to set a new value for the cached matrix
    set <- function(new_x){
        #Set the x in the global environment
        x <<- new_x
        #Reset the inverse because we don't want any old matrices'
        #inverses sticking around
        x_inv <<- NULL
    }
    
    #Function to return the current matrix stored
    get <- function() x
    
    #Function to store the value of the inverse
    set_inverse <- function(new_x_inv){
        x_inv <<- new_x_inv
    }
    
    #Function to return the stored value for the matrix's inverse
    get_inverse <- function() x_inv
    
    list(set=set, get=get, set_inverse=set_inverse,
         get_inverse=get_inverse)
}


## This function either calculates or retrieves the inverse
## depending on whether or not it already exists
## 
## Parameters
## ----------
## x : cacheMatrix
##     The cacheMatrix you want to interact with
##
## Returns
## -------
## x_inv : matrix
##     The inverse of the matrix cached
cacheSolve <- function(x, ...) {
    ## Grab the value that we currently have as the inverse
    current_x_inv <- x$get_inverse()
    
    #Do we have a non-null inverse?
    if(!is.null(current_x_inv)){
        #If so, go ahead and return that value and let them know we
        #didn't recalculate
        message("retrieved cached data")
        return(current_x_inv)
    }
    
    #If we got past that, we have a null inverse currently.
    #Calculate and store the inverse.
    this_matrix <- x$get()
    this_inverse <- solve(this_matrix, ...)
    x$set_inverse(this_inverse)
    
    #Return the newly calculated inverse
    this_inverse
}
