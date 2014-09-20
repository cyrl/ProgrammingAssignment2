## Create a special matrix object and compute the inverse of that matrix, assuming that the 
## matrix supplied is always invertible. This implementation uses lexical 
## scoping to cache the inverse of the matrix.

## This fct creates and returns a special "matrix" object with 4 methods (set, 
## get, set_inverse, get_inverse) and 2 attributes (m_x, m_inverse).
makeCacheMatrix <- function(m_x = matrix()) {
        m_inverse <- NULL
        # Method to set the content of the matrix
        set <- function(y) {
                m_x <<- y
                # Clear the cache since the content of the matrix has changed.
                m_inverse <<- NULL
        }
        # Method to get the content of the matrix
        get <- function() m_x
        # Method to set the inverse of the matrix
        set_inverse <- function(inverse) m_inverse <<- inverse
        # Method to get the inverse of the matrix
        get_inverse <- function() m_inverse
        list(set = set, get = get, 
             set_inverse = set_inverse, get_inverse = get_inverse)
}


## This fct returns the inverse of the special "matrix" that was created by 
## makeCacheMatrix().
cacheSolve <- function(l_x, ...) {
        # Get the cache content.
        m_inverse <- l_x$get_inverse()
        # Check if the inverse has already been computed
        if(!is.null(m_inverse)) {
                # The inverse has already been computed and cached, return it.
                return(m_inverse)
        }
        # Compute the inverse and cache it
        l_x$set_inverse(solve(l_x$get()))
        ## Return a matrix that is the inverse of l_x
        return(l_x$get_inverse())
}
