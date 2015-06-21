## This function should delight and amaze all that witness it and it's outpouring of wisdom.
##  
## At a minimum, it should should check and see if the matrix inversion has already been completed.
## If it has, use the cached value due to the costly nature of inverting a matrix.
## If it hasn't been already inverted and cached, it should invert it, cache it, and return it.
## This should save the next poor redneck trying to invert it a bit of processing time.
## 




## Creates a group of functions to get and set an inverteible matrix.
##  Then a group of functions to set and / or get it, as needed.    We'll see how it goes.

makeCacheMatrix <- function(x = matrix()) {
        
# Starting clean
        invrs = NULL
        
        
# Setting up shop
        set = function (y) {
        x <<- (y)  ## Using the '<<-' to indicate a different environment from current one.
        
        invrs <<-  NULL }  # Keeping clean              
        
                      get = function() x
                setinvrs = function(inverse) invrs <<- inverse
                getinvrs = function() invrs
                list(set=set, get=get, setinvrs=setinvrs, getinvrs=getinvrs)
                
                
       }

## Moment of truth, herein our humble redneck will attempt to astound and amaze
## by inverting the Matrix.....Neo beware......

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invrs = x$getinvrs()
        
        ##Being a lazy redneck, making sure I really have to do all this figurin' 
        ##  and can't just grab it from previous run
        
        if (!is.null(invrs)) {
                ##WHOOHOO!  No more figurin'  gettin' tha cache$$!
                message("Getting cached data")
                return(invrs)
        }
        
        ##Well dad-nabbit,  gonna have to figure some more.
        mat.data = x$get()
        invrs = solve(mat.data, ...)
        
        ##Placing in cache so I don't have to do this AGAIN!
        x$setinvrs(invrs)
        
        ##Spit it back out 
        return(invrs)
        
}
