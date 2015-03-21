

## Creates a special "matrix" which stores the matrix and its inverse (after using cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL                       ## m is the inverse
    get <-function() x
    set <-function(y) {        ## sets the inverse to null if the set function was called
        x<<-y                    ## over the matrix (the matrix has been changed)
        m<<-NULL
    }
    getinverse <- function() {    ## returns the cached inverse which is m
      return(m)
    }
    setinverse <- function(inverse) {   ## sets the inverse (from cacheSolve by default)
       m<<-inverse
    }
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)  ## returns a special "matrix"
}


## Returns inverse of the matrix by getting it from cache or by calculating it if the
## inverse hasn't been calculated yet.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m))                    ## case when the inverse has been already calculated
            return(m)
        
        data<-x$get()
        m<-solve(data)                       ## calculate the inverse
        x$setinverse(m)                 ## set the inverse to the cache
        return(m)
}
