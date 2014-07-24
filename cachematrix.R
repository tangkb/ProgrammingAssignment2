
matinv<-function(x){solve(x)%*%x} #basically same as mean example, only change mean to matrix inverse function
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	

        setmatinv <- function(matinv) m <<- matinv
        getmatinv <- function() m
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


cacheSolve <- function(x, ...) {
        m <- x$getmatinv()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matinv(data, ...)
        x$setmatinv(m)
        m
}
