## Cache Matrix function

## This function will create a matrix that can cache its inverse in order to avoid costly computation

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
                get <- function() x
                setA <- function(inverse) a <<- inverse
                getA <- function() a
                list(set = set, get = get,
                setA = setA,
                getA = getA)

}


## This function will solve for the inverse matrix defined in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        a <- x$getA()
        if (!is.null(a)) {
                message("fetching cached data")
                return(a)
        }
                matrix <- x$get()
                a <- solve(matrix, ...)
                x$setA(a)
                a
}
