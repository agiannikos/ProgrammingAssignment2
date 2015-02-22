## The "makeCacheMatrix" creates a special "matrix" that containing a function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the invesre
## 4. get the inverse


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get  <- function() x
        setsolve  <- function(inverse) s <<- inverse
        getsolve  <- function() s
        
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)

}


## This function calculates the inverse in does not exist or return it if this exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s  <- x$getsolve()
        if (!is.null(s)){
                message("geting cached data")
                return(s)
        }
        
        matrix <- x$get()
        s <- solve(matrix)
        x$setsolve(s)
        s
}
