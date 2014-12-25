makeVector <- function(x=numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y ## set x in makeVector to y
        m <<- NULL ## set m in makeVector to NULL 
    }
    get <- function() x ## return the value x
    setmean <- function(mean) m <<- mean ## set m in makeVector to mean
    getmean <- function() m ## return the value of m
    list(set=set, get=get,
         setmean=setmean,
         getmean=getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get() ## get the value of X
    m <- mean(data, ...) ## calculate the mean
    x$setmean(m) ## use setmean to set m to the mean in makeVector
    m
}