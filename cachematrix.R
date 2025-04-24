## 计算逆矩阵并缓存，再次调用时直接从缓存中获取

## 将矩阵存入闭包函数，后续调用

makeCacheMatrix <- function(x=numeric()){
    ## 初始化m
    m <- NULL
    
    ## 初始化函数，若后续要更改矩阵的值，由于x为
    ## 闭包函数中的变量，需要利用闭包中的函数修改
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## 获取矩阵的值
    get <- function() x
    
    ## 将逆矩阵存入缓存
    setsolve <- function(solve) m <<- solve
    
    ## 获取缓存中的值
    getsolve <- function() m
    
    ## 将函数以列表形式输出，方便后续调用
    list(set = set,get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## 检验逆矩阵是否计算过，计算逆矩阵

cachesolve <- function(x, ...) {
    
    ## 从缓存中获取，检查是否空
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## 将makeCacheMatrix()中的矩阵获取到data中
    ## 利用x$get()可调用get()及其父函数中的变量
    data <- x$get()
    
    ## 计算逆矩阵
    m <- solve(data, ...)
    
    ## 利用父函数，将逆矩阵存入缓存
    x$setsolve(m)
    m
}
