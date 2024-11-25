## Функция makeCacheMatrix создаёт специальную "матрицу", 
## которая хранит саму матрицу и её обратную версию.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # Переменная для хранения обратной матрицы
    set <- function(y) {  # Устанавливает новую матрицу
        x <<- y  # Сохраняет матрицу
        m <<- NULL  # Сбрасывает кэш обратной матрицы
    }
    get <- function() x  # Возвращает исходную матрицу
    setinverse <- function(inverse) m <<- inverse  # Устанавливает обратную матрицу
    getinverse <- function() m  # Возвращает обратную матрицу из кэша
    list(set = set, get = get,  # Возвращает список с функциями
         setinverse = setinverse,
         getinverse = getinverse)
}

## Функция cacheSolve вычисляет обратную матрицу.
## Если обратная матрица уже вычислена и сохранена в кэше, она возвращает её.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()  # Получаем значение обратной матрицы из кэша
    if(!is.null(m)) {  # Если обратная матрица уже вычислена
        message("получение данных из кэша")  # Сообщаем, что используем кэш
        return(m)  # Возвращаем обратную матрицу из кэша
    }
    data <- x$get()  # Получаем исходную матрицу
    m <- solve(data, ...)  # Вычисляем обратную матрицу
    x$setinverse(m)  # Сохраняем обратную матрицу в кэше
    m  # Возвращаем обратную матрицу
}

## Тестовые примеры:

# Создаём матрицу и кэшируем её
a <- makeCacheMatrix(matrix(1:4, 2, 2))
a$get()  # Выводит исходную матрицу
cacheSolve(a)  # Вычисляет и выводит обратную матрицу

# Ещё одна матрица для тестирования
b <- makeCacheMatrix(matrix(4:8, 2, 2))
b$get()  # Выводит исходную матрицу
cacheSolve(b)  # Вычисляет и выводит обратную матрицу

# Проверка с другой матрицей
c <- matrix(c(1, -0.25, -0.25, 1), nrow = 2, ncol = 2, byrow = TRUE)
ci <- cacheSolve(makeCacheMatrix(c))
print(ci)  # Выводим результат

# Ещё один тест
d <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2, byrow = TRUE)
di <- cacheSolve(makeCacheMatrix(d))
print(di)

# Проверка получения данных из кэша
am <- makeCacheMatrix(a$get())
ai <- cacheSolve(am)  # Вычисляем обратную матрицу
ai2 <- cacheSolve(am)  # Повторный вызов, должен взять из кэша

# Проверка произведения матриц (должно быть единичной матрицей)
print(ai %*% a$get())
print(a$get() %*% ai2)
