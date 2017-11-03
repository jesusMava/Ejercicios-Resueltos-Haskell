mul x = \y -> x*y
--
--Ir agregando las funciones que ocupamos
--Relacion ejercicios practica  

factorialR :: (Integral a) => a -> a
factorialR 0 = 1
factorialR n = n * factorialR (n - 1)

--ejercicio 1 --``
comb n k = factorialR n `div` (factorialR k * factorialR (n-k)) 
--Ejercicio 2 
--map'::(Integral a)
map' x [] = [] --cuando ocupas metodo de parametros es importante que definas los errores
--de otra manera el programa te marcara error
map' x (y:ys) = x (y):map' x (ys) 

--filter p l
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (l:ls) 
	| p (l)= l:filter' p (ls)  
	|otherwise = filter' p (ls)

--ejercicio 4 suma
sum' [] = 0
sum' (n:ns) = n+sum' (ns) 

--Ejercicio 5
foldr' _ num [] = num 
foldr' (+) num (x:xs) = x +foldr' (+) num (xs)
foldr' (-) num (x:xs) = x +foldr' (-) num (xs)
foldr' (/) num (x:xs) = x +foldr' (/) num (xs) 
foldr' (*) num (x:xs) = x +foldr' (*) num (xs)

--ejercicio 6
--until' _ 0 _=0
until' (term) (num) 0 = 0
until' (term) (num) x 
	|(term)x=x
	|otherwise= until' (term) (num) ((num)x)
--ejercicio 7
divisible x 0= error "¡Hey, no puedes Dividir entre Zero!"
divisible x y = x `mod` y==0 
--ejercicio 8
--calculaMod x n
--	|x `mod` n ==0 = n:divisores x
--	|otherwise = divisores x

--divisores x = calculaMod x 1

--divisores 0 = []
sumS _ [] = []
sumS x (y:ys)
	|x `mod` y == 0 =y:sumS x (ys)
	|otherwise = sumS x (ys)

divisores x = sumS x y
	where y =[1..x]
	