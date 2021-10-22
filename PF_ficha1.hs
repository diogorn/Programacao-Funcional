import Data.Char
-- 1
--   length l: o numero de elementos da lista l
--   head l: a cabeca da lista (nao vazia) l
--   tail l: a cauda da lista (nao vazia) l
--   last l: o ultimo elemento da lista (nao vazia) l 
--   sqrt x: a raiz quadrada de x
--   div x y: a divisao inteira de x por y
--   mod x y: o resto da divisao inteira de x por y

-- a)
perimetro :: Float -> Float
perimetro r = 2 * pi * r
-- b)
dist :: (Float,Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
-- c)
primUlt :: [a] -> (a,a)
primUlt (x:xs) = (x,last(xs))
-- d)
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0
-- e)
truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar (x:xs) = if mod ((length (x:xs))) 2 == 0 then (x:xs) else xs
-- f)
max2 :: Int -> Int -> Int
max2 x1 x2 | x1 > x2 = x1
           | otherwise = x2
-- g)
max3 :: Int -> Int -> Int -> Int
max3 x1 x2 x3 = max2 x1 (max2 x2 x3)

-- 2
-- a)
nRaizes :: Float -> Float ->Float ->Int
nRaizes a b c | delta > 0 = 2
              | delta == 0 = 1
              | otherwise = 0
              where delta = sqrt(b^2-4*a*c)
-- b)
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | delta /= 0 = []
             | otherwise = [(-b-delta)/2*a,(-b+delta)/2*a]
             where delta = sqrt(b^2-4*a*c)

-- 3
type Hora = (Int, Int)
-- a)
horaValida :: Hora -> Bool
horaValida (h,m) = h >= 0 && h<24 && m>=0 && m<60
-- b)
horaDepoisDeOutra :: Hora -> Hora -> Bool
horaDepoisDeOutra (h1,m1) (h2,m2) | h1 > h2 = True
                                  | h1 == h2 && m1 > m2 = True
                                  | otherwise = False
-- c)
horasToMinutos :: Hora -> Int
horasToMinutos (h,m) = h*60 + m
-- d)
minutosToHoras :: Int -> Hora
minutosToHoras x = (div x 60, mod x 60)
-- e)
diferencaDeHoras :: Hora -> Hora -> Int
diferencaDeHoras (h1,m1) (h2,m2) = horasToMinutos(h2 - h1, m2 - m1) 
-- f)
somaMinToHoras :: Int -> Hora -> Hora
somaMinToHoras m1 (h2,m2) | m1 + m2 > 59 = (h2 + div (m1+m2) 60, (mod (m1+m2) 60))
                          | otherwise = (h2, m1+m2)

-- 4
data Hora2 = H Int Int deriving (Show,Eq)

-- a)
horaValida2 :: Hora2 -> Bool
horaValida2 (H h m) = h >= 0 && h<24 && m>=0 && m<60
-- b)
horaDepoisDeOutra2 :: Hora2 -> Hora2 -> Bool
horaDepoisDeOutra2 (H h1 m1) (H h2 m2) | h1 > h2 = True
                                       | h1 == h2 && m1 > m2 =False
                                       | otherwise = False
-- c)
horasToMinutos2 :: Hora2 -> Int
horasToMinutos2 (H h m) = h*60 + m
-- d)
minutosToHoras2 :: Int -> Hora2
minutosToHoras2 x | x<60 = H 0 x
                  | otherwise = (H (div x 60) (mod x 60))
-- e) 
diferencaDeHoras2 :: Hora2 -> Hora2 -> Int
diferencaDeHoras2 (H h1 m1) (H h2 m2) = horasToMinutos2(H (h2-h1) (m2-m1) )
-- f)
somaMinToHoras2 :: Int -> Hora2 -> Hora2
somaMinToHoras2 m1 (H h2 m2) = ( H (h2 + div (m1+m2) 60) (mod (m1+m2) 60))

-- 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)
-- a)
next :: Semaforo -> Semaforo
next x = case x of
         Verde -> Amarelo
         Amarelo -> Vermelho
         Vermelho -> Verde
-- b)
stop ::  Semaforo -> Bool
stop s = s == Vermelho 
-- c)
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = (s1 == Vermelho) || (s2==Vermelho)
-- 6 
data Ponto = Cartesiano Double Double | Polar Double Double
     deriving (Show, Eq)
-- a) 
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a) = d * cos a
-- b)
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d * sin a
-- c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar d a ) = d 
-- d)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar d a) = a 
-- e)
dist6 :: Ponto -> Ponto -> Double
dist6 (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

-- 7
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show, Eq)

-- a)
poligono :: Figura -> Bool
poligono fig = case fig of 
    (Circulo _ _) -> False
    otherwise -> True

-- b)
vertices :: Figura -> [Ponto]
vertices vrtcs = case vrtcs of 
    (Rectangulo (Cartesiano x1 y1) ( Cartesiano x2 y2) ) -> [(Cartesiano x1 y1), (Cartesiano x2 y2)]
    (Triangulo (Cartesiano x1 y1) ( Cartesiano x2 y2) (Cartesiano x3 y3)) -> [(Cartesiano x1 y1), ( Cartesiano x2 y2), (Cartesiano x3 y3)]
    otherwise -> []
-- c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist6 p1 p2 
        b = dist6 p2 p3
        c = dist6 p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p1 p2) = abs (posy p2 - posy p1) * abs (posx p2 - posx p1)
area (Circulo c r) = pi * r ^ 2
-- d)
perimetro7 :: Figura -> Double
perimetro7 (Circulo c r) = 2*pi*r
perimetro7 (Rectangulo p1 p2) = abs (posy p2 - posy p1) *2 + abs (posx p2 - posx p1) *2
perimetro7 (Triangulo p1 p2 p3) = (dist6 p1 p2) + (dist6 p2 p3) + (dist6 p1 p3) -- está errado

-- 8
-- a)
myisLower :: Char -> Bool
myisLower car = ord car >= 97 && ord car <= 122
-- b)
myisDigit :: Char -> Bool
myisDigit car = ord car >= 48 && ord car <= 57
-- c)
myisAlpha :: Char -> Bool
myisAlpha car = ord car >=65 && ord car <=90 || myisLower car
-- d)
mytoUpper :: Char -> Char
mytoUpper car = chr ((ord car) - 32) 
-- e)
myisToDigit :: Int -> Char
myisToDigit n = chr (n + 48)
-- f)
mydigitToInt :: Char -> Int
mydigitToInt c = ord (c) - 48





