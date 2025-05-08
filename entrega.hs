module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId x = [x]

procCola :: Procesador [a] a
procCola [] = []
procCola (x:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose r hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern r hijo1 hijo2 hijo3) = [hijo1,hijo2,hijo3]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo raiz nodos) = [raiz]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo raiz nodos) = nodos


--Ejercicio 2
--foldAT
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT f z Nil = z
foldAT f z (Tern r h1 h2 h3) = f r (foldAT f z h1) (foldAT f z h2) (foldAT f z h3)

--foldRose
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose raiz hijos) = f raiz (map (foldRose f) hijos)

--foldTrie
mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f [] = []
mapSnd f ((x,y):xs) = (x, f y):mapSnd f xs

foldTrie :: (Maybe a -> [(Char,b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo nodo ramas) = f nodo (mapSnd (foldTrie f) ramas)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (\x -> [x])

sufijos :: Procesador [a] [a]
sufijos = foldr (\x acc -> (x:head acc) :  acc ) [[]]


--Ejercicio 4
--preorder :: undefined
preorder :: Procesador (AT a) a
preorder = foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) []

--inorder :: undefined
inorder :: Procesador (AT a) a
inorder = foldAT (\r h1 h2 h3 -> h1++h2++[r]++h3) []

--postorder :: undefined
postorder :: Procesador (AT a) a
postorder = foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) []

--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\r hijos -> [r] ++ concat hijos)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\r hijos -> if null hijos then [r] else concat hijos)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\r hijos -> if null hijos then [[r]] else map (r:) (concat hijos))

--Ejercicio 6
caminos :: Procesador (Trie a) [Char]
caminos = foldTrie (\nodo ramas -> "" : concatMap (\(c,xs) -> map (c:) xs) ramas)

--Ejercicio 7
palabras :: Procesador (Trie a) [Char]
palabras = foldTrie (\nodo ramas -> case nodo of
                                        Nothing -> concatMap (\(c,xs) -> map (c:) xs) ramas
                                        Just _ -> "" : concatMap (\(c,xs) -> map (c:) xs) ramas)


--Ejercicio 8
-- 8.a) 
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f prim seg c = if f c then prim c else seg c

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) prim seg a = prim a ++ seg a

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) prim seg a = concatMap prim (seg a)

-- Ejercicio 9
{- 
data AT a = Nil | Tern a (AT a) (AT a) (AT a)
Quiero probar que: ∀t::AT a.∀x::a.(elem x (preorder t) = elem x (postorder t))

preorder :: Procesador (AT a) a
{PE} preorder = foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) []
postorder :: Procesador (AT a) a
{PO} postorder = foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) []
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
{F0} foldAT f z Nil = z
{F1} foldAT f z (Tern r h1 h2 h3) = f r (foldAT f z h1) (foldAT f z h2) (foldAT f z h3)

Caso base: 
P(Nil) = ∀x::a.(elem x (preorder Nil) = elem x (postorder Nil))

elem x (preorder Nil) =PE elem x (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] Nil) =F0 elem x [] =E0 False
elem x (postorder Nil) =PO elem x (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] Nil) =F0 elem x [] =E0 False 

∀x::a.(False = False)
Se verifica el caso base.


Caso inductivo: ∀h::AT a.∀j::AT a.∀k::AT a.∀r'::a.((P(h)∧P(j)∧P(k)) => P(Tern r' h j k))
P(h) =  ∀x::a.(elem x (preorder h) = elem x (postorder h))
P(j) =  ∀x::a.(elem x (preorder j) = elem x (postorder j))
P(k) =  ∀x::a.(elem x (preorder k) = elem x (postorder k))

H.I.: (P(h)∧P(j)∧P(k))

preorder (Tern r' h j k) =PE foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] (Tern r' h j k)
                         =F1 (\r h1 h2 h3 -> [r]++h1++h2++h3) r' (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] h) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] j) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] k)
                         =β (\h1 h2 h3 -> [r']++h1++h2++h3) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] h) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] j) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] k)
                         =β (\h2 h3 -> [r']++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] h)++h2++h3) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] j) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] k)
                         =β (\h3 -> [r']++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] h)++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] j)++h3) (foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] k)
                         =β [r']++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] h)++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] j)++(foldAT (\r h1 h2 h3 -> [r]++h1++h2++h3) [] k)
                         =(PE x3) [r']++(preorder h)++(preorder j)++(preorder k)

postorder (Tern r' h j k) =PO foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] (Tern r' h j k)
                          =F1 (\r h1 h2 h3 -> h1++h2++h3++[r]) r' (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] h) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] j) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] k)
                          =β (\h1 h2 h3 -> h1++h2++h3++[r']) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] h) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] j) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] k)
                          =β (\h2 h3 -> (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] h)++h2++h3++[r']) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] j) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] k)
                          =β (\h3 -> (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] h)++(foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] j)++h3++[r']) (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] k)
                          =β (foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] h)++(foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] j)++(foldAT (\r h1 h2 h3 -> h1++h2++h3++[r]) [] k)++[r']
                          =(PO x3) (postorder h)++(postorder j)++(postorder k)++[r']

P(Tern r' h j k) = ∀x::a.(elem x ([r']++(preorder h)++(preorder j)++(preorder k)) = elem x ((postorder h)++(postorder j)++(postorder k)++[r']))


Propongo el lema: ∀xs::[a].∀ys::[a].∀e::a.(elem e (xs++ys) = elem e xs || elem e ys)
Para usar este lema lo pruebo por inducción estructural:
elem :: a -> [a] -> Bool
{E0} elem _ [] = False
{E1} elem e (x:xs) = e==x || elem e xs
(++) :: [a] -> [a] -> [a]
{C0} (++) [] ys = ys
{C1} (++) (x:xs) ys = x : (xs ++ ys)

Caso base: 
P([]) = ∀ys::[a].∀e::a.(elem e ([]++ys) = elem e [] || elem e ys)
      =(C0 y E0) ∀ys::[a].∀e::a.(elem e ys = False || elem e ys)
      =(por lógica) ∀ys::[a].∀e::a.(elem e ys = elem e ys)
Se verifica el caso base. 

Caso inductivo: ∀xs::[a].∀x::a.P(xs) => P(x:xs)
P(xs) = ∀ys::[a].∀e::a.(elem e (xs++ys) = elem e xs || elem e ys)

P(x:xs) = ∀ys::[a].∀e::a.(elem e ((x:xs)++ys) = elem e (x:xs) || elem e ys)

elem e ((x:xs)++ys) =C1 elem e (x:(xs++ys))
                    =E1 e==x || elem e (xs++ys)

Tengo dos casos: 
1) Si e==x
   elem e (x:(xs++ys)) = True 

   elem e (x:xs) || elem e ys =E1 e==x || elem e xs || elem e ys = True || elem e xs || elem e ys  =(por lógica) True 
   
2) Si e/=x
   elem e (x:(xs++ys)) = elem e (xs++ys)

   elem e (x:xs) || elem e ys =E1 e==x || elem e xs || elem e ys 
                              = False || elem e xs || elem e ys 
                              =(por lógica) elem e xs || elem e ys
                              =(H.I.) elem e (xs++ys)
 
Así queda demostrado que vale el lema: ∀xs::[a].∀ys::[a].∀e::a.(elem e (xs++ys) = elem e xs || elem e ys)

Y ahora lo uso para terminar la demostración del caso inductivo P(Tern r' h j k):
P(Tern r' h j k) = ∀x::a.(elem x ([r']++(preorder h)++(preorder j)++(preorder k)) = elem x ((postorder h)++(postorder j)++(postorder k)++[r']))

elem x ([r']++(preorder h)++(preorder j)++(preorder k)) =(por lema aplicado varias veces) elem x [r'] || elem x (preorder h) || elem x (preorder j) || elem x (preorder k)

elem x (postorder h)++(postorder j)++(postorder k)++[r']) =(por lema aplicado várias veces) elem x (postorder h) || elem x (postorder j) || elem x (postorder k) || elem x [r']
                                                          =(el ó lógico es asociativo) elem x [r'] || elem x (postorder h) || elem x (postorder j) || elem x (postorder k)
                                                          =(H.I.) elem x [r'] || elem x (preorder h) || elem x (preorder j) || elem x (preorder k))
          
-}


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

-- Algunas estructuras que se van a utilizar en los tests:
listaVaciaAT :: [AT Int]
listaVaciaAT = []

ejemploAT :: AT Int
ejemploAT = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

ejemploRose :: RoseTree Int
ejemploRose = Rose 1 [Rose 2 [Rose 3 [],Rose 4 [],Rose 5 []],Rose 6 [Rose 7 [],Rose 8 [],Rose 9 [],Rose 10 []],Rose 11 [Rose 12 [],Rose 13 []],Rose 14 [Rose 15 [],Rose 16 [],Rose 17 [],Rose 18 [],Rose 19 []],Rose 20 []]

ejemploTrie :: Trie [Char]
ejemploTrie = TrieNodo Nothing [('T', TrieNodo Nothing [('P', TrieNodo Nothing [('1', TrieNodo (Just "TP1") [])])]), ('P', TrieNodo Nothing [('L', TrieNodo Nothing [('P', TrieNodo (Just "PLP") [])])])]

-- Las siguientes funciones cantPalabras y palabrasDelTrie fueron pensadas para un trie cuyos nodos solo guardan un valor de tipo 'Just [Char]' cuando se llega al final de una palabra y en el ultimo nodo se guarda la misma palabras (los nodos intermedios valen Nothing)
cantPalabras :: Trie [Char] -> Int
cantPalabras = foldTrie (\nodo ramas -> case nodo of
                                            Nothing -> sum (map snd ramas)
                                            Just _ -> 1 + sum (map snd ramas))

palabrasDelTrie :: Trie [Char] -> [[Char]]
palabrasDelTrie = foldTrie (\nodo ramas -> case nodo of
                                              Nothing -> concatMap snd ramas
                                              Just palabra -> palabra:concatMap snd ramas)


testsEj1 = test [ -- Casos de test para el ejercicio 1
  procVacio (Tern 1 Nil Nil Nil) ~=? listaVaciaAT, 
  procVacio (Rose 'a' [Rose 'b' [Rose 'c' []]]) ~=? listaVaciaAT,

  procId (Tern 1 Nil Nil Nil) ~=? [Tern 1 Nil Nil Nil],
  procId (Rose 'a' [Rose 'b' [Rose 'c' []]]) ~=? [Rose 'a' [Rose 'b' [Rose 'c' []]]],

  procCola [] ~=? listaVaciaAT,
  procCola [Tern 1 Nil Nil Nil,Tern 2 Nil Nil Nil,Tern 3 Nil Nil Nil] ~=? [Tern 2 Nil Nil Nil,Tern 3 Nil Nil Nil],

  procHijosRose (Rose 'a' [Rose 'b' [], Rose 'c' [], Rose 'e' []]) ~=? [Rose 'b' [], Rose 'c' [], Rose 'e' []],
  procHijosRose (Rose 1 []) ~=? [],

  procHijosAT Nil ~=? listaVaciaAT,
  procHijosAT (Tern 'a' (Tern 'b' Nil Nil Nil) (Tern 'c' Nil Nil Nil) (Tern 'd' Nil Nil Nil)) ~=? [Tern 'b' Nil Nil Nil,Tern 'c' Nil Nil Nil,Tern 'd' Nil Nil Nil],

  procRaizTrie ejemploTrie ~=? [Nothing],
  procRaizTrie (TrieNodo (Just "PLP") []) ~=? [Just "PLP"],

  procSubTries ejemploTrie ~=? [('T', TrieNodo Nothing [('P', TrieNodo Nothing [('1', TrieNodo (Just "TP1") [])])]), ('P', TrieNodo Nothing [('L', TrieNodo Nothing [('P', TrieNodo (Just "PLP") [])])])],
  procSubTries (TrieNodo (Just "PLP") []) ~=? []
  ]

testsEj2 = test [ 
  foldAT (\r h1 h2 h3 -> r + h1 + h2 + h3 ) 0 ejemploAT ~=? 85, --suma todas los numeros guardados en los nodos de un arbol ternario 
  foldAT (\r h1 h2 h3 -> 1 + h1 + h2 + h3) 0 ejemploAT ~=? 13, ----devuelve la cantidad de nodos de un arbol ternario 

  foldRose (\r hijos -> r + sum hijos) ejemploRose ~=? 210, --suma todas los numeros guardados en los nodos de una RoseTree 
  foldRose (\r hijos -> 1 + sum hijos) ejemploRose ~=? 20, --devuelve la cantidad de nodos de una RoseTree

  cantPalabras ejemploTrie ~=? 2,
  palabrasDelTrie ejemploTrie ~=? ["TP1","PLP"]
  ]

testsEj3 = test [
  unoxuno [1,2,3] ~=? [[1],[2],[3]],
  unoxuno "hola" ~=? ["h","o","l","a"],

  sufijos [1,2,3] ~=? [[1,2,3],[2,3],[3],[]],
  sufijos "hola" ~=? ["hola","ola","la","a",""]
  ]

testsEj4 = test [
  preorder ejemploAT ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4],
  inorder ejemploAT ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4],
  postorder ejemploAT ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16]
  ]


testsEj5 = test [
  preorderRose ejemploRose ~=? [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
  hojasRose ejemploRose ~=? [3,4,5,7,8,9,10,12,13,15,16,17,18,19,20],
  ramasRose ejemploRose  ~=? [[1,2,3],[1,2,4],[1,2,5],[1,6,7],[1,6,8],[1,6,9],[1,6,10],[1,11,12],[1,11,13],[1,14,15],[1,14,16],[1,14,17],[1,14,18],[1,14,19],[1,20]]
  ]

t = TrieNodo Nothing  [ ('a', TrieNodo (Just True) []),
    ('b', TrieNodo Nothing
    [('a', TrieNodo (Just True)
    [('d', TrieNodo Nothing [])])
    ]),
    ('c', TrieNodo (Just True) [])
  ]

t2 = TrieNodo Nothing  [ ('a', TrieNodo Nothing []),
    ('b', TrieNodo (Just True)
    [('a', TrieNodo Nothing
    [('d', TrieNodo Nothing [])])
    ]),
    ('c', TrieNodo (Just True) []),
    ('d', TrieNodo (Just True) [])
  ]

testsEj6 = test [
  caminos t ~=? ["","a","b","ba","bad","c"],
  caminos t2 ~=? ["","a","b","ba","bad","c","d"]
  ]

testsEj7 = test [ 
  palabras t ~=? ["a","ba","c"],
  palabras t2 ~=? ["b","c","d"]
  ]

esNil :: AT a -> Bool
esNil = (\t -> case t of
                    Nil -> True
                    _ -> False)

testsEj8a = test [
  ifProc esNil procVacio procId Nil ~=? listaVaciaAT,
  ifProc esNil procVacio procId ejemploAT ~=? [ejemploAT]
  ]
testsEj8b = test [
  (++!) postorder preorder ejemploAT ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16,16,1,9,7,2,14,0,3,6,10,8,5,4],
  (++!) preorderRose hojasRose ejemploRose ~=? [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,3,4,5,7,8,9,10,12,13,15,16,17,18,19,20]
  ]
testsEj8c = test [
  ((\z->[0..z]) .! map (+1)) [1,3] ~=? [0,1,2,0,1,2,3,4],
  ((\z -> [z]) .! map (+2))  [2,4] ~=? [4,6]
  ]


