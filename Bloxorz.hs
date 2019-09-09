{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = '-'

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Orientation = Vertical | Horizontal | Standing
    deriving (Eq, Ord)

data Cell = HardT | SoftT | SwitchT | EmptyT | WinningT | BlockT
    deriving (Eq, Ord)

data State = Won | Lost | Continue 
    deriving (Eq, Ord)

instance Show Cell where
    show cell 
      | cell == HardT = [hardTile]
      | cell == SoftT = [softTile]
      | cell == SwitchT = [switch]
      | cell == EmptyT = [emptySpace]
      | cell == WinningT = [winningTile]
      | cell == BlockT = [block]
      | otherwise = [emptySpace]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Lstart { 
                bmap :: A.Array Position Cell,
                bx :: (Position, Position),
                rdown :: Position,
                swtch :: A.Array Int (Position, [Position]),
                ornt :: Orientation,
                state :: State
             }
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

myshow :: Position -> Level -> String
myshow p l 
    | snd p == (snd $ rdown l) = show (if (fst (bx l) == p) || (snd (bx l) == p) then
                                         BlockT 
                                       else
                                         ((bmap l) A.! p)) ++ "\n" ++ myshow (fst p + 1, 0) l 
    | fst p > (fst $ rdown l) =  ""
    | otherwise = show (if (fst (bx l) == p) || (snd (bx l) == p) then
                          BlockT 
                        else
                          ((bmap l) A.! p)) ++ myshow (fst p, snd p + 1) l

instance Show Level where
    show level 
      | state level == Won = levelShow ++ "Congrats! You won!\n"
      | state level == Lost = levelShow ++ "Game Over\n"
      | otherwise = levelShow  
        where levelShow = "\n" ++ myshow (0,0) level

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel p1 p2 = Lstart {
    bmap = A.array ((0,0), p1) [((x,y), EmptyT) | x <- [0.. fst p1], y <- [0..snd p1]],
    bx = (p2, p2),
    rdown = p1,
    swtch = A.array (0,5) [(x, ((0,0), [])) | x <- [0..5]],
    ornt = Standing,
    state = Continue
}

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile c p l 
    | c == 'H' = (Lstart ((bmap l) A.// [(p, HardT)]) (bx l) (rdown l) (swtch l) (ornt l) (state l))
    | c == 'S' = (Lstart ((bmap l) A.// [(p, SoftT)]) (bx l) (rdown l) (swtch l) (ornt l) (state l))
    | c == 'W' = (Lstart ((bmap l) A.// [(p, WinningT)]) (bx l) (rdown l) (swtch l) (ornt l) (state l))
    | otherwise = l

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

findNewSwitchIndex :: Level -> Int -> Int
findNewSwitchIndex l x
    | snd ((swtch l) A.! x) == [] = x
    | otherwise = findNewSwitchIndex l (x + 1)  

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch p lp lvl = (Lstart ((bmap lvl) A.// [(p, SwitchT)]) (bx lvl) (rdown lvl) ((swtch lvl) A.// [(newIndex, (p, lp))]) (ornt lvl) (state lvl)) 
    where newIndex = findNewSwitchIndex lvl 0   

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

onSwitch :: Level -> Position -> Int -> Int
onSwitch l p x 
    | x == 5 = (-1)
    | fst ((swtch l) A.! x) == p = x
    | otherwise = onSwitch l p (x + 1)

makeHard :: Level -> [Position] -> Level
makeHard l p 
    | null p == True = l
    | otherwise = makeHard (Lstart ((bmap l) A.// [(head p, HardT)]) (bx l) (rdown l) (swtch l) (ornt l) (state l)) (tail p)

makeEmpty :: Level -> [Position] -> Level
makeEmpty l p 
    | null p == True = l
    | otherwise = makeEmpty (Lstart ((bmap l) A.// [(head p, EmptyT)]) (bx l) (rdown l) (swtch l) (ornt l) (state l)) (tail p)

activate :: Cell -> Level -> Level
activate c l 
    | c == HardT = l
    | c == SoftT = if (ornt l) == Standing then 
                        (Lstart (bmap l) (bx l) (rdown l) (swtch l) (ornt l) Lost)
                   else l
    | c == EmptyT = (Lstart (bmap l) (bx l) (rdown l) (swtch l) (ornt l) Lost)
    | c == SwitchT = if bxFirst /= -1 || bxSecond /= -1 then 
                        if (bmap l) A.! (head onoff) == EmptyT then (makeHard l onoff)
                        else (makeEmpty l onoff)   
                     else l
    | c == WinningT = (Lstart (bmap l) (bx l) (rdown l) (swtch l) (ornt l) Won)
    | otherwise = l

    where 
        bxFirst = (onSwitch l (fst (bx l)) 0)
        bxSecond = (onSwitch l (snd (bx l)) 0)
        indexSwitch = max bxFirst bxSecond
        onoff = snd ((swtch l) A.! indexSwitch)
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
newLevel :: Level -> Int -> Int -> Int -> Int -> Orientation -> Level
newLevel lvl d1 d2 d3 d4 newOrnt = (Lstart (bmap lvl) ((bx11 + d1, bx12 + d2), (bx21 + d3, bx22 + d4)) (rdown lvl) (swtch lvl) (newOrnt) (state lvl))
    where 
        bx11 = fst $ fst $ bx lvl
        bx12 = snd $ fst $ bx lvl
        bx21 = fst $ snd $ bx lvl
        bx22 = snd $ snd $ bx lvl

generateNewLevel :: Directions -> Level -> Level
generateNewLevel d l    
    | (state l) == Won || (state l) == Lost = l
    | d == North = case (ornt l) of 
                        Horizontal -> newLevel l (-1) 0 (-1) 0 Horizontal
                        Vertical   -> (Lstart (bmap l) (((min bx11 bx21) - 1, bx12), ((min bx11 bx21) - 1, bx12)) (rdown l) (swtch l) Standing (state l)) 
                        Standing   -> newLevel l (-1) 0 (-2) 0 Vertical
    | d == South = case (ornt l) of 
                        Horizontal -> newLevel l 1 0 1 0 Horizontal
                        Vertical   -> (Lstart (bmap l) (((max bx11 bx21) + 1, bx12), ((max bx11 bx21) + 1, bx12)) (rdown l) (swtch l) Standing (state l))
                        Standing   -> newLevel l 1 0 2 0 Vertical
    | d == East = case (ornt l) of 
                        Horizontal -> (Lstart (bmap l) ((bx11, (max bx12 bx22) + 1), (bx11, (max bx12 bx22) + 1)) (rdown l) (swtch l) Standing (state l))
                        Vertical   -> newLevel l 0 1 0 1 Vertical
                        Standing   -> newLevel l 0 1 0 2 Horizontal
    | d == West = case (ornt l) of 
                        Horizontal -> (Lstart (bmap l) ((bx11, (min bx12 bx22) - 1), (bx11, (min bx12 bx22) - 1)) (rdown l) (swtch l) Standing (state l))
                        Vertical   -> newLevel l 0 (-1) 0 (-1) Vertical
                        Standing   -> newLevel l 0 (-1) 0 (-2) Horizontal
    | otherwise = l

    where
        bx11 = fst $ fst $ bx l
        bx12 = snd $ fst $ bx l
        bx21 = fst $ snd $ bx l
        bx22 = snd $ snd $ bx l

move :: Directions -> Level -> Level
move d l 
    | ornt newGeneratedLevel == Standing = activatedNewGeneratedLevel
    | otherwise = (activate ((bmap activatedNewGeneratedLevel) A.! (snd $ bx activatedNewGeneratedLevel)) activatedNewGeneratedLevel)

    where newGeneratedLevel = (generateNewLevel d l)
          activatedNewGeneratedLevel = (activate ((bmap newGeneratedLevel) A.! (fst $ bx newGeneratedLevel)) newGeneratedLevel)
 

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame l = (state l) == Continue

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors l 
        | state l == Won = []
        | otherwise = (if (state goNorth /= Lost) then [(North, goNorth)] else [] ) ++
                      (if (state goSouth /= Lost) then [(South, goSouth)] else [] ) ++ 
                      (if (state goEast /= Lost) then [(East, goEast)] else [] ) ++
                      (if (state goWest /= Lost) then [(West, goWest)] else []) 
        where 
            goNorth = move North l
            goSouth = move South l
            goEast  = move East l
            goWest  = move West l
        -- generezi toate (move DIRECTION level) -> si daca in urma DIRECTION nu s-a pierdut level ul, atunci adaugi (DIRECTION, (move DIRECTION level)) intr-o lista pe care o returnezi la final cu toate directiile posibile
    isGoal l = (state l) == Won  

    -- Doar petru BONUS
    -- heuristic = undefined
