{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import Data.List

{- Tipul unei nod utilizat în procesul de căutare. -}

data Node s a = NodeNil | Node {
                            state    :: s,
                            action   :: a,
                            parent   :: Node s a,
                            depth    :: Int,
                            children :: [Node s a]
                          }

{-
    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState n = state n

{-
    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}


createGraph :: (ProblemState s a) => s -> a -> Int -> Node s a -> Node s a
createGraph x naction ndepth nparent = root
      where root = Node {
                          state = x,
                          action = naction,
                          parent = nparent,
                          depth = ndepth,
                          children = [createGraph (snd i) (fst i) (ndepth + 1) root
                                      | i <- successors x ]
                        }


createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace x = createGraph x dummyAction 0 NodeNil
    where dummyAction = (fst $ head $ successors x)

{-
    Ordonează întreg spațiul stărilor după euristica din ProblemState.
-}

compareFunc :: (ProblemState s a) => Node s a -> Node s a -> Ordering
compareFunc s1 s2 = compare (heuristic $ nodeState s1) (heuristic $ nodeState s2)

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace n = Node {
                        state = state n,
                        action = action n,
                        parent = parent n,
                        depth = depth n,
                        children = Data.List.sortBy compareFunc [orderStateSpace i | i <- children n]
                    }


{-
    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.
-}

parseStateSpace :: (Ord s) => Node s a -> Int -> S.Set s -> [Node s a] -> (S.Set s, [Node s a])

parse1Ch :: (Ord s) => [Node s a] -> Int -> S.Set s -> [Node s a] -> (S.Set s, [Node s a])
parse1Ch child maxDepth vis nodesParsed = parseC1
 where c1 = child !! 0
       parseC1 = parseStateSpace c1 maxDepth vis nodesParsed

parse2Ch :: (Ord s) => [Node s a] -> Int -> S.Set s -> [Node s a] -> (S.Set s, [Node s a])
parse2Ch child maxDepth vis nodesParsed = parseC2
  where c2 = child !! 1
        parse1 = parse1Ch child maxDepth vis nodesParsed
        parseC2 = parseStateSpace c2 maxDepth (fst parse1) (snd parse1)

parse3Ch :: (Ord s) => [Node s a] -> Int -> S.Set s -> [Node s a] -> (S.Set s, [Node s a])
parse3Ch child maxDepth vis nodesParsed = parseC3
  where c3 = child !! 2
        parse2 = parse2Ch child maxDepth vis nodesParsed
        parseC3 = parseStateSpace c3 maxDepth (fst parse2) (snd parse2)

parse4Ch :: (Ord s) => [Node s a] -> Int -> S.Set s -> [Node s a] -> (S.Set s, [Node s a])
parse4Ch child maxDepth vis nodesParsed = parseC4
  where c4 = child !! 3
        parse3 = parse3Ch child maxDepth vis nodesParsed
        parseC4 = parseStateSpace c4 maxDepth (fst parse3) (snd parse3)


parseStateSpace n maxDepth vis nodesParsed
  | (depth n) == maxDepth =  (vis, nodesParsed)
  | (S.member (nodeState n) vis) && ((depth n) /= 0) = (vis, nodesParsed)
  | length chil == 0 = ((S.union vis setN), (nodesParsed ++ [n]))
  | length chil == 4 =  parse4Ch chil maxDepth (S.union setN vis) (nodesParsed ++ [n])
  | length chil == 3 =  parse3Ch chil maxDepth (S.union setN vis) (nodesParsed ++ [n])
  | length chil == 2 = parse2Ch chil maxDepth  (S.union setN vis) (nodesParsed ++ [n])
  | length chil == 1 = parse1Ch chil maxDepth  (S.union setN vis) (nodesParsed ++ [n])
  | otherwise =  (vis, nodesParsed)
    where chil = children n
          setN = S.fromList [nodeState n]



limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri

limitedDfs n maxDepth = snd $ parseStateSpace n (maxDepth + 1) (S.fromList [nodeState n]) []

{-
    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate
iterativeDeepening n = itDeep n 0 0


itDeep :: (ProblemState s a, Ord s) => Node s a -> Int -> Int -> (Node s a, Int)

itDeep n maxDepth nodesSoFar
  | length t == 0 = itDeep n (maxDepth + 1) nodesSoFar
  | any isGoal (map nodeState t) = ((head solution),    -- nodul solutie --
                                    ((nodesSoFar +      -- nodurile de pana acum     +
                                     (length t))        -- nodurile din parc curenta -
                                     - (getNodesAfter   -- nodurile care nu trebuie parcurse (cele de dupa solutie)
                                              (nodeState $ head solution) -- :sol: -> starea solutiei
                                              (map nodeState t)           -- :xs:  -> lista cu toate starile
                                              0                           -- :idx: -> indexul initial
                                              len)))                      -- :ini: -> lungimea totala
  | otherwise = itDeep n (maxDepth + 1) (nodesSoFar + (length t))
    where t = limitedDfs n maxDepth
          len = length t
          solution = [x | x <- t, isGoal (nodeState x)]

getNodesAfter :: (Eq s) => s -> [s] -> Int -> Int -> Int
getNodesAfter sol xs idx ini
  | sol == (head xs)  = (ini - idx)
  | otherwise = getNodesAfter sol (tail xs) (idx + 1) ini


{-
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extPath :: Node s a -> [Node s a] -> [Node s a]
extPath n acc
  | depth n == 0 = acc
  | otherwise = extPath (parent n) (n : acc)

extractPath :: Node s a -> [(a, s)]
extractPath n = zip (map action nodePath) (map state nodePath)
  where nodePath = extPath n []

{-
    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru
    a găsi prima stare finală și reface calea către nodul inițial folosind
    extractPath.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> Bool       -- Dacă să folosească sau nu euristica dată
      -> [(a, s)]   -- Lista perechilor
solve n withEuristic = extractPath $ fst $ iterativeDeepening (if withEuristic then orderStateSpace $ createStateSpace n
                                                                               else createStateSpace n)

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
