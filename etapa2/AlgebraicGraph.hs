module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node x) = S.fromList [x]
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2)
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node _) = S.empty
edges (Overlay g1 g2) = S.union (edges g1) (edges g2)
edges (Connect g1 g2) = S.union (edges g1) $ S.union (edges g2) $ S.cartesianProduct (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node x) = S.empty
outNeighbors node (Overlay g1 g2)
    | isIn1 && isIn2 = S.union (outNeighbors node g1) (outNeighbors node g2)
    | isIn1 = outNeighbors node g1
    | isIn2 = outNeighbors node g2
    | otherwise = S.empty
    where
        nodes1 = nodes g1
        nodes2 = nodes g2
        isIn1 = S.member node nodes1
        isIn2 = S.member node nodes2
outNeighbors node (Connect g1 g2)
    | isIn1 = S.union (outNeighbors node g1) nodes2
    | isIn2 = outNeighbors node g2
    | otherwise = S.empty
    where
        nodes1 = nodes g1
        nodes2 = nodes g2
        isIn1 = S.member node nodes1
        isIn2 = S.member node nodes2
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node x) = S.empty
inNeighbors node (Overlay g1 g2)
    | isIn1 && isIn2 = S.union (inNeighbors node g1) (inNeighbors node g2)
    | isIn1 = inNeighbors node g1
    | isIn2 = inNeighbors node g2
    | otherwise = S.empty
    where
        nodes1 = nodes g1
        nodes2 = nodes g2
        isIn1 = S.member node nodes1
        isIn2 = S.member node nodes2
inNeighbors node (Connect g1 g2)
    | isIn1 = inNeighbors node g1
    | isIn2 = S.union (inNeighbors node g2) nodes1
    | otherwise = S.empty
    where
        nodes1 = nodes g1
        nodes2 = nodes g2
        isIn1 = S.member node nodes1
        isIn2 = S.member node nodes2

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = deleteNode graph
    where
        deleteNode Empty = Empty
        deleteNode (Node x) = if x == node then Empty else (Node x)
        deleteNode (Overlay g1 g2) = Overlay (deleteNode g1) (deleteNode g2)
        deleteNode (Connect g1 g2) = Connect (deleteNode g1) (deleteNode g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node newNodes graph = mySplitNode graph
    where
        mySplitNode Empty = Empty
        mySplitNode (Node x) = if x == node then myGraph else (Node x)
        mySplitNode (Overlay g1 g2) = Overlay (mySplitNode g1) (mySplitNode g2)
        mySplitNode (Connect g1 g2) = Connect (mySplitNode g1) (mySplitNode g2)
        myGraph = createMyGraph newNodes
        createMyGraph [x] = (Node x)
        createMyGraph (x:xs) = Overlay (Node x) $ createMyGraph xs
        createMyGraph [] = Empty
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = myMergeNodes graph
    where
        myMergeNodes Empty = Empty
        myMergeNodes (Node x) = if prop x then (Node node) else (Node x)
        myMergeNodes (Overlay g1 g2) = Overlay (myMergeNodes g1) (myMergeNodes g2)
        myMergeNodes (Connect g1 g2) = Connect (myMergeNodes g1) (myMergeNodes g2)
