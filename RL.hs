module RL where

import Data.Array
import System.Random
import Data.List

{-
    O stare este reprezentată ca un număr întreg.
    O cale este o listă de stări, eventual infinită.

    O estimare reprezintă o mulțime de asocieri (stare, informație aferentă).
    Cu toate că mediul este bidimensional, utilizăm o reprezentare liniară
    a acestuia, bazată pe un `Array`, cu stările indexate ca în figura din 
    enunț.
-}
type State      = Int
type Path       = [State]
type Estimation = Array State Float

{-
    Lățimea și înălțimea mediului, precum și numărul de stări.
-}
width, height, nStates :: Int
width   = 4
height  = 3
nStates = width * height

{-
    Perechile de stări vecine.
-}
neighbors :: [(State, State)]
neighbors = [ (1, 2), (1, 5)
            , (2, 1), (2, 3)
            , (3, 2), (3, 4)
            , (3, 7)
            , (4, 3), (4, 8)
            , (5, 1), (5, 9)
            , (7, 3), (7, 8), (7, 11)
            , (8, 4), (8, 7), (8, 12)
            , (9, 5), (9, 10)
            , (10, 9), (10, 11)
            , (11, 7), (11, 10), (11, 12)
            , (12, 8), (12, 11)
            ]

{-
    Starea de pornire.
-}
startState :: State
startState = 1

{-
     Stările terminale.
-}
terminalStates :: [State]
terminalStates = [8, 12]

{-
    Rata de învățare alfa și factorul de scalare a acesteia.
-}
learningRate, scaleFactor :: Float
learningRate = 0.1
scaleFactor  = 0.999

-------------------------------------------------------------------------------
-- Completați sub această linie.


--  === 1. Generarea căilor ===

{-
    *** TODO ***

    Întoarce toate stările vecine ale unei stări.
-}

neighborsOf :: State -> [State]
neighborsOf firstOccur = 
    map snd (filter (\ x -> firstOccur == fst x) neighbors)

{-
    *** TODO ***

    Construiește o cale aleatoare infinită, pe b  aza unui generator.

    Hint: `Data.List.iterate`, `System.Random.split`.
-}
randomPath :: RandomGen g => g -> (Path, g)
randomPath generator = 
    let (firstGen, secondGen) = split generator
    in (((map fst (iterate (\ pair -> ((neighborsOf (fst pair)) !! (mod (fst (next (snd pair))) (length (neighborsOf (fst pair)))), (snd (split (snd pair))))) (startState, firstGen))), secondGen))
                                                                                                                                  -- snd sau fst, e acelasi lucru
{-
    *** TODO ***

    Trunchiază o cale, eventual infinită, la prima stare terminală.
-}
terminatePath :: Path -> Path
terminatePath path = 
    if (path == [])
        then
            []
            else
                if (not (elem (head path) terminalStates))
                    then ((head path) : (terminatePath (tail path)))
                    else [] ++ [head path]

{-
    *** TODO ***

    Construiește o infinitate de căi infinite.
-}
-- genereaza acceasi cale la infinit :(
randomPaths :: RandomGen g => g -> [Path]
randomPaths generator =
    (fst (randomPath generator)) : []

--  === 2. Estimarea utilităților fără diminuarea ratei de învățare ===

{-
    *** TODO ***

    Array cu conscințele specifice fiecărei stări.
-}
reinforcements :: Array State Float
reinforcements = 
    array (1, 12) [(1, 0.0), (2, 0.0), (3, 0.0), (4, 0.0), (5, 0.0), (6, 0.0), (7, 0.0), (8, -1.0), (9, 0.0), (10, 0.0), (11, 0.0), (12, 1.0)]

{-
    *** TODO ***

    Valorile inițiale ale stărilor, înaintea rulării algoritmului.
    Se construiesc pe baza array-ului de consecințe.
-}
initialEstimation :: Estimation
initialEstimation  = 
    array (1, 12) [(1, 0.0), (2, 0.0), (3, 0.0), (4, 0.0), (5, 0.0), (6, 0.0), (7, 0.0), (8, -1.0), (9, 0.0), (10, 0.0), (11, 0.0), (12, 1.0)]

{-
    *** TODO ***

    Lista de utilități provenite dintr-o estimare.
-}
values :: Estimation -> [Float]
values utilities = 
    elems utilities

{-
    *** TODO ***

    Reprezentarea sub formă de șir de caractere a unei estimări.
    Se va întrebuința forma bidimensională, ca în imaginile din enunț.
    De asemenea, utilitățile vor fi rotunjite la 2 zecimale, și vor
    avea semnul inclus.

    Hint: `Text.Printf`.

    Exemplu de rezultat:

    -0.07 +0.06 +0.20 +1.00
    -0.20 +0.00 -0.43 -1.00
    -0.32 -0.45 -0.56 -0.78

    Pentru a vizualiza corect caracterele de linie nouă, aplicați
    în interpretor funcția `putStrLn` asupra șirului obținut.
-}
showEstimation :: Estimation -> String
showEstimation pp = "PP FTW"

{-
    *** TODO ***

    Actualizează o estimare în urmare parcurgerii unei căi.

    Hint: `Data.Array.accum`.
-}

updateEstimation :: Estimation -> Path -> Estimation
updateEstimation estimation path = 
    accum (+) estimation [(r, v) | r <- path, 
    let v = if (not (elem (path !! ((head (elemIndices r path)))) terminalStates)) 
        then (learningRate * ((rr !! (r - 1)) + (ee !! (path !! (head (elemIndices r path) + 1) - 1)) - (ee !! (r - 1)))) 
        else 0]
    where rr = elems reinforcements
          ee = elems estimation
-- accum aduna elementele aflate pe acelasi index

{-
    *** TODO ***

    Obține un flux infinit de estimări rafinate succesiv, pe baza unui flux
    infinit de căi finite, încheiate în stări terminale.

    Hint: `Data.List.mapAccumL`.
-}
estimations :: [Path] -> [Estimation]
estimations = undefined

{-
    *** TODO ***

    Determină estimarea de rang dat ca parametru, pe baza unui generator.
-}
estimate :: RandomGen g => Int -> g -> Estimation
estimate = undefined

{-
    *** TODO ***

    Pentru o stare, determină vecinul cu cea mai mare valoare estimată.

    Hint: `Data.Function.on`.
-}
bestNeighborOf :: State -> Estimation -> State
bestNeighborOf = undefined

{-
    *** TODO ***

    Contruiește o cale începută în starea inițială, pe principiul alegerii 
    vecinului cu utilitata maximă.
-}
bestPath :: Estimation -> Path
bestPath = undefined


--  === 3. Estimarea utilităților cu diminuarea ratei de învățare ===

{-
    *** TODO ***

    Fluxul infinit al ratelor de învățare scalate:

    [ 1
    , learningRate
    , learningRate * scaleFactor
    , learningRate * scaleFactor^2
    , ...
    ]
-}
scaledLearningRates :: [Float]
scaledLearningRates = undefined

{-
    *** TODO ***

    Tip de date pentru reținerea atât a valorii estimate a unei stări,
    cât și a numărului de vizitări ale acesteia.
-}
data StateInfo = Todo
    deriving (Show)
