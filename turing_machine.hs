import Control.Monad.State


type TeapIndex = Int
type InternalState = Int

type MachineState = (TeapIndex, InternalState)

readTeap :: State MachineState InternalState
readTeap = do
    (currentTeapIndex, currentInternalState) <- get
    let (moveTeapDirection, nextInternalState) = runRule currentTeapIndex currentInternalState
    put (currentTeapIndex + moveTeapDirection, nextInternalState)
    return nextInternalState

runRule :: Int -> Int -> (Int, Int)
runRule teapIndex internalState = do
    let currentTeapString = teap !! teapIndex
    case (currentTeapString, internalState) of
        (0, 0) -> (-1, 2) 
        (0, 1) -> (1, 1)
        (0, 2) -> (1, 2)
        (1, 0) -> (-1, 0)
        (1, 1) -> (-1, 2)
        (1, 2) -> (-1, 1)

teap = [1, 0, 1, 0, 0, 1, 0, 1, 1, 0]
startState = (5, 1)
main = do
    let firstState = execState readTeap startState    
        firstInternalState = evalState readTeap startState    
        secondState = execState readTeap firstState
        secondInternalState = evalState readTeap firstState
    print firstInternalState
    print secondInternalState
    print $ runState readTeap secondState
