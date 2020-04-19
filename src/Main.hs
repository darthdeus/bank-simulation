{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified System.Random as R

expCDF :: Float -> Float -> Float
expCDF alpha t = 1 - (exp (- (t / alpha)))

inverseExpCDF :: Float -> Float -> Float
inverseExpCDF alpha p = (- log (1 - p)) * alpha

betaPDF :: Float -> Float -> Float -> Float -> Float
betaPDF alpha beta rho x = rho * (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0))


data State = State {
    currentTime :: Float,    -- Current time in the simulation for snapshotting
    lastArrival :: Float,    -- Time of last arrival
    lastProcessed :: Float,  -- Time of last processed customer
    queue :: [Float],        -- Queue of customers in front of the teller
    arrivals :: [Float],     -- Realtive arrival times
    processing :: [Float],   -- Processing times of each customer
    waited :: [Float]        -- Time waited in the queue
} deriving (Show)

mkState :: [Float] -> [Float] -> State
mkState arrivals processing = State {
        currentTime = 0,
        lastArrival = 0,
        lastProcessed = 0,
        queue = [],
        arrivals = arrivals,
        processing = processing,
        waited = []
    }

step :: State -> (Bool, State)
step state@State{processing=[]} = (True, state)
step state@State{arrivals=[]} = (False, stepProcessed state)
step state@State{queue=[]} = (False, stepArrival state)
step state@State{arrivals=a:_, processing=p:_, lastArrival=la, queue=q:_} = 
    let nextArrival = (lastArrival state) + a
        nextProcessing = (lastProcessed state) + p
    in if nextArrival < nextProcessing
        then (False, stepArrival state)
        else (False, stepProcessed state)

stepArrival :: State -> State
stepArrival state = let
        a:as = arrivals state
        arrivalTime = a + lastArrival state
    in state {
        currentTime = arrivalTime,
        lastArrival = arrivalTime,
        lastProcessed = if (null $ queue state) then arrivalTime else (lastProcessed state),
        queue = (queue state) ++ [arrivalTime], -------------------- |
        arrivals = as                                             -- |
    }                                                             -- |
                                                                  -- |
stepProcessed :: State -> State                                   -- |
stepProcessed state =                                             -- |                       
    let                                                           -- |
        p:ps = processing state                                   -- |----- these two are a performance issue
        q:qs = queue state                                        -- |      and should be done with prepend + reverse
        w = waited state                                          -- |      to avoid O(n^2) ops
        ct = (lastProcessed state) + p                            -- |
    in state {                                                    -- |
        currentTime = ct,                                         -- |
        lastProcessed = (lastProcessed state) + p,                -- |
        queue = qs,                                               -- |
        processing = ps,                                          -- |
        waited = w ++ [ct - q] ------------------------------------- |
    }

iterateSimulation :: [State] -> Bool -> State -> [State]
iterateSimulation states True _ = reverse states
iterateSimulation states False state = 
    let (finished, newState) = step state
        --- Since we use `step` to determine if the simulation finished,
        --- we have to explicitly avoid duplicating the last state.
        newStates = if finished then states else newState:states
    in iterateSimulation newStates finished newState

runSimulation :: State -> [State]
runSimulation initial = if (length $ arrivals initial) == (length $ processing initial)
                        then iterateSimulation [initial] False initial
                        else error "Number of arrivals and process times must be equal."


--- Simulation parameters
data Params = Params {
    expAlpha :: Float,
    rho :: Float,
    alpha :: Float,
    beta :: Float
}

defaultParams :: Params
defaultParams = Params {
    expAlpha = 100,
    rho = 200,
    alpha = 2,
    beta = 5
}

--- Run the simulation with given parameters and `n` iterations.
simulation :: Params -> Int -> IO ([Float], [Int])
simulation Params{ expAlpha, rho, alpha, beta } n = do
    gen <- R.newStdGen
    R.setStdGen gen
    
    arrivals <- fmap (map (inverseExpCDF expAlpha)) $ replicateM n R.randomIO
    processing <- fmap (map (betaPDF alpha beta rho)) $ replicateM n R.randomIO

    let results = runSimulation $ mkState arrivals processing
    
    let waitTimes = waited $ last results
    let queueLengths = map (length . queue) results

    return (waitTimes, queueLengths)

yellowParams :: Params
yellowParams = defaultParams { alpha = 2, beta = 5 }

redParams :: Params
redParams = defaultParams { alpha = 2, beta = 2 }

blueParams :: Params
blueParams = defaultParams { alpha = 5, beta = 1 }

average :: [Float] -> Float
average xs = (sum xs) / (fromIntegral $ length xs)

argmax :: [Float] -> Int
argmax xs = let m = maximum xs
            in snd $ head $ filter (\x -> (fst x) == m) (zip xs [0..])

main :: IO ()
main = do
    --- Due to the above mentioned list appends `n` is kept relatively small
    --- to get an instant answer.
    let n = 1000
    
    -- This last part could definitely be written in a more general way,
    -- but I sort of ran out of time and wanted to get it done under 6 hours :)

    (yw, yq) <- simulation yellowParams n
    (rw, rq) <- simulation redParams n
    (bw, bq) <- simulation blueParams n
   
    putStrLn "- Given only yellow customers, what are the average and maximum customer waiting times?"
    putStrLn $ "Average: " ++ (show $ average yw) ++ "\tMaximum: " ++ (show $ maximum yw)
    
    putStrLn "- Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
    putStrLn $ "Average: " ++ (show $ average $ map fromIntegral rq) ++ "\tMaximum: " ++ (show $ maximum rq)
    
    putStrLn "- Which type of customer (yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?"
    
    let ydiff = (maximum yw) - (average yw)
    let rdiff = (maximum rw) - (average rw)
    let bdiff = (maximum bw) - (average bw)

    let names = ["Yellow", "Red", "Blue"]

    putStrLn $ names !! (argmax [ydiff, rdiff, bdiff])
    