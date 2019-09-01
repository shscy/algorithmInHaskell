
module GrpathState where
import qualified Data.Map as Map
import Debug.Trace

-- 状态图的遍历 倒水问题
type CurState = (Int, (Int, Int))

stack = (8, (5, 3))

getF (a, _) = a
getS (_, (b, _)) = b
getT (_, (_, c)) = c

filterF :: CurState -> Bool
filterF (a, (b, c)) = (a >= 0) && (b >= 0) && (c >= 0) && (a <= 8) && (b <= 5) && (c <= 3)
-- a-b (两种)  a可以全部倒进去 或者a不能全部倒进去
runNext :: CurState -> [CurState]
runNext (a, (b, c)) = [(0, (b+a, c)), (a - (getS stack -b), (getS stack, c))] ++ [(0, (b, c+a)), (a - (getT stack -c), (b, getT stack))] ++ [(b+a, (0, c)), (getF stack, (b - (getF stack - a ), c))]  ++ [(a, (0, b+c)), (a, (b - (getT stack - c), getT stack))]++ [(c+a, (b, 0)), (getF stack, (b, c - (getF stack - a)))]  ++ [(a, (c+b, 0)), (a, (getT stack, c- (getT stack - b))) ] 

success:: CurState  -> Bool
success (a, (b, c)) = (a == result ) || (b == result) || (c == result)

type GrpathState = ([StepInfo], Map.Map CurState Bool)
type StepInfo = (CurState, [CurState])

result = 4;
returnCS curState = ([(curState, [])], Map.fromList [(curState, True)])

bfs :: GrpathState -> Maybe [CurState]
bfs (stack, state)  =  case stack of 
    (head: other) -> let cur = fst head
                         history = snd head
                         nextStates1 =  filter (`Map.notMember` state)  (runNext cur) :: [CurState]
                         nextStates =  map (\x -> (x, history++ [cur])) (filter filterF  nextStates1)
                         in if success cur 
                            then 
                                Just (history ++ [cur])
                            else 
                                bfs(other ++ nextStates, Map.insert cur True state)
    []            -> Nothing 