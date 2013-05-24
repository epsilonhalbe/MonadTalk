import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

swap :: State Stack ()
swap = do a <- pop
          b <- pop
          push a
          push b

dup :: State Stack ()
dup = do a <- pop
         push a
         push a

drop :: State Stack ()
drop = do pop
          a <- pop
          push a

rot :: State Stack ()
rot = do a <- pop
         b <- pop
         c <- pop
         push a
         push c
         push b

stackManip :: State Stack Int
stackManip = do push 3
                pop
                pop

stackManip' :: State Stack Int
stackManip' = push 3 >> pop >> pop
