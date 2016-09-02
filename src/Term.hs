

module Term where

-- import qualified Test.QuickCheck as QC

data Term =
  X
  | Const Double
  | Minus Term
  | Add Term Term
  | Mult Term Term
  | Hole
  deriving (Show)

data Dir = L | R deriving Show


holes :: Term -> [(Term, Term)]
holes x@X = [(x, Hole)]
holes c@(Const _) = [(c, Hole)]
holes u@(Minus t) = (u, Hole) : map (fmap Minus) (holes t)
holes u@(Add s t) =
  (u, Hole)
  : map (fmap (Add s)) (holes t) ++ map (fmap (flip Add t)) (holes s)
holes u@(Mult s t) =
  (u, Hole)
  : map (fmap (Mult s)) (holes t) ++ map (fmap (flip Mult t)) (holes s)

insert :: Term -> Term -> Term
insert u =
  let go Hole = u
      go x@X = x
      go c@(Const _) = c
      go (Minus p) = Minus (go p)
      go (Add p q) = Add (go p) (go q)
      go (Mult p q) = Mult (go p) (go q)
  in go

cst :: Double -> Term
cst a = if a < 0 then Minus (Const (-a)) else Const a

linear :: Double -> Double -> Term
linear a b = Add (Mult (cst a) X) (cst b)

quadratic :: Double -> Double -> Double -> Term
quadratic a b c =
  Add (Mult (cst a) (Mult X X)) (Add (Mult (cst b) X) (cst c))

haveSex :: (Term, Term) -> (Term, Term) -> (Term, Term)
haveSex (s, t) (u, v) = (insert u t, insert s v)

eval :: Double -> Term -> Double
eval x =
  let go (Const y) = y
      go X = x
      go (Minus t) = negate (go t)
      go (Add s t) = go s + go t
      go (Mult s t) = go s + go t
  in go
     


{-
instance Arbitrary Term where
  arbitrary = do
   -} 


pretty :: Show a => [a] -> IO ()
pretty = mapM_ print
