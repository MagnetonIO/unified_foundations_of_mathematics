{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

class AlgebraicStructure a op | a -> op where
    operate :: op -> [a] -> a

data Operation a = BinaryOp (a -> a -> a)
                 | UnaryOp (a -> a)

class AlgebraicStructure a (Operation a) => Group a where
    identity :: a
    inverse  :: a -> a

instance AlgebraicStructure Integer (Operation Integer) where
    operate (BinaryOp f) [x, y] = f x y
    operate (UnaryOp f) [x]     = f x
    operate _ _                 = error "Invalid operation"

instance Group Integer where
    identity = 0
    inverse x = -x

addOp :: Operation Integer
addOp = BinaryOp (+)

exampleSum :: Integer
exampleSum = operate addOp [5, 3]

exampleInv :: Integer
exampleInv = operate (UnaryOp negate) [5]
