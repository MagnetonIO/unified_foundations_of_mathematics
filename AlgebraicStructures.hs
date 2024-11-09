{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | The 'AlgebraicStructure' type class defines a generic algebraic structure
--   with an operation type 'op' that acts on elements of type 'a'.
--   The functional dependency `a -> op` specifies that the operation type 'op'
--   is uniquely determined by the element type 'a'.
class AlgebraicStructure a op | a -> op where
    -- | 'operate' applies an operation of type 'op' on a list of elements of type 'a'
    --   and returns a result of type 'a'.
    operate :: op -> [a] -> a

-- | The 'Operation' data type represents different kinds of operations
--   that can be applied to elements of an algebraic structure.
--   It includes:
--   * 'BinaryOp': A binary operation that takes two arguments.
--   * 'UnaryOp': A unary operation that takes a single argument.
data Operation a = BinaryOp (a -> a -> a)
                 | UnaryOp (a -> a)

-- | The 'Group' type class extends 'AlgebraicStructure' with additional requirements
--   for a group, namely the existence of an identity element and an inverse operation.
class AlgebraicStructure a (Operation a) => Group a where
    -- | 'identity' is the identity element of the group, such that for any element 'x',
    --   'operate (BinaryOp op) [identity, x] == x' and 'operate (BinaryOp op) [x, identity] == x'.
    identity :: a

    -- | 'inverse' returns the inverse of a given element 'x' in the group, such that
    --   'operate (BinaryOp op) [x, inverse x] == identity'.
    inverse  :: a -> a

-- | An instance of 'AlgebraicStructure' for 'Integer' with operations of type 'Operation Integer'.
instance AlgebraicStructure Integer (Operation Integer) where
    -- | Defines how the 'operate' function applies different kinds of operations:
    --   * For a 'BinaryOp', it applies the function to two elements.
    --   * For a 'UnaryOp', it applies the function to a single element.
    --   * Any other usage results in an error.
    operate (BinaryOp f) [x, y] = f x y
    operate (UnaryOp f) [x]     = f x
    operate _ _                 = error "Invalid operation"

-- | An instance of 'Group' for 'Integer', using standard integer addition:
--   * The identity element is '0'.
--   * The inverse of 'x' is '-x'.
instance Group Integer where
    identity = 0
    inverse x = -x

-- | A predefined addition operation for integers, represented as a 'BinaryOp'.
addOp :: Operation Integer
addOp = BinaryOp (+)

-- | An example of using the 'operate' function with 'addOp' to sum two integers.
exampleSum :: Integer
exampleSum = operate addOp [5, 3]  -- Result: 8

-- | An example of using the 'operate' function with a unary negate operation
--   to find the inverse of an integer.
exampleInv :: Integer
exampleInv = operate (UnaryOp negate) [5]  -- Result: -5
