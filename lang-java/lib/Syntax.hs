module Syntax where

import ATerms.ATerm
import qualified ATerms.Parser as P


-- Data type for Java types
data JavaType
  = IntType
  | LongType
  | FloatType
  | DoubleType
  | BooleanType
  | CharType
  | StringType
--   | ByteType
--   | ShortType
  | ObjectType String  -- For custom object types
  | ArrayType JavaType
  | Void
  deriving (Eq, Show)





-- Data type for literals
data Literal
  = IntLiteral Int
  | LongLiteral Integer
  | FloatLiteral Float
  | DoubleLiteral Double
  | CharLiteral Char
  | StringLiteral String
  | BooleanLiteral Bool
  | NullLiteral
  deriving (Eq, Show)

data BinaryOp
    = EqualityOp
    | BooleanOp
    | ArithmaticOp
    | StringConcatOp
    deriving (Eq, Show)


data UnaryOp
    = Not
    deriving (Eq, Show)


  -- Data type for expressions
data Expression
    = LiteralE Literal
    | VariableIdE String
    | MethodCallE String [Expression]
    | BinaryOpE Expression BinaryOp Expression
    | UnaryOpE UnaryOp Expression
    -- | CastE JavaType Expression
    -- | InstanceOfE Expression JavaType
    | ThisE  -- I need to make sure apply the correct path for the query here as only on P is allowed
    -- | SuperE
    | NewE String [Expression]
    | FieldAccessE Expression String
    | MethodInvocationE Expression String [Expression]
    deriving (Eq, Show)

data Statement
  = AssignmentS String Expression
  | IfS Expression [Statement] (Maybe [Statement])
--   | ForStatement (Maybe Statement) (Maybe Expression) (Maybe Expression) [Statement]
  | WhileS Expression [Statement]
--   | DoWhileStatement Expression [Statement]
--   | SwitchStatement Expression [SwitchCase] (Maybe [Statement])
  | VariableDeclarationS JavaType String (Maybe Expression)
  | ReturnS (Maybe Expression)
  | BreakS
  | ContinueS
--   | ThrowStatement Expression
--   | TryStatement [Statement] (Maybe [CatchClause]) (Maybe [Statement]) (Maybe FinallyClause)
--   | SynchronizedStatement Expression [Statement]
  | ExpressionS Expression
  deriving (Eq, Show)



-- Data type for Compilation Unit
data CompilationUnit = CompilationUnit [ImportDeclaration] ClassDeclaration
  deriving (Eq, Show)

-- Data type for Import Declaration
newtype ImportDeclaration = ImportDeclaration String
  deriving (Eq, Show)

-- Data type for Class Declaration
data ClassDeclaration = 
  ClassDeclaration {
    className :: String,
    memebers :: [Member],
    isStatic :: Bool,
    constructor :: Maybe Constructor
  }
  deriving (Eq, Show)

-- Data type for Members
data Member
  = FieldDeclaration JavaType String (Maybe Expression)
  | MethodDeclaration {
    returnType :: Maybe JavaType,
    methodName :: String,
    methodParameters :: [MethodParameter],
    methodBody :: [Statement]
  }
  deriving (Eq, Show)

-- Data type for Constructor
data Constructor = Constructor {
  constructorParameters :: [MethodParameter],
  constructorBody :: [Statement]
  }
  | DefaultConstructor -- use the super constructor
  deriving (Eq, Show)

data MethodParameter = Parameter JavaType String
    deriving (Eq, Show)


 {-
 
 
 type checking is hard, (scope graphs) sematics for name binding
 how to order type checking (with statix it takes care of stable querying)

 How bad is this in practice, can you we get the benifits of using scope graphs without the comelexity of statixs 
Manually phasing the type checking is usefull 

================================================================
How do we do it?? using scope graphs
How difficult is it to do it in phases and how many phases we need.
================================================================



 -}