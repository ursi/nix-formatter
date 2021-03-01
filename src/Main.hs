module Main where

import           ByteString.TreeBuilder (intercalate)
import           Control.Monad.Reader (Reader, ask, runReader, withReader)
import           Data.Fix (Fix(..))
import           Data.Foldable (fold)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.Functor.Compose (Compose(..))
-- import           Data.List (intercalate)
import           Data.Semigroup.Foldable (intercalate1)
import           Data.Text (Text, unpack)
import           Nix.Parser (Result(..), parseNixFile, parseNixFileLoc)
import           Nix.Expr.Types
                  ( Antiquoted(..)
                  , Binding(..)
                  , NAttrPath(..)
                  , NBinaryOp(..)
                  , NExpr
                  , NExprF(..)
                  , NKeyName(..)
                  , NString(..)
                  , NRecordType(..)
                  , Params(..)
                  )
import           Nix.Expr.Types.Annotated
                  ( Ann(..)
                  , NExprLoc
                  , NExprLocF
                  , SrcSpan(..)
                  , SourcePos(..)
                  )

file :: String
file = "shell.nix"

main :: IO ()
main = do
  result <- parseNixFileLoc file
  result' <- parseNixFile file
  let
    nixStr =
      FormatState 0
        & (runReader
            $ case result of
                Success nix -> formatNExpr nix
                Failure _ -> pure $ file <> " did no parse"
          )

  putStrLn nixStr
  putStrLn $ show result'


data FormatState =
  FormatState { indentation :: Int }

formatNExpr ::  NExprLoc -> Reader FormatState String
formatNExpr (Fix (Compose (Ann srcSpan nExpr))) =
  let multiline = isMultiline srcSpan in
    case nExpr of
      NConstant natom -> pure $ show natom
      NStr str -> formatNString str
      NSym sym -> pure $ unpack sym

      NList list -> do
        listInside <- ntercalate " " formatNExpr list
        pure $ "[ " <> listInside <> " ]"
      NSet recordType bindings-> do
        bindings <- ntercalate "\n" formatBinding bindings
        let
          spaceOrNl =
            if multiline then "\n"
            else " "

        pure $
          (case recordType of
            NNonRecursive -> ""
            NRecursive -> "rec" <> spaceOrNl
          )
            <> "{"
            <> spaceOrNl
            <> bindings
            <> spaceOrNl
            <> "}"
      -- NSet !NRecordType ![Binding r]	
        -- An attribute set literal

        -- NSet NRecursive    [NamedVar x y _]         ~  rec { x = y; }
        -- NSet NNonRecursive [Inherit Nothing [x] _]  ~  { inherit x; }
      NLiteralPath path -> pure $ path
      -- NEnvPath !FilePath	
        -- A path which refers to something in the Nix search path (the NIX_PATH environment variable. For example, nixpkgs/pkgs.

        -- NEnvPath "x"                                ~  <x>
      -- NUnary !NUnaryOp !r	
        -- Application of a unary operator to an expression.

        -- NUnary NNeg x                               ~  - x
        -- NUnary NNot x                               ~  ! x
      NBinary op nExprL nExprR -> do
        eL <- formatNExpr nExprL
        eR <- formatNExpr nExprR
        let
          surround :: NExprLoc -> Assoc -> String -> String
          surround (Fix (Compose (Ann srcSpan exp))) assoc str =
            case exp of
              NBinary op' _ _ ->
                if addParens assoc op' op then
                  "(" <> str <> ")"
                else
                  str
              _ -> str

          left = surround nExprL ALeft eL
          right = surround nExprR ARight eR

          opStr = formatNBinaryOp op

          fullOpStr =
            case op of
              NApp -> opStr
              _ -> " " <> opStr <> " "
        pure $ left <> fullOpStr <> right
          -- Application of a binary operator to two expressions.

          -- NBinary NPlus x y                           ~  x + y
          -- NBinary NApp  f x                           ~  f x
      NSelect nExpr attrPath mNExpr -> do
        nExprStr <- formatNExpr nExpr
        select <- formatNAttrPath attrPath
        or <- case mNExpr of
          Just nExpr' -> (" or " <>) <$> formatNExpr nExpr'
          Nothing -> pure ""
        pure $ nExprStr <> "." <> select <> or
      -- NHasAttr !r !(NAttrPath r)	
        -- Ask if a set contains a given attribute path.

        -- NHasAttr s (x :| [])                        ~  s ? x
      NAbs params nExprLoc' -> do
        paramsStr <- formatParams params
        rest <- formatNExpr nExprLoc'
        pure $ paramsStr <> ": " <> rest
          -- A function literal (lambda abstraction).

          -- NAbs (Param "x") y                          ~  x: y
      -- NLet ![Binding r] !r	
        -- Evaluate the second argument after introducing the bindings.

        -- NLet []                    x                ~  let in x
        -- NLet [NamedVar x y _]      z                ~  let x = y; in z
        -- NLet [Inherit Nothing x _] y                ~  let inherit x; in y
      -- NIf !r !r !r	
        -- If-then-else statement.

        -- NIf x y z                                   ~  if x then y else z
      -- NWith !r !r	
        -- Evaluate an attribute set, bring its bindings into scope, and evaluate the second argument.

        -- NWith x y                                   ~  with x; y
      -- NAssert !r !r	
        -- Assert that the first returns true before evaluating the second.

        -- NAssert x y                                 ~  assert x; y
      -- NSynHole !VarName
      _ -> pure $ show nExpr

formatParams :: Params NExprLoc -> Reader FormatState String
formatParams params = case params of
  Param varName -> pure $ unpack varName
  ParamSet paramSet variadic mbind -> do
    args <-
      paramSet
        & ntercalate ", "
            (\(varName, mdefault) -> do
              default_ <- case mdefault of
                Just defautlt_ -> (" ? " <>) <$> formatNExpr defautlt_
                Nothing -> pure ""
              pure $ unpack varName <> default_
            )
    pure
      $ "{ "
      <> args
      <> (if variadic then ", ... " else "")
      <> "}"
      <>
        case mbind of
          Just varName -> "@" <> unpack varName
          Nothing -> ""

formatNBinaryOp :: NBinaryOp -> String
formatNBinaryOp op = case op of
  NEq -> "=="
  NNEq -> "!="
  NLt -> "<"
  NLte -> "<="
  NGt -> ">"
  NGte -> ">="
  NAnd -> "&&"
  NOr -> "||"
  NImpl -> "->"
  NUpdate -> "//"
  NPlus -> "+"
  NMinus -> "-"
  NMult -> "*"
  NDiv -> "/"
  NConcat -> "++"
  NApp -> " "

isMultiline :: SrcSpan -> Bool
isMultiline (SrcSpan (SourcePos _ l1 _) (SourcePos _ l2 _)) = l1 /= l2

formatBinding :: Binding NExprLoc -> Reader FormatState String
formatBinding binding = case binding of
  NamedVar attrPath nExpr srcPos -> do
    key <- formatNAttrPath attrPath
    value <- formatNExpr nExpr
    pure $ key <> " = " <> value <> ";"
  Inherit minheritedFrom inherited srcPos -> do
    inheritedFrom <-
      case minheritedFrom of
        Just nExpr -> do
          value <- formatNExpr nExpr
          pure $ "(" <> value <> ") "
        Nothing -> pure ""
    keyNames <- ntercalate " " formatNKeyName inherited
    pure $ "inherit " <> inheritedFrom <> keyNames


formatNKeyName :: NKeyName NExprLoc -> Reader FormatState String
formatNKeyName keyName =
  case keyName of
    DynamicKey antiquoted ->
      formatAntiquoted
        formatNString
        formatNExpr
        antiquoted
    StaticKey key -> pure $ unpack key

formatAntiquoted ::
  (v -> Reader FormatState String) ->
  (r -> Reader FormatState String) ->
  Antiquoted v r ->
  Reader FormatState String
formatAntiquoted fromV fromR antiquoted =
  case antiquoted of
    Plain v -> fromV v
    EscapedNewline -> error "I don't know what to do here"
    Antiquoted r -> fromR r

formatNString :: NString NExprLoc -> Reader FormatState String
formatNString nString =
  case nString of
    DoubleQuoted antiquoted ->
      antiquoted
        & ntercalate ""
            (formatAntiquoted
              (pure . unpack)
              formatNExpr
            )
    Indented int antiquoted -> pure $ show antiquoted

ntercalate ::
  (Monoid m, Traversable t) =>
  m ->
  (r -> Reader FormatState m) ->
  t r ->
  Reader FormatState m
ntercalate sep fromR rs =
  traverse fromR rs
    <&> intercalate sep

formatNAttrPath :: NAttrPath NExprLoc -> Reader FormatState String
formatNAttrPath attrPath = ntercalate "." formatNKeyName attrPath

data Assoc
  = ALeft
  | ARight
  | None
  deriving Eq

newtype Prec = Prec Int deriving Eq

instance Ord Prec where
  (Prec a) <= (Prec b) = b <= a

type OpDesc = (Prec, Assoc)

toOpDesc :: NBinaryOp -> OpDesc
toOpDesc op =
  (\(a, b) -> (Prec a, b))
    $ case op of
        NEq -> (11, None)
        NNEq -> (11, None)
        NLt -> (10, None)
        NLte -> (10, None)
        NGt -> (10, None)
        NGte -> (10, None)
        NAnd -> (12, ALeft)
        NOr -> (13, ALeft)
        NImpl -> (14, None)
        NUpdate -> (9, ARight)
        NPlus -> (7, ALeft)
        NMinus -> (7, ALeft)
        NMult -> (6, ALeft)
        NDiv -> (6, ALeft)
        NConcat -> (5, ARight)
        NApp -> (2, ALeft)

addParens :: Assoc -> NBinaryOp -> NBinaryOp -> Bool
addParens assoc subOp mainOp =
  let
    (subPrec, subAssoc) = toOpDesc subOp
    (mainPrec, mainAssoc) = toOpDesc mainOp
  in
    if subPrec > mainPrec then
      False
    else if subPrec == mainPrec then
      if subAssoc == assoc && mainAssoc == assoc then
        False
      else
        True
    else
      True
{-

addParens ARight NUpdate NApp
  subPrec = Prec 9
  subAssoc = ARight
  mainPrec = Prec 2
  mainAssoc = ALeft

Success
  (Fix
    (NAbs
      (ParamSet [("inNixShell",Nothing)] True (Just "args"))
      (Fix
        (NBinary NApp
          (Fix
            (NBinary NApp
              (Fix (NSym "import"))
              (Fix (NLiteralPath "./."))
            )
          )
          (Fix
            (NBinary NUpdate
              (Fix
                (NSet
                  NNonRecursive
                  [NamedVar
                    (StaticKey "asShell" :| [])
                    (Fix (NSym "inNixShell"))
                    (SourcePos {sourceName = "shell.nix", sourceLine = Pos 1, sourceColumn = Pos 41})
                  ]
                )
              )
              (Fix
                (NBinary NApp
                  (Fix
                    (NBinary NApp
                      (Fix
                        (NSelect
                          (Fix (NSym "builtins"))
                          (StaticKey "removeAttrs" :| [])
                          Nothing
                        )
                      )
                      (Fix (NSym "args"))
                    )
                  )
                  (Fix (NList [Fix (NStr (DoubleQuoted [Plain "inNixShell"]))]))
                )
              )
            )
          )
        )
      )
    )
  )
-}
