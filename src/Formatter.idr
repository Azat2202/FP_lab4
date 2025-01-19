module Formatter
import Parser
import Utils
import Data.String

%default total

public export
data Format : Type where
    Doubl : Maybe Nat -> Format
    Number : Format
    Percent : Format
    Lit : String -> Format

public export
Show Format where 
    show (Doubl l) = "double (" ++ show l ++ ")"
    show Number    = "num"
    show Percent   = "%"
    show (Lit s)   = s

partial
public export
ToFormat : Parser Format
ToFormat = fdouble <|> fnumber <|> fcharacter <|> fpercent

        where
        rightBound : Parser (Maybe Nat)
        rightBound = maybe $ char '.' ~> nat 

        fdouble : Parser Format
        fdouble = map (Doubl) $ char '%' ~> char 'f' ~> rightBound

        fnumber : Parser Format
        fnumber = char '%' ~> char 'd' ~> pure Number

        fpercent : Parser Format 
        fpercent = char '%' ~> char '%' >>= \_ => pure Percent 

        fcharacter : Parser Format
        fcharacter = do s <- some $ sat (/= '%')
                        pure $ Lit $ pack s


public export
PrintFType : List Format -> Type
PrintFType [] = String
PrintFType ((Doubl a) :: fmt)   = (i : Double) -> PrintFType fmt
PrintFType (Number    :: fmt)   = (i : Int)    -> PrintFType fmt
PrintFType (Percent   :: fmt)   = PrintFType fmt
PrintFType ((Lit s)   :: fmt)   = PrintFType fmt

public export
format : (fmt: List Format) -> (acc : String) -> PrintFType fmt
format [] acc                     = acc
format (Number :: xs) acc         = \i => format xs (acc ++ show i)
format ((Doubl len) :: xs) acc    = \i => case len of
                                               Nothing => format xs (acc ++ show i)
                                               (Just l) => format xs (acc ++ show (round l i))
format (Percent :: xs) acc        = format xs (acc ++ "%")
format ((Lit s) :: xs) acc        = format xs (acc ++ s)

public export
partial
printf : (fmt : String) -> PrintFType (ParseFull ToFormat fmt)
printf fmt = format _ ""

partial
main : IO ()
-- main = putStrLn $ show $ round (5) (1111.000137)
main = do putStrLn $ printf "%d %f.1" 1 2.45
-- main = putStrLn $ show $ ParseFull ToFormat "%% %d"