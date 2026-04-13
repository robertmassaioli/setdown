{
{-# OPTIONS_GHC -w #-}
module SetLanguage where

import qualified Data.ByteString.Lazy    as ByteString
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
}

%wrapper "posn-bytestring"

$digit = [0-9]
$nonFilename = [^\\\/\?\%\*\:\|\<\>\ ]
$ident = [a-zA-Z0-9\-\_]

tokens :-

   $white+           ;
   "--".*            ;
   "("               { tok LParenTok }
   ")"               { tok RParenTok }
   "/\"              { tok IntersectionTok }
   "\/"              { tok UnionTok }
   "∪"               { tok UnionTok }
   "∩"               { tok IntersectionTok }
   "-"               { tok DifferenceTok }
   \"[^\"]+\"        { \pos bs -> LocatedToken pos (FilenameTok . TL.unpack . TL.init . TL.tail . TLE.decodeUtf8 $ bs) }
   $ident+$white*":" { \pos bs -> LocatedToken pos (IdentifierDefinitionTok . TL.strip . TL.init . TLE.decodeUtf8 $ bs) }
   $ident+           { \pos bs -> LocatedToken pos (IdentifierTok . TLE.decodeUtf8 $ bs) }

{
data SetToken
   = FilenameTok FilePath
   | IdentifierDefinitionTok TL.Text
   | IdentifierTok TL.Text
   | IntersectionTok
   | UnionTok
   | DifferenceTok
   | LParenTok
   | RParenTok
   deriving(Show, Eq)

prettyToken :: SetToken -> String
prettyToken (FilenameTok fp)               = "\"" ++ fp ++ "\""
prettyToken (IdentifierDefinitionTok name) = TL.unpack name ++ ":"
prettyToken (IdentifierTok name)           = TL.unpack name
prettyToken IntersectionTok                = "/\\"
prettyToken UnionTok                       = "\\/"
prettyToken DifferenceTok                  = "-"
prettyToken LParenTok                      = "("
prettyToken RParenTok                      = ")"

data LocatedToken = LocatedToken AlexPosn SetToken deriving (Show, Eq)

tok :: SetToken -> AlexPosn -> ByteString.ByteString -> LocatedToken
tok t pos _ = LocatedToken pos t
}
