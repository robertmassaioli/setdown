{
module SetLanguage where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
}

%wrapper "basic-bytestring"

$digit = [0-9]
$nonFilename = [^\\\/\?\%\*\:\|\<\>\ ]
$ident = [a-zA-Z0-9\-\_]

tokens :-

   $white+           ;
   "/\"              { const And }
   "\/"              { const Or }
   "-"               { const Difference }
   [\"]              { const Quote }
   \"$nonFilename+\" { Filename . TL.init . TL.tail . TLE.decodeUtf8 }
   $ident+$white*":" { IdentifierDefinition . TL.strip . TL.init . TLE.decodeUtf8 }
   $ident+           { Identifier . TLE.decodeUtf8 }

{
data Token 
   = Filename TL.Text
   | IdentifierDefinition TL.Text
   | Identifier TL.Text
   | Colon
   | And
   | Or
   | Difference
   | Quote
   deriving(Show, Eq)
}
