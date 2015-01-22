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
   "--".*            ;
   "("               { const LParenTok }
   ")"               { const RParenTok }
   "/\"              { const IntersectionTok }
   "\/"              { const UnionTok }
   "∪"               { const IntersectionTok }
   "∩"               { const UnionTok }
   "-"               { const DifferenceTok }
   \"[^\"]+\"        { FilenameTok . TL.unpack . TL.init . TL.tail . TLE.decodeUtf8 }
   $ident+$white*":" { IdentifierDefinitionTok . TL.strip . TL.init . TLE.decodeUtf8 }
   $ident+           { IdentifierTok . TLE.decodeUtf8 }

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
}
