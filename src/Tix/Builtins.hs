module Tix.Builtins (builtins) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Tix.Types

builtins :: Map Text Scheme
builtins =
  M.fromList
    [ ( "builtins",
        scheme $
          NAttrSet $
            M.fromList
              [ ("abort", scheme $ NAtomic String :-> deBruijn 0 0),
                ("add", [StringOrNumber $ deBruijn 2 0] :=> (deBruijn 3 0 :-> (deBruijn 4 0 :-> deBruijn 4 0))),
                ("all", [] :=> ((deBruijn 4 0 :-> NAtomic Bool) :-> (List (deBruijn 5 0) :-> NAtomic Bool))),
                ("any", [] :=> ((deBruijn 4 0 :-> NAtomic Bool) :-> (List (deBruijn 5 0) :-> NAtomic Bool))),
                ("attrNames", [] :=> (deBruijn 3 0 :-> List (NAtomic String))),
                ("attrValues", [] :=> (deBruijn 3 0 :-> List (deBruijn 0 0))),
                ("baseNameOf", [] :=> (NAtomic String :-> NAtomic String)),
                ("bitAnd", [] :=> (NAtomic Number :-> (NAtomic Number :-> NAtomic Number))),
                ("bitXor", [] :=> (NAtomic Number :-> (NAtomic Number :-> NAtomic Number))),
                ("bitOr", [] :=> (NAtomic Number :-> (NAtomic Number :-> NAtomic Number))),
                ("ceil", [] :=> (NAtomic Number :-> NAtomic Number)),
                ("compareVersions", [] :=> (NAtomic String :-> (NAtomic String :-> NAtomic Number))),
                ("concatLists", [] :=> (List (List (deBruijn 5 0)) :-> List (deBruijn 4 0))),
                ("concatMap", [] :=> ((deBruijn 4 0 :-> List (deBruijn 5 1)) :-> (List (deBruijn 5 0) :-> List (deBruijn 5 1)))),
                ("map", [] :=> ((deBruijn 4 0 :-> deBruijn 4 1) :-> (List (deBruijn 5 0) :-> List (deBruijn 5 1)))),
                ("catAttrs", [] :=> (NAtomic String :-> (List (deBruijn 0 0) :-> List (deBruijn 0 0))))
              ]
      ),
      ( "derivation",
        [ deBruijn 2 0
            // attrSet
              [ ("all", scheme $ List (deBruijn 5 1)),
                ("drvAttrs", scheme $ deBruijn 4 0),
                ("drvPath", string),
                ("out", scheme $ deBruijn 4 1),
                ("outPath", string),
                ("outputName", string),
                ("type", string)
              ]
            $ deBruijn 2 1,
          deBruijn 2 0 .! ("name", string),
          deBruijn 2 0 .! ("builder", string),
          deBruijn 2 0 .! ("system", string),
          deBruijn 2 0 .? ("args", scheme $ List (NAtomic String)),
          deBruijn 2 0 .? ("outputs", scheme $ List (NAtomic String))
        ]
          :=> (deBruijn 3 0 :-> deBruijn 3 1)
      )
    ]

string :: Scheme
string = scheme $ NAtomic String

deBruijn :: Int -> Int -> NType
deBruijn a b = NBruijn $ DeBruijn a b

attrSet :: [(Text, Scheme)] -> NType
attrSet = NAttrSet . M.fromList

(.!) :: NType -> (Text, Scheme) -> Pred
(.!) = HasField RequiredField

(.?) :: NType -> (Text, Scheme) -> Pred
(.?) = HasField OptionalField
