type var = string
 and toplevel =
   | Toplevel_Let    of var * var list * var list * core_term
   | Toplevel_LetRec of var * var list * var list * core_term
 and core_term =
   | VarE    of var
   | AccE    of path * var
   | FunE    of var * core_type option * core_term
   | AppE    of core_term * core_term
   | LetE    of var * var list * var list * core_term * core_term
   | LetRecE of var * var list * var list * core_term * core_term
   | LetModE of var * mod_term * core_term
   | IfE     of core_term * core_term * core_term
   | IntE    of int
   | StrE    of string
   | BoolE   of bool
   | AddE    of core_term * core_term
   | SubE    of core_term * core_term
   | MulE    of core_term * core_term
   | DivE    of core_term * core_term
   | EqE     of core_term * core_term
   | NeE     of core_term * core_term   
   | GtE     of core_term * core_term
   | LeE     of core_term * core_term
   | GtEqE   of core_term * core_term
   | LeEqE   of core_term * core_term
   | ConjE   of core_term * core_term
   | DisjE   of core_term * core_term
   | ConsE   of core_term * core_term
   | MatchE  of core_term * (pattern * core_term) list
   | NotE    of core_term
   | NegE    of core_term
   | ModE    of mod_term * mod_type
   | ModCodE   of mod_term * mod_type
   | CodE    of core_term
   | EscE    of core_term
   | RunE    of core_term
   | RunModE of core_term * mod_type
   | PairE   of core_term * core_term

 and core_type =
   | VarT  of var
   | AccT  of path * var
   | ArrT  of core_type * core_type
   | AppT  of core_type * core_type
   | CodT  of core_type
   | EscT  of core_type
   | ModT  of mod_type
   | ModCodT of mod_type
   | PairT of core_type * core_type

 and mod_decl =
   | StructureDec of var * mod_term
   | SignatureDec of var * mod_type

 and mod_term  =
   | Structure    of structure
   | VarM         of var
   | UnpackM      of core_term
 and structure = structure_component list
 and structure_component =
   | TypeM        of var * core_type option
   | LetRecM      of var * var list * var list * core_term
   | LetM         of var * var list * var list * core_term
   | ModM         of var * mod_term

 and mod_type  = 
   | Signature of signature 
   | VarS of var 
   | Sharing of mod_type * var * core_type
 and signature = signature_component list
 and signature_component =
   | TypeS  of var * core_type option
   | ValS   of var * core_type
   | ModS   of var * mod_type

 and path =
   | VarP of string
   | DollarP of string

 and pattern =
   | VarPat  of var
   | ConsPat of pattern * pattern
   | PairPat of pattern * pattern
   | WildPat
