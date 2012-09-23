tests :: Spec
  = "testset" ident "{" test* "}" { TestSet $1 $2 }

test :: TestCase
  = "test" ident "{" statement* "}" { Test $1 $2 }

statement :: Statement
  = ident "=" expression ";" { Assign $1 $2 }
  / expression ";" { ExpressionState $1 }
  / "assert" "(" expression operator expression ")" ";" { Assert $1 $2 $3 }

expression :: Expression
  = call { $1 }
  / ident { Var $1 }
  / const_double { ConstDouble $1 }
  / const_int { ConstInt $1 }
  / const_string { ConstString $1 }
  / const_list { ConstList $1 }

call :: Expression
  = ident "(" (expression, ",") ")" { Call $1 $2 }

const_list :: [Expression]
  = "[" (expression, ",") "]" { $1 }

const_int ::: Int
  = [0-9]+ { read $1 }

const_double ::: Double
  = [0-9]+ "." [0-9]+ { read ($1 ++ "." ++ $2)}

const_string :: String
  = '\"' literal* '\"'    { concat $1 }
  / '\'' literal* '\''    { concat $1 }

literal :: String
  = '\\' esc_literal { ['\\', $1] }
  / nonesc_literal   { [$1] }

esc_literal :: Char
  = 'n' { 'n' }
  / 'r' { 'r' }
  / 't' { 't' }
  / '\\' { '\\' }
  / '\"' { '\"' }
  / '\'' { '\'' }

nonesc_literal :: Char
  = [^\n\r\t\\\"\'] { $1 }

ident ::: String
  = [a-zA-Z_][a-zA-Z0-9_.]* { ($1 : $2) }

operator ::: Operator
  = "==" { Equal }
  / ">=" { GreaterEqual }
  / ">" { GreaterThan }
  / "<=" { LowerEqual }
  / "<" { LowerThan }

skip :: () = [ \r\n\t] { () } / comment
comment :: () = '#' _:(!'\n' . { () })* '\n' { () }
delimiter :: () = [()[\]{}<>;:,./@?\"] { () }
