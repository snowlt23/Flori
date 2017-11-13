
import ast
import scope

import tables

type
  SemanticContext* = object
    modules*: Table[Name, Scope]


