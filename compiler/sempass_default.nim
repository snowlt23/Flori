
import sempass, sempass_create
import sempass_resolve, sempass_special, sempass_ccodegen

proc newDefaultSemPassContext*(): SemPassContext =
  result = newSemPassContext()
  result.register(newSpecialPass())
  result.register(newResolvePass())
