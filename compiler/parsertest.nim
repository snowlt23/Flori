
import image, fexpr, parser

let p = parseToplevel("test.flori", """
fn id[T](x T) T {
  x
}
4 + 5
""")
