
import image
import fexpr

# discard fident(Span(filename: "test.flori"), "new_fident")
# discard fident(Span(filename: "test.flori"), "new_fsymbol")
# discard fident(Span(filename: "test.flori"), "new_fblock")
# saveimage("testimage.fimg")

loadimage("testimage.fimg")
echo gImage.fexprs[0]
echo gImage.fexprs[1]
echo gImage.fexprs[2]
