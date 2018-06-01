
import image, fexpr

# discard fident(Span(filename: "test.flori"), "new_fident")
# discard fseq(Span(filename: "test.flori"), farray([fident(Span(filename: "test.flori"), "new_fblock"), flist(Span(filename: "test.flori"), farray[FExpr]())]))
# saveimage("testimage.fimg")

loadimage("testimage.fimg")
echo gImage.fexprs[0]
echo gImage.fexprs[1]
echo gImage.fexprs[2]
echo gImage.fexprs[3]
