
import ../image, ../fexpr, ../parser

# discard parseToplevel("test.flori", """
# 1 + 1
# """)
# saveimage("testimage.fimg")

loadimage("testimage.fimg")
echo gImage.fexprs[0]
echo gImage.fexprs[1]
echo gImage.fexprs[2]
echo gImage.fexprs[3]
gImage.fexprs[3].error("span test!")
