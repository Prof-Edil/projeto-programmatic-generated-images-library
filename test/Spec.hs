import CreateDrawings 
import Codec.Picture( PixelRGBA8( .. ))
import Draw (saveDrawing)

test_img = saveDrawing "img_tests/img.png" CreateDrawings.white img_base
test_rot = saveDrawing "img_tests/rot.png" CreateDrawings.white img_rot
test_rot45 = saveDrawing "img_tests/rot45.png" CreateDrawings.white img_rot45
test_img2 = saveDrawing "img_tests/img2.png" CreateDrawings.white img_img2
test_img3 = saveDrawing "img_tests/img3.png" CreateDrawings.white img_img3
test_u = saveDrawing "img_tests/u.png" CreateDrawings.white img_u
test_t = saveDrawing "img_tests/t.png" CreateDrawings.white img_t
test_quartet = saveDrawing "img_tests/quartet.png" CreateDrawings.white img_quartet
test_side2 = saveDrawing "img_tests/side2.png" CreateDrawings.white img_side2
test_corner2 = saveDrawing "img_tests/corner2.png" CreateDrawings.white img_corner2
test_squarelimit2 = saveDrawing "img_tests/squarelimit2.png" CreateDrawings.white img_squarelimit2
test_squarelimit3 = saveDrawing "img_tests/squarelimit3.png" CreateDrawings.white img_squarelimit3
test_squarelimit4 = saveDrawing "img_tests/squarelimit4.png" CreateDrawings.white img_squarelimit4


testShapes = saveDrawing "img_tests/testShapes.png" CreateDrawings.white test2

main :: IO ()
main = do
  test_img
  test_rot
  test_rot45
  test_img2
  test_img3
  test_u
  test_t
  test_quartet
  test_side2
  test_corner2
  test_squarelimit2
  test_squarelimit3
  test_squarelimit4

  testShapes


