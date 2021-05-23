import Draw (saveDrawing)
import TestDrawings
import CreateDrawings (white)

test_img = saveDrawing "img_tests/img.png" 1000 white img_base
test_rot = saveDrawing "img_tests/rot.png" 1000 white img_rot
test_rot45 = saveDrawing "img_tests/rot45.png" 1000 white img_rot45
test_img2 = saveDrawing "img_tests/img2.png" 1000 white img_img2
test_img3 = saveDrawing "img_tests/img3.png" 1000 white img_img3
test_u = saveDrawing "img_tests/u.png" 1000 white img_u
test_t = saveDrawing "img_tests/t.png" 1000 white img_t
test_quartet = saveDrawing "img_tests/quartet.png" 1000 white img_quartet
test_side2 = saveDrawing "img_tests/side2.png" 1000 white img_side2
test_corner2 = saveDrawing "img_tests/corner2.png" 1000 white img_corner2
test_squarelimit2 = saveDrawing "img_tests/squarelimit2.png" 1000 white img_squarelimit2
test_squarelimit3 = saveDrawing "img_tests/squarelimit3.png" 1000 white img_squarelimit3
test_squarelimit4 = saveDrawing "img_tests/squarelimit4.png" 1000 white img_squarelimit4

testShapes = saveDrawing "img_tests/testShapes.png" 1000 white test2

test_simple_img = saveDrawing "img_tests/simplefig.png" 1200 white simpleFig

main :: IO ()
main = do
  test_img
  test_rot
  test_rot45
  test_img2
  test_img3
  --test_u
  --test_t
  --test_quartet
  --test_side2
  --test_corner2
  --test_squarelimit2
  --test_squarelimit3
  --test_squarelimit4

  testShapes

  test_simple_img


