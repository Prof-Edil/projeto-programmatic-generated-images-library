import Draw (saveDrawing)
import TestDrawings
import CreateDrawings (white, colorsD)
import Models

test_img = saveDrawing "img_tests/img.png" 1000 white img_base
test_rot = saveDrawing "img_tests/rot.png" 1000 white img_rot
test_rot45 = saveDrawing "img_tests/rot45.png" 1000 white img_rot45
test_img2 = saveDrawing "img_tests/img2.png" 1000 white img_img2
test_img3 = saveDrawing "img_tests/img3.png" 1000 white img_img3
test_u = saveDrawing "img_tests/u.png" 1000 white img_u
test_t = saveDrawing "img_tests/t.png" 1000 white img_t
test_v = saveDrawing "img_tests/v.png" 1000 white img_v
test_quartet = saveDrawing "img_tests/quartet.png" 1000 white img_quartet
test_side2 = saveDrawing "img_tests/side2.png" 1000 white img_side2
test_corner2 = saveDrawing "img_tests/corner2.png" 1000 white img_corner2
test_squarelimit2 = saveDrawing "img_tests/squarelimit2.png" 1000 white img_squarelimit2
test_squarelimit3 = saveDrawing "img_tests/squarelimit3.png" 1000 white img_squarelimit3
test_squarelimit4 = saveDrawing "img_tests/squarelimit4.png" 1000 white img_squarelimit4

test_simple_img = genArcs "img_tests/simplefig.png" 0 seedE

test_simple_img2 = genArcsNew testArc1 "img_tests/simplefig2.png" 0 (seedE+10) colorsD

test_simple_img3 = genArcsNew testArc "img_tests/simplefig3.png" 0 (seedE+4) colorsE
  where testArc = createNewArc 0.6 0.2 0.8 0.4

test_squarelimitN = genSquareLimitNew testSquareLimit "img_tests/squarelimitN.png" 3

test_semiCicles = saveDrawing "img_tests/semiCicles.png" 1000 white semiCicles
test_semiCiclesFill = saveDrawing "img_tests/semiCiclesFill.png" 1000 white semiCiclesFill
test_semiCiclesSquares = saveDrawing "img_tests/semiCiclesSquares.png" 1000 white semiCiclesSquares
test_semiCiclesSquaresFill = saveDrawing "img_tests/semiCiclesSquaresFill.png" 1000 white semiCiclesSquaresFill


main :: IO ()
main = do
  --test_img
  --test_rot
  --test_rot45
  --test_img2
  --test_img3
  --test_u
  --test_t
  test_v
  --test_quartet
  --test_side2
  --test_corner2
  --test_squarelimit2
  --test_squarelimit3
  test_squarelimit4

  test_simple_img
  test_simple_img2
  test_simple_img3

  test_squarelimitN

  test_semiCicles
  test_semiCiclesFill
  test_semiCiclesSquares
  test_semiCiclesSquaresFill

