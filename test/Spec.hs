import Lib
import Graphics.Rasterific
import Graphics.Rasterific.Texture
--fish, triangle or testFig


testOut1 = [CubicBezier (V2 1.00 1.00) (V2 0.85 0.95) (V2 0.70 0.95) (V2 0.50 0.98)
           ,CubicBezier (V2 0.50 0.98) (V2 0.30 0.70) (V2 0.15 0.60) (V2 0.00 1.00)
           ]
testOut2 = over (rot45 (Lib.flip (rot testOut1))) (rot (rot (rot45 (Lib.flip (rot testOut1))))) 
testOut3 = transform (multFst (-1)) ((rot.rot.rot) (Lib.flip testOut1))
testFig = over (over testOut1 testOut2) testOut3

test_img = drawAndWrite "img_tests/img.png" testFig
test_rot = drawAndWrite "img_tests/rot.png" (rot testFig)
test_rot45 = drawAndWrite "img_tests/rot45.png" (rot45 testFig)
test_img2 = drawAndWrite "img_tests/img2.png" (img2 testFig)
test_img3 = drawAndWrite "img_tests/img3.png" (img3 testFig)
test_u = drawAndWrite "img_tests/u.png" (u testFig)
test_t = drawAndWrite "img_tests/t.png" (t testFig)
test_quartet = drawAndWrite "img_tests/quartet.png" (quartet testFig testFig testFig testFig)
test_side2 = drawAndWrite "img_tests/side2.png" (side 2 testFig)
test_corner2 = drawAndWrite "img_tests/corner2.png" (corner 2 testFig)
test_squarelimit2 = drawAndWrite "img_tests/squarelimit2.png" (squarelimit 2 testFig)
test_squarelimit3 = drawAndWrite "img_tests/squarelimit3.png" (squarelimit 3 testFig)
test_squarelimit4 = drawAndWrite "img_tests/squarelimit4.png" (squarelimit 4 testFig)

test_simple_img = drawAndWrite2 "img_tests/simplefig.png" (arcLimit 1) 1

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
  test_simple_img