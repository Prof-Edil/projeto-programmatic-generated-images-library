import Lib
import Shapes
import Draw
import Graphics.Rasterific

--fish, triangle or testFig


testOut2 = over (rot45 (Lib.flip (rot testOut1))) (rot (rot (rot45 (Lib.flip (rot testOut1))))) 
testOut3 = transform (multFst (-1)) ((rot.rot.rot) (Lib.flip testOut1))

semiCircles1 = quartet q1 q1 q1 q4
semiCircles2 = quartet q4 q2 q2 q3
semiCircles3 = quartet q2 q3 q1 q1
semiCircles4 = quartet q1 q3 q4 q1
semiCircles = quartet semiCircles1 semiCircles2 semiCircles3 semiCircles4
  
test = drawLines $ joinImgs 
   [testOut1,
   q1]

testFig = semiCircles

test_img = draw "img_tests/img.png" testFig
test_rot = draw "img_tests/rot.png" (rot testFig)
test_rot45 = draw "img_tests/rot45.png" (rot45 testFig)
test_img2 = draw "img_tests/img2.png" (img2 testFig)
test_img3 = draw "img_tests/img3.png" (img3 testFig)
test_u = draw "img_tests/u.png" (u testFig)
test_t = draw "img_tests/t.png" (t testFig)
test_quartet = draw "img_tests/quartet.png" (quartet testFig testFig testFig testFig)
test_side2 = draw "img_tests/side2.png" (side 2 testFig)
test_corner2 = draw "img_tests/corner2.png" (corner 2 testFig)
test_squarelimit2 = draw "img_tests/squarelimit2.png" (squarelimit 2 testFig)
test_squarelimit3 = draw "img_tests/squarelimit3.png" (squarelimit 3 testFig)
test_squarelimit4 = draw "img_tests/squarelimit4.png" (squarelimit 4 testFig)

test_circumference = draw "img_tests/circumference.png" (circumference fish)

testPath = drawFill "img_tests/testPath.png" (rot $ rot q1)

testShapes = drawShapes "img_tests/testShapes.png" test

main :: IO ()
main = do
  --test_img
  --test_rot
  --test_rot45
  --test_img2
  --test_img3
  --test_u
  --test_t
  --test_quartet
  --test_side2
  --test_corner2
  --test_squarelimit2
  --test_squarelimit3
  --test_squarelimit4
  --test_circumference

  testPath
  testShapes


