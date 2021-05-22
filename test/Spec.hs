import Lib

test_triangle = drawAndWrite "img_tests/img.png" triangle
test_rot = drawAndWrite "img_tests/rot.png" (rot triangle)
test_rot45 = drawAndWrite "img_tests/rot45.png" (rot45 triangle)
test_triangle2 = drawAndWrite "img_tests/img2.png" (img2 triangle)
test_triangle3 = drawAndWrite "img_tests/img3.png" (img3 triangle)
test_u = drawAndWrite "img_tests/u.png" (u triangle)
test_t = drawAndWrite "img_tests/t.png" (t triangle)
test_side2 = drawAndWrite "img_tests/side2.png" (side 2 triangle)
test_corner2 = drawAndWrite "img_tests/corner2.png" (corner 2 triangle)
test_squarelimit2 = drawAndWrite "img_tests/squarelimit2.png" (squarelimit 2 triangle)
test_squarelimit3 = drawAndWrite "img_tests/squarelimit3.png" (squarelimit 3 triangle)
test_squarelimit4 = drawAndWrite "img_tests/squarelimit4.png" (squarelimit 4 triangle)

main :: IO ()
main = do
  test_triangle
  test_rot
  test_rot45
  test_triangle2
  test_triangle3
  test_u
  test_t
  test_side2
  test_corner2
  test_squarelimit2
  test_squarelimit3
  test_squarelimit4

