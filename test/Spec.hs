import Lib

test_fish = drawAndWrite "img_tests/fish.png" fish
test_rot = drawAndWrite "img_tests/rot.png" (rot fish)
test_rot45 = drawAndWrite "img_tests/rot45.png" (rot45 fish)
test_fish2 = drawAndWrite "img_tests/fish2.png" (img2 fish)
test_fish3 = drawAndWrite "img_tests/fish3.png" (img3 fish)
test_u = drawAndWrite "img_tests/u.png" (u fish)
test_t = drawAndWrite "img_tests/t.png" (t fish)
test_side2 = drawAndWrite "img_tests/side2.png" (side 2 fish)
test_corner2 = drawAndWrite "img_tests/corner2.png" (corner 2 fish)
test_squarelimit2 = drawAndWrite "img_tests/squarelimit2.png" (squarelimit 2 fish)
test_squarelimit3 = drawAndWrite "img_tests/squarelimit3.png" (squarelimit 3 fish)
test_squarelimit4 = drawAndWrite "img_tests/squarelimit4.png" (squarelimit 4 fish)

main :: IO ()
main = do
  test_fish
  test_rot
  test_rot45
  test_fish2
  test_fish3
  test_u
  test_t
  test_side2
  test_corner2
  test_squarelimit2
  test_squarelimit3
  test_squarelimit4

