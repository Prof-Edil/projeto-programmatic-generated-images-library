import Lib

test_fish = drawAndWrite "img_tests/fish.png" fish
test_rot = drawAndWrite "img_tests/rot.png" (rot fish)
test_rot45 = drawAndWrite "img_tests/rot45.png" (rot45 fish)
test_fish2 = drawAndWrite "img_tests/fish2.png" fish2
test_fish3 = drawAndWrite "img_tests/fish3.png" fish3
test_u = drawAndWrite "img_tests/u.png" u
test_t = drawAndWrite "img_tests/t.png" t
test_side2 = drawAndWrite "img_tests/side2.png" (side 2)
test_corner2 = drawAndWrite "img_tests/corner2.png" (corner 2)


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

