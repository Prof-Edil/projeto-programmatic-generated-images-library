import Lib

test_rot = drawAndWrite "img_tests/rot.png" (rot fish)
test_rot45 = drawAndWrite "img_tests/rot45.png" (rot45 fish)
test_fish2 = drawAndWrite "img_tests/fish2.png" fish2
test_fish3 = drawAndWrite "img_tests/fish3.png" fish3
test_u = drawAndWrite "img_tests/u.png" u

main :: IO ()
main = do
  test_rot
  test_rot45
  test_fish2
  test_fish3
  test_u
