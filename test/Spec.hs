import Lib

test_rot = drawAndWrite "img_tests/rot.png" rot
test_rot45 = drawAndWrite "img_tests/rot45.png" rot45

main :: IO ()
main = do
  test_rot
  test_rot45
