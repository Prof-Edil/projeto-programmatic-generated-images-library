module Shapes where


import Codec.Picture( PixelRGBA8( .. ), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

triangle :: [Line]
triangle = [ Line (V2 0.00 0.00) (V2 1.00 1.00)
           , Line (V2 0.00 0.00) (V2 0.00 1.00)
           , Line (V2 0.00 1.00) (V2 1.00 1.00)
           ]

fish :: [CubicBezier]
fish = [ (CubicBezier (V2 0.00 0.00) (V2 0.08 0.02) (V2 0.22 0.18) (V2 0.29 0.28))
       , (CubicBezier (V2 0.29 0.28) (V2 0.30 0.36) (V2 0.29 0.43) (V2 0.30 0.50))
       , (CubicBezier (V2 0.30 0.50) (V2 0.34 0.60) (V2 0.43 0.68) (V2 0.50 0.74))
       , (CubicBezier (V2 0.50 0.74) (V2 0.58 0.79) (V2 0.66 0.78) (V2 0.76 0.80))
       , (CubicBezier (V2 0.76 0.80) (V2 0.82 0.88) (V2 0.94 0.95) (V2 1.00 1.00))
       , (CubicBezier (V2 1.00 1.00) (V2 0.90 0.97) (V2 0.81 0.96) (V2 0.76 0.95))
       , (CubicBezier (V2 0.76 0.95) (V2 0.69 0.96) (V2 0.62 0.96) (V2 0.55 0.96))
       , (CubicBezier (V2 0.55 0.96) (V2 0.49 0.90) (V2 0.40 0.83) (V2 0.35 0.80))
       , (CubicBezier (V2 0.35 0.80) (V2 0.29 0.76) (V2 0.19 0.72) (V2 0.14 0.69))
       , (CubicBezier (V2 0.14 0.69) (V2 0.09 0.65) (V2 (-0.03) 0.57) (V2 (-0.05) 0.28))
       , (CubicBezier (V2 (-0.05) 0.28) (V2 (-0.04) 0.18) (V2 (-0.02) 0.05) (V2 0.00 0.00))
    
       , (CubicBezier (V2 0.10 0.15) (V2 0.14 0.18) (V2 0.18 0.22) (V2 0.18 0.25))
       , (CubicBezier (V2 0.18 0.25) (V2 0.16 0.26) (V2 0.14 0.27) (V2 0.12 0.27))
       , (CubicBezier (V2 0.12 0.27) (V2 0.11 0.23) (V2 0.11 0.19) (V2 0.10 0.15))
    
       , (CubicBezier (V2 0.05 0.18) (V2 0.10 0.20) (V2 0.08 0.26) (V2 0.09 0.30))
       , (CubicBezier (V2 0.09 0.30) (V2 0.07 0.32) (V2 0.06 0.34) (V2 0.04 0.33))
       , (CubicBezier (V2 0.04 0.33) (V2 0.04 0.27) (V2 0.04 0.19) (V2 0.05 0.18))

       , (CubicBezier (V2 0.11 0.30) (V2 0.16 0.44) (V2 0.24 0.61) (V2 0.30 0.66))
       , (CubicBezier (V2 0.30 0.66) (V2 0.41 0.78) (V2 0.62 0.84) (V2 0.80 0.92))

       , (CubicBezier (V2 0.23 0.20) (V2 0.35 0.20) (V2 0.44 0.22) (V2 0.50 0.25))
       , (CubicBezier (V2 0.50 0.25) (V2 0.50 0.33) (V2 0.50 0.41) (V2 0.50 0.49))
       , (CubicBezier (V2 0.50 0.49) (V2 0.46 0.53) (V2 0.42 0.57) (V2 0.38 0.61))

       , (CubicBezier (V2 0.29 0.29) (V2 0.36 0.26) (V2 0.43 0.27) (V2 0.48 0.31))

       , (CubicBezier (V2 0.34 0.39) (V2 0.38 0.34) (V2 0.44 0.36) (V2 0.48 0.37))

       , (CubicBezier (V2 0.34 0.49) (V2 0.38 0.44) (V2 0.41 0.42) (V2 0.48 0.43))

       , (CubicBezier (V2 0.45 0.58) (V2 0.46 0.60) (V2 0.47 0.61) (V2 0.48 0.61))

       , (CubicBezier (V2 0.42 0.61) (V2 0.43 0.64) (V2 0.46 0.68) (V2 0.48 0.67))

       , (CubicBezier (V2 0.25 0.74) (V2 0.17 0.83) (V2 0.08 0.91) (V2 0.00 0.99))
       , (CubicBezier (V2 0.00 0.99) (V2 (-0.08) 0.91) (V2 (-0.17) 0.82) (V2 (-0.25) 0.74))
       , (CubicBezier (V2 (-0.25) 0.74) (V2 (-0.20) 0.63) (V2 (-0.11) 0.53) (V2 (-0.03) 0.43))

       , (CubicBezier (V2 (-0.17) 0.74) (V2 (-0.13) 0.66) (V2 (-0.08) 0.60) (V2 (-0.01) 0.56))

       , (CubicBezier (V2 (-0.12) 0.79) (V2 (-0.07) 0.71) (V2 (-0.02) 0.66) (V2 0.05 0.60))

       , (CubicBezier (V2 (-0.06) 0.86) (V2 (-0.03) 0.77) (V2 0.03 0.72) (V2 0.10 0.66))

       , (CubicBezier (V2 (-0.02) 0.92) (V2 0.02 0.84) (V2 0.09 0.77) (V2 0.16 0.70))
       ]


testOut1 = [CubicBezier (V2 1.00 1.00) (V2 0.85 0.95) (V2 0.70 0.95) (V2 0.50 0.98)
           ,CubicBezier (V2 0.50 0.98) (V2 0.30 0.70) (V2 0.15 0.60) (V2 0.00 1.00)
           ]

petal = [ toPrim $ CubicBezier (V2 0.5 0.2) (V2 0.9 0.6)
                            (V2  0.05 1.0) (V2 0.5 1.40)
     , toPrim $ Line (V2 0.5 1.40) (V2 1.20 0.8)
     , toPrim $ Line (V2 1.20 0.8) (V2 0.5 0.2) ]


pathscycle = [PathCubicBezierCurveTo (V2 0.0 0.0) (V2 0.8 0.2) (V2 0.9 0.9),
            PathLineTo (V2 0.0 0.9), PathLineTo (V2 0.0 0.0)]            


q1 = [CubicBezier (V2 0.0 0.0) (V2 0.1 0.4) (V2  0.4 0.7) (V2 0.9 0.9),
      --CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.9) (V2 0.9 0.9),
      --CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.0) (V2 0.9 0.0), 
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.0 0.9) (V2 0.0 0.9),
      CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.9) (V2 0.9 0.9)] 
      --CubicBezier (V2 0.9 0.0) (V2 0.9 0.0) (V2 0.9 0.9) (V2 0.9 0.9)]

q2 = [CubicBezier (V2 0.9 0.0) (V2 0.75 0.2) (V2  0.4 0.7) (V2 0.0 0.9),
      --CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.0) (V2 0.9 0.0),
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.0) (V2 0.9 0.0), 
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.0 0.9) (V2 0.0 0.9),
      CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.9) (V2 0.9 0.9), 
      CubicBezier (V2 0.9 0.0) (V2 0.9 0.0) (V2 0.9 0.9) (V2 0.9 0.9)]

q3 = [CubicBezier (V2 0.0 0.0) (V2 0.4 0.1) (V2 0.7 0.4) (V2 0.9 0.9),
      --CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.9) (V2 0.9 0.9),
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.0) (V2 0.9 0.0), 
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.0 0.9) (V2 0.0 0.9),
      CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.9) (V2 0.9 0.9), 
      CubicBezier (V2 0.9 0.0) (V2 0.9 0.0) (V2 0.9 0.9) (V2 0.9 0.9)]

q4 = [CubicBezier (V2 0.0 0.9) (V2 0.4 0.8) (V2 0.8 0.5) (V2 0.9 0.0),
      --CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.0) (V2 0.9 0.0),
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.9 0.0) (V2 0.9 0.0), 
      CubicBezier (V2 0.0 0.0) (V2 0.0 0.0) (V2 0.0 0.9) (V2 0.0 0.9),
      CubicBezier (V2 0.0 0.9) (V2 0.0 0.9) (V2 0.9 0.9) (V2 0.9 0.9), 
      CubicBezier (V2 0.9 0.0) (V2 0.9 0.0) (V2 0.9 0.9) (V2 0.9 0.9)]      

arc :: [CubicBezier]
arc = [CubicBezier (V2 0.5 0.0) (V2 0.5 0.2759575) (V2 0.72404248 0.5) (V2 1.0 0.5)]

