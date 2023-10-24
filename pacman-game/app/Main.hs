import Graphics.Gloss

window :: Display
window = InWindow "My Gloss Game" (400, 300) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = Color red (Circle 50)

main :: IO ()
main = display window background drawing
