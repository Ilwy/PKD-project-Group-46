import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss

window :: Display
window = InWindow "Color test" (windowWidth, windowHeight) (0, 0)

background :: Color
background = black

main :: IO ()
main = do
         drawing' <- drawing
         display window background drawing'

imgPath = "C:/Users/Martin/Desktop/cat.bmp"
ballsOnWidth = 100
windowWidth = 1000
windowHeight = 700
ballSizeFactor = 0.6
animationOfWindow = 0.9

--convention: Ball with position (x,y), color c and radius r given by Ball (x,y) c r
data Ball = Ball (Float, Float) Color Float

drawing :: IO Picture
drawing = do
            img <- getImage
            return $ Pictures $ map drawBall (createBallList img ballsOnWidth windowWidth windowHeight)


drawBall :: Ball -> Picture
drawBall (Ball (x,y) c r) = Translate x y $ Color c (circleSolid r)


getImage' :: IO (Image PixelRGB8)
getImage' = do
            dynImg <- readImage imgPath
            case dynImg of
               Left _ -> error "error1 loading image"
               Right dynImg ->
                  case dynImg of
                     ImageRGB8 img -> return img
                     _ -> error "error2 loading image"

getImage :: IO (Image PixelRGB8)
getImage = do
            dynImg <- readImage imgPath
            case dynImg of
               Left _ -> error "error1 loading image"
               Right dynImg ->
                  return $ convertRGB8 dynImg

--createBallList img n ww wh
--post: img transformed into balls with dimension and position according to n (=number of balls on screen length) and window dim (ww*wh)
createBallList :: Image PixelRGB8 -> Int -> Int -> Int -> [Ball]
createBallList img@(Image iw ih _) n ww wh = let
                                                d = iw `div` n
                                                ips = [(x,y) | x <- [0,d..(iw-d)], y <- [0,d..(ih-d)]]
                                                (aw, ah) = getAnimationDim iw ih ww wh
                                             in
                                                map (createBall img aw ah d) ips

--getAnimationDim iw ih ww wh
--post: (animation width, animation height), based on imaage dim (iw*ih) and window dim (ww*wh)
getAnimationDim :: Int -> Int -> Int -> Int -> (Float, Float)
getAnimationDim iw ih ww wh = let
                                 iw' = fromIntegral iw
                                 ih' = fromIntegral ih
                                 ww' = fromIntegral ww
                                 wh' = fromIntegral wh
                              in
                                 if ih' / iw' > wh' / ww'
                                    then  let
                                             ah = animationOfWindow * wh'
                                             aw = ah * (iw'/ih')
                                          in
                                             (aw, ah)
                                    else  let
                                             aw = animationOfWindow * ww'
                                             ah = aw * (ih'/iw')
                                          in
                                             (aw, ah)

                                    

--createBall img aw ah d (ix,iy)
--post: ball with same relative position in animation area aw*ah as in img at (ix,iy), color by box d*d in img at (ibx,iby) and radius relative to d of img
createBall :: Image PixelRGB8 -> Float -> Float -> Int -> (Int, Int) -> Ball
createBall img@(Image iw ih _) aw ah d (ibx,iby) = let
                                                      c = avgBoxColor img ibx iby d
                                                      d' = fromIntegral d
                                                      ibx' = fromIntegral ibx + d'/2   --image box center
                                                      iby' = fromIntegral iby + d'/2   --image box center
                                                      iw' = fromIntegral iw
                                                      icx = iw' /2   --image center
                                                      icy = fromIntegral ih /2   --image center
                                                      x = ((ibx' - icx) / icx) * (aw / 2)
                                                      y = ((icy - iby') / icy) * (ah /2)
                                                      r = (d' / (2*iw')) * aw * ballSizeFactor
                                                   in
                                                      Ball (x,y) c r


--avgBoxColor img x y d
--post: average color of box in img at pixel location (x,y) with sides d pixels
avgBoxColor :: Image PixelRGB8 -> Int -> Int -> Int -> Color
avgBoxColor img x y d = avgColor [toRGB $ pixelAt img x y | x <- [x..(x+d-1)], y <- [y..(y+d-1)]]

--toRGB pixel
--post: the rgb of the pixel
toRGB :: PixelRGB8 -> (Float, Float, Float)
toRGB (PixelRGB8 r g b) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

--avgColor rgbs
--post: average color of all rgb values in rgbs
avgColor :: [(Float, Float, Float)] -> Color
avgColor cs =  let
                  r = avg $ map (\(r,_,_) -> r) cs
                  g = avg $ map (\(_,g,_) -> g) cs
                  b = avg $ map (\(_,_,b) -> b) cs
               in
                  makeColor r g b 1

--avg xs
--post: average value of elements in list
avg :: [Float] -> Float
avg xs = (sum xs) / (fromIntegral $ length xs)
