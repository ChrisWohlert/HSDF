module Main (main) where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Base
import Graphics.UI.GLUT
import LoadShaders
import Prelude hiding (init)
import Core
import GLSL (buildFragment)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices UniformLocations

data UniformLocations = UniformLocations {resolution :: UniformLocation}

init :: IO Descriptor
init = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices =
        [ Vertex2 (-1.0) (-1.0), -- Triangle 1
          Vertex2 1.0 (-1.0),
          Vertex2 (-1.0) 1.0,
          Vertex2 1.0 (-1.0), -- Triangle 2
          Vertex2 1.0 1.0,
          Vertex2 (-1.0) 1.0
        ] ::
          [Vertex2 GLfloat]
      numVertices = length vertices
      vertexSize = sizeOf (head vertices)

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <-
    loadShaders
      [ ShaderInfo VertexShader (FileSource "./shaders/sdf.vert"),
        ShaderInfo FragmentShader (ByteStringSource $ buildFragment $ circle (Vec3 (1.0, 1.0, 1.0)) 1.5 +-> circle (Vec3 (0.5, 1.0, 1.0)) 0.2)
      ]
  currentProgram $= Just program

  resolutionUL <- uniformLocation program "iResolution"

  let firstIndex = 0
      vPosition = AttribLocation 0
  vertexAttribPointer vPosition
    $= ( ToFloat,
         VertexArrayDescriptor 2 Float 0 (bufferOffset (firstIndex * vertexSize))
       )
  vertexAttribArray vPosition $= Enabled

  return $
    Descriptor triangles (fromIntegral firstIndex) (fromIntegral numVertices) (UniformLocations resolutionUL)

resolutionV :: Size -> Vector2 GLfloat
resolutionV (Size x y) = Vector2 (realToFrac x) (realToFrac y)

display :: Descriptor -> DisplayCallback
display (Descriptor triangles firstIndex numVertices uniformLocations) = do
  size <- get windowSize
  uniform (resolution uniformLocations) $= resolutionV size
  clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  swapBuffers

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 3)
  _ <- createWindow progName
  descriptor <- init
  displayCallback $= display descriptor
  mainLoop