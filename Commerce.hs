type Producto = (String,Float)

precio :: Producto -> Float
precio producto = snd producto

nombre :: Producto -> String
nombre producto = fst producto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento unProducto unDescuento = precio unProducto * (1 - unDescuento)

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = elem 'x' (nombre unProducto) || elem 'z' (nombre unProducto) || elem 'X' (nombre unProducto) || elem 'Z' (nombre unProducto)

productoXL :: Producto -> String
productoXL unProducto = nombre unProducto ++ "XL"

productoCorriente :: Producto -> Bool
productoCorriente unProducto = vocal (head $ nombre unProducto)

vocal :: Char -> Bool
vocal letra = elem letra "aeiouAEIOU"

productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = length (nombre unProducto) > 10

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio productoPrecio costoEnvio = productoPrecio + costoEnvio

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 (nombre unProducto)

productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoCodiciado unProducto && productoDeLujo unProducto && not (productoCodiciado unProducto)

entregaSencilla :: String -> Bool
entregaSencilla fecha = even . length $ fecha

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal unProducto cantidad descuento costoDeEnvio = aplicarCostoDeEnvio (aplicarDescuento unProducto descuento * cantidad) costoDeEnvio




-- take :: Int -> String -> String
-- drop :: Int -> String -> String
-- head :: String -> Char
-- elem :: Char -> String -> Bool
-- reverse :: String -> String