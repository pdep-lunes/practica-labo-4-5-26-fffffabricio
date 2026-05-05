module Parcial where

import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)
type Number = Float
--Parte A

data Perro= UnPerrito{
    raza::String,
    juguetesPreferidos :: [String],
    tiempo::Number,
    energia::Number
}deriving (Show,Eq)

zara::Perro
zara=UnPerrito{raza="dalmata",juguetesPreferidos=["pelota","mantita"],tiempo=60,energia=80}

jugar :: Perro -> Perro
jugar = modificarEnergia (-10)

ladrar :: Number -> Perro -> Perro
ladrar cantidad = modificarEnergia (cantidad / 2)

modificarEnergia :: Number -> Perro -> Perro
modificarEnergia cantidad perro =
    perro { energia = max 0 (energia perro + cantidad) }

regalar:: String->Perro->Perro
regalar regaloIngresado unperrito= unperrito{juguetesPreferidos=juguetesPreferidos unperrito ++[regaloIngresado]}

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro
    | tiempo unPerro >= 50 || esDeRazaExtravagante unPerro =
        regalar "peine de goma" (unPerro { energia = 100 })
    | otherwise = unPerro

esDeRazaExtravagante :: Perro -> Bool
esDeRazaExtravagante unPerro =
    raza unPerro == "dalmata" || raza unPerro == "pomerania"

diaDeCampo :: Perro -> Perro
diaDeCampo =
    perderPrimerJuguete . jugar

perderPrimerJuguete :: Perro -> Perro
perderPrimerJuguete perro =
    perro { juguetesPreferidos = drop 1 (juguetesPreferidos perro) }

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [(Perro -> Perro, Number)]
} deriving (Show)


guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos =
    UnaGuarderia "PdePerritos"
        [ (jugar, 30)
        , (ladrar 18, 20)
        , (regalar "pelota", 0)
        , (diaDeSpa, 120)
        , (diaDeCampo, 720)
        ]

perroPuedeEstar :: Perro -> Guarderia -> Bool
perroPuedeEstar perro guarderia =
    tiempo perro > sum (map snd (rutina guarderia))

perroResponsable :: Perro -> Bool
perroResponsable = (>3) . length . juguetesPreferidos . diaDeCampo
