data Animal = Monyet | Kodok | Burung | Ular deriving (Eq, Show)

(⋆) :: Animal -> Animal -> Animal
Monyet ⋆ Kodok = Burung
Kodok ⋆ Monyet = Burung
Burung ⋆ Monyet = Kodok
Ular ⋆ _ = Ular
_ ⋆ Ular = Ular
_ ⋆ _ = Burung

identity :: Animal
identity = Ular

inverse :: Animal -> Animal
inverse Monyet = Kodok
inverse Kodok = Monyet
inverse Burung = Burung
inverse Ular = Ular

isGroup :: Bool
isGroup =
  all (\a -> a ⋆ identity == identity ⋆ a && a ⋆ inverse a == inverse a ⋆ a) [Monyet, Kodok, Burung, Ular]

main :: IO ()
main = do
  putStrLn "Group Operations on {Monyet, Kodok, Burung, Ular}"
  putStrLn "--------------------------------------------"

  putStrLn "Binary Operation (⋆):"
  putStrLn $ "Monyet ⋆ Kodok = " ++ show (Monyet ⋆ Kodok)
  putStrLn $ "Kodok ⋆ Monyet = " ++ show (Kodok ⋆ Monyet)
  putStrLn $ "Burung ⋆ Monyet = " ++ show (Burung ⋆ Monyet)
  putStrLn $ "Ular ⋆ Monyet = " ++ show (Ular ⋆ Monyet)

  putStrLn $ "Associativity: " 
  putStrLn $ "(Monyet ⋆ Kodok) ⋆ Burung = " ++ show ((Monyet ⋆ Kodok) ⋆ Burung)
  putStrLn $ "Monyet ⋆ (Kodok ⋆ Burung) = " ++ show (Monyet ⋆ (Kodok ⋆ Burung))

  putStrLn "\nIdentity Element:"
  putStrLn $ "identity = " ++ show identity
  putStrLn $ "(Ular ⋆ Monyet) = " ++ show (Ular ⋆ Monyet)
  putStrLn $ "(Kodok ⋆ Ular) = " ++ show (Kodok ⋆ Ular)
  putStrLn $ "(Ular ⋆ Ular) = " ++ show (Ular ⋆ Ular)

  putStrLn "\nInverse Elements:"
  putStrLn $ "inverse Monyet = " ++ show (inverse Monyet)
  putStrLn $ "inverse Kodok = " ++ show (inverse Kodok)
  putStrLn $ "inverse Burung = " ++ show (inverse Burung)
  putStrLn $ "inverse Ular = " ++ show (inverse Ular)

  putStrLn "\nIs it a Group?"
  putStrLn $ "Answer: " ++ if isGroup then "Yes" else "No"
