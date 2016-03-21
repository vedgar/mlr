import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

data Atom = At Char (Maybe Integer)
    deriving (Eq, Ord)

instance Show Atom where
    show (At p Nothing) = [p]
    show (At p (Just i)) = [p] ++ show i

data Connective = Conn Integer ([Bool] -> Bool)

logconst b = Conn 0 (\ [] -> b)
unary op = Conn 1 (\ [x] -> op x)
binary op = Conn 2 (\ [x, y] -> x `op` y)

truec = logconst True
falsec = logconst False
notc = unary not
andc = binary (&&)
orc = binary (||)
condc = binary (\ x y -> not x || y)
bicondc = binary (==)

table (Conn 0 f) = [f []]
table (Conn n f) = [b | start <- [False, True],
    b <- table (Conn (n-1) (\ rest -> f([start] ++ rest)))]

instance Eq Connective where
    c1 == c2 = table c1 == table c2

data Formula = Atomic Atom
    | Compound Connective [Formula]
    deriving (Eq)

p = Atomic (At 'p' Nothing)
q = Atomic (At 'q' Nothing)
p1 = Atomic (At 'p' (Just 1))
p2 = Atomic (At 'p' (Just 2))
p3 = Atomic (At 'p' (Just 3))

f & g = Compound andc [f, g]
ne f = Compound notc [f]
true = Compound truec []
false = Compound falsec []
f ||| g = Compound orc [f, g]
f --> g = Compound condc [f, g]
f <-> g = Compound bicondc [f, g]

instance Show Connective where
    show c | c == notc = "!"
           | c == andc = "&"
           | c == orc = "|"
           | c == condc = "->"
           | c == bicondc = "<->"
           | otherwise = error ("Unnamed operator with table "
                                ++ show (table c))

instance Show Formula where
    show (Atomic p) = show p
    show (Compound c@(Conn 0 b) []) = show (b [])
    show (Compound c@(Conn 1 _) [f]) = show c ++ show f
    show (Compound c@(Conn 2 _) [f, g]) = "("
        ++ show f ++ show c ++ show g ++ ")"

		
vars (Atomic p) = Set.singleton p
vars (Compound _ fs) = Set.unions (map vars fs)


complexity (Atomic p) = 0
complexity (Compound _ fs) = 1 + sum (map complexity fs)

type InterpAtomic = Atom -> Bool
interp :: InterpAtomic -> Formula -> Bool
interp i (Atomic p) = i p
interp i (Compound (Conn _ c) fs) = c (map (interp i) fs)


{- npr. > let i at = at == At 'p' Nothing
sad je i interpretacija koja je na p True, na svim ostalima False
> interp i (q --> p) -}



-- DZ1

-- 1. popravak heighta
----------------------
height (Atomic p) = 1
height (Compound _ fs) = 1 + foldl max 0 (map height fs)



-- 2. parcijalna interpretacija
-------------------------------

-- djelovanje svakog testa:{valjana,proturjecna,oboriva,ispunjiva} nad svakom formulom:{p ||| ne p,   p | q,   p & ne p, false}
ispisZad2 = putStrLn $ unwords [ "\n" ++ (show primjer) ++ "\t" ++ (fst test) ++":\t"++ (show $ snd test primjer) | 	
									primjer <-[	p ||| (ne p)	, p |||  q ,	p & (ne p), false	],
									test 	<-[("valjana",valjana),("proturjecna",proturjecna),("oboriva",oboriva),("ispunjiva",ispunjiva)] ] 

primjerNePraznaInterp			= interpPartial (\at -> if at == (At 'p' Nothing) then Just True else Nothing )	[(p ||| q)]



type InterpAtomicPartial = Atom -> Maybe Bool

interpPartial :: InterpAtomicPartial -> [Formula] -> (Set.Set Bool)
interpPartial i' fs = Set.fromList [ interp i f | f<-fs,i <- prosirenja i' (Set.toList $ vars f) ] 



prosirenja :: InterpAtomicPartial -> [Atom] -> [InterpAtomic]
prosirenja i' [] 		=  [(\at -> Maybe.fromJust $ i' at)]
prosirenja i' (at:ats)	| i' at == Nothing 		=  	(prosirenja (\at2 -> if(at2 == at) then Just True  else i' at2) ats)  ++ 
													(prosirenja (\at2 -> if(at2 == at) then Just False else i' at2) ats)
						| otherwise				=  	prosirenja i' ats

						
						
valjana :: 			Formula -> Bool;		valjana f 		= interpPartial (\_ -> Nothing) [f] 	== 	Set.singleton True
proturjecna :: 		Formula -> Bool;		proturjecna f 	= interpPartial (\_ -> Nothing) [f] 	== 	Set.singleton False
oboriva :: 			Formula -> Bool; 		oboriva 		= not . valjana
ispunjiva :: 		Formula -> Bool; 		ispunjiva 		= not . proturjecna

 
 
 
 
-- 4. ideja: djelovanje sa svakom kombinacijom veznika truec, falsec, notc, bicondc nad stupcima
-- tablice istinitosti koja u pocetku sadrzi 2 stupca(vrijednosti svih interpretacija nad varijablama A i B).
-- Djeluje se tako dugo dok ni jedan veznik ne generira novi stupac
-- ako je broj svih mogucih razlicitih izgeneriranih stupaca jednak (2^2)^2=16 tada su ti veznici dovoljni
-- zakljucak: nisu dovoljni

--rjesenje: 
ispisZad4 				= finiIspis $ djelovanje [stupacA,stupacB] [tempFalsec,tempTruec,tempNotc,tempBicondc]

--primjer djelovanja sa dovoljnim veznikom "nand"
ispisDovoljnogVeznika 	= finiIspis $ djelovanje [stupacA,stupacB] [nand]

--primjer djelovanja sa "And"
ispisDjelovanjaVeznikaAnd 	= finiIspis $ djelovanje [stupacA,stupacB] [Conn 2 (\xs -> foldl (&&) True xs)]

--primjer djelovanja sa "And" i "Neg"
ispisDjelovanjaVeznikaAndINeg 	= finiIspis $ djelovanje [stupacA,stupacB] [Conn 2 (\xs -> foldl (&&) True xs),tempNotc]


--redefinicija falsec,truec,not,bicond jer su gornje definicije fiksirane za mjesnost npr falsec= \[] -> False  ili bicond = \[x,y] -> x==y
tempFalsec 	= Conn 0 (\_ -> False)
tempTruec	= Conn 0 (\_ -> True)
tempNotc	= Conn 1 (\xs -> foldl (\a x -> not x) False xs)  -- vraca negaciju zadnjeg parametra, nema smisla za vise id 1 parametra, ali definirano
tempBicondc	= Conn 2 (\xs -> (length $ List.nub xs) == 1 ) -- [False,False] , [True,True] ili [True,True,True] nije tocno ali je definirano za mjesnosti != 2

nand 		= Conn 2 (\xs -> not (foldl (&&) True xs))		

type Stupac 	= [Bool]
brojRedaka = 4
stupacB::Stupac;			stupacB		= [False,True,False,True]	--vrijednosti varijable A po svim interpretacija nad A i B
stupacA::Stupac;			stupacA		= [False,False,True,True] 	--vrijednosti varijable B po svim interpretacija nad A i B

 
djelovanje :: [ Stupac ] -> [ Connective ] -> [ Stupac ]
djelovanje stupacs vezniks 	| (length newStupacs) == (length stupacs) 	= stupacs
							| otherwise									= djelovanje newStupacs vezniks
	where 
	newStupacs 	= List.nub $ stupacs ++ ( foldl (++) [] ( map djelovanjeVeznika vezniks ) )
	djelovanjeVeznika :: Connective -> [ Stupac ]
	djelovanjeVeznika (Conn k f) 	| k==0 		= [ take brojRedaka $ repeat (f []) ]
									| otherwise = [ map f (List.transpose stupacVector) | stupacVector <- sequence $ take (fromInteger k :: Int) $ repeat stupacs ] -- djelovanje sa k-veznikom po svakoj k-torci stupaca
						
finiIspis :: [Stupac] -> [String]
finiIspis xs =  map (\stupac ->  map (\b -> if b==True then '1' else '0' ) stupac) xs

