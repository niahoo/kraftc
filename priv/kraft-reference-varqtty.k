-- Kraft syntax reference

-- Ce document est un fichier kraft valide qui peut être donné au
-- compilateur pour être évalué. Les textes descriptifs sont des
-- commentaires : des lignes précédées de deux tirets. Ces
-- commentaires peuvent être insérés n'importe-où dans le code source
-- (en tenant compte du fait qu'ils courent jusqu'à la fin de la
-- ligne)

-- L'indentation du code n'a aucune importance lexicalement.

-- Pour déterminer une technic, il faut spécifier son nom, les types
-- de composants en entrée et les types de composants en sortie. Pour
-- chaque type, on précise entre parenthèses la quantité en entrée ou
-- la quantité en sortie. Deux technics peuvent avoir le même nom. Les
-- noms de types commencent par une majuscule.

crush Ble(1000) -> Farine(100) end
pain Farine(100) Eau(20) -> Pain(10) end

-- Les composants en entrée sont optionnels

recolte -> Ble(100) end

-- La quantité peut être exprimée sous forme variable pour un type et
-- la quantité des autres composants ou des éléments de sortie peut
-- être fonction de la quantité d'un composant.

pain Farine(x) Eau(x/100) Sel(10) -> Pain(x/10) end

-- les technics ont aussi des métadonnées précisées entre parenthèses
-- après la définition des types, l'exemple le plus simple est le
-- temps pris par la technic (Si le temps n'est pas défini, il vaut
-- par défaut 0). Ces métadonnées sont transmises au compilateur afin
-- de définir des comportements de la technic. (Pour le moment seul
-- 'delay' est implémentée)

crush Ble(x) (delay: x/1000) -> Farine(x/10) end

-- Certaines fonctions mathématiques sont disponibles afin de définir
-- les quantités de types. Si par exemple on veut limiter la quantité
-- maximale de sel nécessaire, on écrit le code suivant. Une fonction
-- mathématique s'écrit entre parenthèses, commencant par le nom de la
-- fonction suivi des arguments. Les arguments sont des expressions et
-- peuvent faire appel aux variables définies dans les types
-- précédents. La structure actuelle du compilateur exige de doubler
-- les parenthèses si la quantité de type est définie par un seul
-- appel de fonction


pain Farine(x) Eau(x/100) Sel((min 20 x/10)) -> Pain(x/10) end -- on double les parenthèses
pain Farine(x) Eau(x/100) Sel((min (sqrt x) x/10)) -> Pain(x/10) end
pain Farine(x) Eau(x/100) Sel(3 + (min x 10)) -> Pain(x/10) end

-- On peut également utiliser les opérateurs mathématiques sous forme
-- d'appel de fonctions, afin d'additionner plus simplement des
-- nombres. Les deux lignes suivantes sont équivalentes.

pain Farine(x) Eau(x/100) Sel(1 + 2 + 3 + 4 + 5) -> Pain(x/10) end
pain Farine(x) Eau(x/100) Sel((+ 1 2 3 4 5)) -> Pain(x/10) end


-- Au lieu de stocker les propriétés des items dans une base de
-- données on peut vouloir que chaque item ait ses propriétés propres.
-- Selon les capacités du fabricant, un item peut être meilleur qu'un
-- autre tout en appartenant au même type. On peut donc donner les
-- propriétés de sortie en faisant suive la déclaration par une liste
-- de propriétés. Une liste de propriétés est entourée d'accolades {}
-- et associe des nom de propriété commençant par une minuscule (ou un
-- tiret de soulignement) à des expression via l'opérateur '=',
-- séparées par une virgule.

crush Ble(x) (delay: x/1000)  -> Farine(x/10) {
    nutrition = 10, conservation = 5
    , quality = 5 , _prop = 3
} end

-- Il est possible de faire référence aux variables de quantité de
-- type dans une expression, tout comme il est possible d'utiliser des
-- fonctions mathématiques

crush Ble(x) (delay: x/1000)  -> Farine(x/10) {
    nutrition = 10,
    conservation = x * 2,
    quality = (min x 10)
} end

-- Cependant, définir les propriétés d'un type avec des nombres
-- constants est limité. Les définir à partir des quantités l'est
-- également. Il est donc possible d'utiliser les propriétés des types
-- en entrée pour définir celles des types en sortie. Les propriétés
-- des types en entrée sont fournies par l'application appelante lors
-- du lancement d'une technic. Afin de faire référence aux propriétés
-- d'un type, on déclare une variable accolée au type ciblé via
-- l'opérateur ':'. On peut ensuite à partir de cette variable faire
-- appel à l'une de ses propriétés en faisant suivre la variable d'un
-- point et du nom de la propriété. Les propriétés commencent
-- également par une minuscule (ou un tiret de soulignement)

crush a:Agriculteur(1) b:Ble(x) (delay: x/1000) -> Farine(x/10) {
    nutrition = (10 + a.skill_agriculture) * 2,
    conservation = (+ b.conservation 5),
    quality = 5 + a.skill_agriculture/3
} end

-- Le résultat d'une technic n'est pas un type, mais une liste de
-- types. Même si une technic retourne un seul type, l'application
-- appelante doit considérer cela comme une liste contenant un seul
-- élément. Il est donc possible de retourner plusieurs types en
-- sortie, chacun accompagné ou non de propriétés

pain vp:VieuxPain(10) ->
    Pain(5) {quality = vp.quality - 2}
    Miettes(50)
end


-- On voudra parfois comparer une valeur à plusieurs palier et définir
-- le type de sortie en fonction de cette valeur. Pour cela on utilise
-- la structure 'draw': On indique 'draw', suivi de l'expression à
-- comparer aux paliers puis, pour chaque palier, du symbole '>>', de
-- la valeur du palier, d'une flèche et du type résultant. On peut
-- également spécifier une clause sans palier qui sera automatiquement
-- sélectionnée si aucune clause précédente n'a été validée.

-- L'opérateur '>>' signifie que la valeur étudiée (ici b.quality)
-- doit être strictement supérieur au palier pour que la clause soit
-- validée. Ici, si la qualité du blé est 10 alors on obtient
-- Farine(x/10)

crush b:Ble(x) (delay: x/1000) ->
    draw b.quality
      -- Avec un blé de bonne qualité on obitient plus de farine
      >> 10 -> Farine(x/9)
      -- sinon on obtient x/10 Farine
      >> 5 -> Farine(x/10)
      -- en dessous on décide que le blé est trop mauvais, mais la
      -- technic doit quand même retourner un type. On peut donc
      -- renvoyer une quantité de 0: Farine(0). Mais du côté de
      -- l'application il peut être intéressant de gérer les échecs de
      -- toutes les technics de la même manière. On peut donc créer
      -- un type spécifique pour ce cas.
      >> Void(0)
      -- cette dernière clause est ignorée car la précédente est
      -- toujours valide
      >> 3 -> Farine(1)
end

-- La fonction (random max min) permet de tirer au sort un nombre
-- entre un maximum et un minimum, (les deux sont inclus).

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
end

-- Une technic doit toujours retourner un type. Dans le code
-- précédent, si p.skillAgricole est inférieur à 5 il est possible que
-- le tirage random renvoie une valeur inférieure à 5. Dans ce cas,
-- Kraft rencontrera une erreur d'exécution. Il faut donc soit
-- spécifier un retour par défaut ...

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
        >> Void(0)
end

-- ... ou s'assurer qu'un palier sera toujours sélectionné:

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random (max 5 p.skillAgricole) 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
        >> Void(0)
end

-- Il est possible d'utiliser des expressions pour les paliers ainsi
-- que d'imbriquer les draw-expressions

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> p.skillAgricole * 2 -> Farine(x/9)
        >> 5 -> draw (random 1 2)
                    >> 1 -> Farine(x/10)
                    >> Farine(x/15)
        >> Void(0)
end

-- Les quantités des types sont optionnelles. Par convention, un type
-- sans quantité représente un service, c'est à dire une ressource
-- potentiellement infinie mais qui doit être présente.

recrutement Villageois() -> MaindOeuvre(1) end

-- Un résultat sans quantité représente un "switch on", c'est à dire
-- qu'il est  censé activer un service dans l'application appelante.
-- C'est une simple convention qui peut être détournée, on peut par
-- exemple vouloir retourner vrai ou faux en fonction d'une variable

revolution ps:Paysans() ->
   draw ps.faim
    >> 10 -> True()
    >> False()
end

