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
-- la quantité en sortie. Deux technics peuvent avoir le même nom mais
-- pas la même signature (les types d'item en entrée). Les noms de
-- types commencent par une majuscule.

crush Ble(1000) -> Farine(100) end
pain Farine(100) Eau(20) -> Pain(10) end

-- Les composants en entrée sont optionnels

recolte -> Ble(100) end


-- les technics ont aussi des métadonnées précisées entre parenthèses
-- après la définition des types, l'exemple le plus simple est le
-- temps pris par la technic (Si le temps n'est pas défini, il vaut
-- par défaut 0). Ces métadonnées sont transmises au compilateur afin
-- de définir des comportements de la technic. (Pour le moment seul
-- 'delay' est implémentée)

crush Ble(1) (delay: 1) -> Farine(1) end


-- Au lieu de stocker les propriétés des items dans une base de
-- données on peut vouloir que chaque item ait ses propriétés propres.
-- Selon les capacités du fabricant, un item peut être meilleur qu'un
-- autre tout en appartenant au même type. On peut donc donner les
-- propriétés de sortie en faisant suive la déclaration par une liste
-- de propriétés. Une liste de propriétés est entourée d'accolades {}
-- et associe des nom de propriété commençant par une minuscule (ou un
-- tiret de soulignement) à des expression via l'opérateur '=',
-- séparées par une virgule.

crush Ble(10) (delay: 1)  -> Farine(1) {
    nutrition = 10, conservation = 5
    , quality = 5 , _prop = 3
} end


-- Il est possible d'utiliser les propriétés des types en entrée pour
-- définir celles des types en sortie. Les propriétés des types en
-- entrée sont fournies par l'application appelante lors du lancement
-- d'une technic. Afin de faire référence aux propriétés d'un type, on
-- déclare une variable accolée au type ciblé via l'opérateur ':'. On
-- peut ensuite à partir de cette variable faire appel à l'une de ses
-- propriétés en faisant suivre la variable d'un point et du nom de la
-- propriété. Les propriétés commencent également par une minuscule
-- (ou un tiret de soulignement)

crush a:Agriculteur(1) b:Ble(1) (delay: 1) -> Farine(1) {
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

-- La quantité en sortie peut-être définie selon une variable

pain vp:VieuxPain(10) c:Cuisinier() ->
    Pain(c.skillCuisine / 5) {quality = vp.quality - 2}
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
-- Farine(9)

crush b:Ble(1) (delay: 1) ->
    draw b.quality
      -- Avec un blé de bonne qualité on obitient plus de farine
      >> 10 -> Farine(9)
      -- sinon on obtient x/10 Farine
      >> 5 -> Farine(10)
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



-- Une fonction mathématique s'écrit entre parenthèses, commencant par
-- le nom de la fonction suivi des arguments. Les arguments sont des
-- expressions et peuvent faire appel aux variables définies dans les
-- types précédents. La structure actuelle du compilateur exige de
-- doubler les parenthèses si la quantité de type est définie par un
-- seul appel de fonction

-- La fonction (random max min) permet de tirer au sort un nombre
-- entre un maximum et un minimum, (les deux sont inclus).
crush a:Agriculteur() Ble(10) (delay: 10) ->
    draw (random a.skill_agriculture 100)
        >> 97 -> Farine(10)
        >> 5 -> Farine(9)
end

-- Une technic doit toujours retourner un type. Dans le code
-- précédent, si a.skill_agriculture est inférieur à 5 il est possible
-- que le tirage random renvoie une valeur inférieure à 5. Dans ce
-- cas, Kraft rencontrera une erreur d'exécution. Il faut donc soit
-- spécifier un retour par défaut ...

crush a:Agriculteur() Ble(10) (delay: 10) ->
    draw (random a.skill_agriculture 100)
        >> 97 -> Farine(10)
        >> 5 -> Farine(9)
        >> Void(0)
end

-- ... ou s'assurer qu'un palier sera toujours sélectionné:

crush a:Agriculteur() Ble(10) (delay: 10) ->
    draw (random (max 5 a.skill_agriculture) 100)
        >> 97 -> Farine(10)
        >> 5 -> Farine(9)
        >> Void(0)
end

-- Il est possible d'utiliser des expressions pour les paliers ainsi
-- que d'imbriquer les draw-expressions

crush a:Agriculteur() Ble(10) (delay: 10) ->
    draw (random a.skill_agriculture 100)
        >> 90 -> Farine(10)
        >> 5 -> draw (random 1 2)
                    >> 1 -> Farine(9)
                    >> Farine(8)
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

revolution as:Agriculteurs() ->
   draw as.faim
    >> 10 -> True()
    >> False()
end
