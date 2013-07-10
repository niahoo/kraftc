

crush Ble(1000) -> Farine(100) end
pain Farine(100) Eau(20) -> Pain(10) end

recolte -> Ble(100) end

pain Farine(x) Eau(x/100) Sel(10) -> Pain(x/10) end

crush Ble(x) (delay: x/1000) -> Farine(x/10) end

pain Farine(x) Eau(x/100) Sel((min 20 x/10)) -> Pain(x/10) end pain Farine(x) Eau(x/100) Sel((min (sqrt x) x/10)) -> Pain(x/10) end
pain Farine(x) Eau(x/100) Sel(3 + (min x 10)) -> Pain(x/10) end

pain Farine(x) Eau(x/100) Sel(1 + 2 + 3 + 4 + 5) -> Pain(x/10) end
pain Farine(x) Eau(x/100) Sel((+ 1 2 3 4 5)) -> Pain(x/10) end

crush Ble(x) (delay: x/1000)  -> Farine(x/10) {
    nutrition = 10, conservation = 5
    , quality = 5 , _prop = 3
} end

crush Ble(x) (delay: x/1000)  -> Farine(x/10) {
    nutrition = 10,
    conservation = x * 2,
    quality = (min x 10)
} end

crush a:Agriculteur(1) b:Ble(x) (delay: x/1000) -> Farine(x/10) {
    nutrition = (10 + a.skill_agriculture) * 2,
    conservation = (+ b.conservation 5),
    quality = 5 + a.skill_agriculture/3
} end

pain vp:VieuxPain(10) ->
    Pain(5) {quality = vp.quality - 2}
    Miettes(50)
end

crush b:Ble(x) (delay: x/1000) ->
    draw b.quality
            >> 10 -> Farine(x/9)
            >> 5 -> Farine(x/10)
            >> Void(0)
            >> 3 -> Farine(1)
end

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
end

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
        >> Void(0)
end

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random (max 5 p.skillAgricole) 100)
        >> 97 -> Farine(x/9)
        >> 5 -> Farine(x/10)
        >> Void(0)
end

crush p:Paysan() Ble(x) (delay: x/1000) ->
    draw (random p.skillAgricole 100)
        >> p.skillAgricole * 2 -> Farine(x/9)
        >> 5 -> draw (random 1 2)
                    >> 1 -> Farine(x/10)
                    >> Farine(x/15)
        >> Void(0)
end

crush p:Paysan(y) Ble(x) (delay: x/1000) ->
    draw y
        >> p.skillAgricole * 2 -> Farine(x/9)
        >> 5 -> draw (random 1 2)
                    >> 1 -> Farine(x/10)
                    >> Farine(x/15)
        >> Void(0)
end

recrutement Villageois() -> MaindOeuvre(1) end

revolution ps:Paysans() ->
   draw ps.faim
    >> 10 -> True()
    >> False()
end

