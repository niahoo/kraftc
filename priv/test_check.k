crush a:Agriculteur(1) b:Ble(1) (delay: 1) -> Farine(1)  end


pain c:Cuisinier() vp:VieuxPain(10) ->
    draw (random c.skillCooking 100)
        >> 97 -> Pain(2)
        >> 5 -> Farine(1)
        >> Void(0)
end

pain Farine(100) Eau(20) -> Pain(10) end


