
crush Agriculteur() Ble(10) ->
    draw 3
        >> 10 ->
            draw 3
                >> 97 -> Farine(10) Suck()
                >> otherwise -> Suck() Farine()
                >> 1 -> Ooo()
            end
        >> otherwise -> Haha()
    end
end

