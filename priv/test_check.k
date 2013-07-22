
crush Agriculteur() Ble(10) ->
    draw 3
        >> 97 -> Farine(10)
        >> 10 ->
            draw 3
                >> 97 -> Farine(10)
                >> otherwise -> Haha()
                >> 1 -> Puuute()
            end
        >> otherwise -> Haha()
        >> 1 -> Lolilo()
    end
end

