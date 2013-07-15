crush a:Agriculteur(1) b:Ble(1) (delay: 1 - b.durete) -> Farine(a.skill_agriculture / 10) {
    nutrition = (10 + a.skill_agriculture) * 2,
    conservation = (+ b.conservation 5),
    quality = 5 + a.skill_agriculture/3
} end
