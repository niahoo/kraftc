


crush a:Agriculteur(1) b:Ble(1)
      (delay: 10 - 5, x: delay/2, y:x *5) -> Farine(a.skill_agriculture / 10) {
    nutrition = (10 + a.skill_agriculture) * 2,
    conservation = (+ b.conservation 5),
    quality = 5 + a.skill_agriculture/3
} end
