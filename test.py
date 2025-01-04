v = [
    130,
    122,
    119,
    142,
    136,
    127,
    120,
    152,
    141,
    132,
    127,
    118,
    150,
    141,
    133,
    137,
    129,
    142
]
mean = sum(v)/18
total = 0
for v_ in v:
    total = total + (float(v_)- mean)**2
s_binh = 1/17*total
s = s_binh**(1/2)
print()