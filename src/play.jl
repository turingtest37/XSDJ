
mixed = [rand() > 0.5 ? "A" : 1.0 for _ = 1:20]
s = filter(v->isa(v,String), mixed)
n = filter(v->isa(v,Number), mixed)