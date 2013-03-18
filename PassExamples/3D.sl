(Int, (Int, Int)) transpose ((Int, (Int, Int))  p1, (Int, (Int, Int))  p2) {
	return ((fst(p1) + fst(p2)), (first(snd(p1)) + first(snd(p2)), snd(snd(p1)) + snd(snd(p2))));
}

(Int, Int) scale((Int, Int) p, Int scalar) {
	return (fst(p) * scalar, (first(snd(p)) * scalar, snd(snd(p)) * scalar));
}
