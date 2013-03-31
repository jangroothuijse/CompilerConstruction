Bool equals([a] a, [a] b) {
	if (isEmpty(a) && isEmpty(b)) return True;
	if (isEmpty(a) || isEmpty(b)) return False;
	return (hd(a) == hd(b)) && equals (tail(a), tail(b));	
}
