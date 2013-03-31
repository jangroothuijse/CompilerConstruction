// SPL has overloading?

Int sum([Int] list) {
	if (isEmpty(list)) return 0;	
	return hd(list) + sum(tl(list));
}

Int product([Int] list) {
	if (isEmpty(list)) return 1;	
	return hd(list) * sum(tl(list));
}

Bool sum([Bool] list) {
	if (isEmpty(list)) return False;	
	return hd(list) || sum(tl(list));
}

Bool product([Bool] list) {
	if (isEmpty(list)) return True;	
	return hd(list) && sum(tl(list));
}
