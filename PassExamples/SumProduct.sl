// SPL has overloading?

Int sum([Int] list) {
	if (isEmpty list) return 0;	
	return head(list) + sum(tail(list))
}

Int product([Int] list) {
	if (isEmpty list) return 1;	
	return head(list) * sum(tail(list))
}

Bool sum([Bool] list) {
	if (isEmpty list) return False;	
	return head(list) || sum(tail(list))
}

Bool product([Bool] list) {
	if (isEmpty list) return True;	
	return head(list) && sum(tail(list))
}
