token '(';
token ')';
token c;

A {
	A B ;
	B ;
}

B {
	c ;
	'(' A ')' ;
}
