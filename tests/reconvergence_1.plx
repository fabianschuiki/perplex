// In this grammar the `T` causes the parse to diverge, since `T->A` and
// `T->B` are both valid interpretations. It reconverges upon `y` which
// is common for both branches. This is an example of local ambiguity.

token x;
token y;
token z;

S {	T y z ; }
T { A; B; }
A { x; }
B { x; }
