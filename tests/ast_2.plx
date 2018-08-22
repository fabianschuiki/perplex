// This is an extract of the SystemVerilog syntax.
token IDENT;
token '(';
token ')';
token ',';
token '.';

let_expr {
	IDENT:ident ('(' let_args:args ')')?:args ;
}

let_args {
	epsilon ;
	let_actual_args:actual ;
	let_ident_args:ident ;
	// let_actual_args:actual ',' let_ident_args:ident ; // CONFLICT
}

let_actual_args {
	(let_actual_arg:arg; ',')+:args ;
}

let_ident_args {
	(let_ident_arg:arg; ',')+:args ;
}

let_actual_arg {
	IDENT:name ;
	// expr
}

let_ident_arg {
	'.' IDENT:name '(' let_actual_arg?:arg ')' ;
}
