// This is an extract of the SystemVerilog syntax.
token IDENT => `Token::Ident(_)`, `String`;
token end => `Token::End`;

root => `Root` {
	IDENT IDENT ;
}
