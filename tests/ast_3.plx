token A => `Token::A`, `String`;
token B => `Token::B`, `usize`;
token 'var' => `Token::Var`;
token ';' => `Token::Semicolon`;
token end => `Token::End`;

root => `Root` {
	'var' A ';' ;
	'var' B ';' ;
	'var' (A B)+ ';' ;
}
