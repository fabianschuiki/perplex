token A => `Token::A`, `String`;
token end => `Token::End`;

root => `Root` {
	A ;
	root A ;
}
