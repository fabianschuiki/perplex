token IDENT     => `Some(Token::Ident(_))`;
token CODE      => `Some(Token::Code(_))`;
token 'token'   => `Some(Token::Keyword(Keyword::Token))`;
token 'epsilon' => `Some(Token::Keyword(Keyword::Epsilon))`;
token 'end'     => `Some(Token::Keyword(Keyword::End))`;
token '('       => `Some(Token::LParen)`;
token ')'       => `Some(Token::RParen)`;
token '{'       => `Some(Token::LBrace)`;
token '}'       => `Some(Token::RBrace)`;
token '.'       => `Some(Token::Period)`;
token ':'       => `Some(Token::Colon)`;
token ','       => `Some(Token::Comma)`;
token ';'       => `Some(Token::Semicolon)`;
token '|'       => `Some(Token::Pipe)`;
token '?'       => `Some(Token::Question)`;
token '*'       => `Some(Token::Star)`;
token '+'       => `Some(Token::Plus)`;
token '='       => `Some(Token::Eq)`;
token '=>'      => `Some(Token::RArrow)`;
token end       => `None`;

desc => `ast::Desc` {
	desc item => `reduce_desc_a`;
	item      => `reduce_desc_b`;
	desc ';'  => `reduce_desc_c`;
	';'       => `reduce_desc_d`;
}

item => `ast::Item` {
	token_decl                 => `reduce_item_a`;
	rule_decl                  => `reduce_item_b`;
}

token_decl => `ast::TokenDecl` {
	'token' token_name '=>' CODE ';' => `reduce_token_decl_a`;
	'token' token_name ';'           => `reduce_token_decl_b`;
}

token_name => `ast::TokenName` {
	IDENT => `reduce_token_name_a`;
	'end' => `reduce_token_name_b`;
}

rule_decl => `ast::RuleDecl` {
	IDENT '=>' CODE '{' rule_list '}' => `reduce_rule_decl_a`;
	IDENT '{' rule_list '}'           => `reduce_rule_decl_b`;
}

rule_list => `Vec<ast::Variant>` {
	rule_list variant  => `reduce_rule_list_a`;
	variant            => `reduce_rule_list_b`;
}

variant => `ast::Variant` {
	sequence_or_epsilon '=>' CODE ';' => `reduce_variant_a`;
	sequence_or_epsilon ';'           => `reduce_variant_b`;
}

sequence_or_epsilon => `Vec<ast::Symbol>` {
	sequence  => `reduce_sequence_or_epsilon_a`;
	'epsilon' => `reduce_sequence_or_epsilon_b`;
}

sequence => `Vec<ast::Symbol>` {
	sequence symbol => `reduce_sequence_a`;
	symbol          => `reduce_sequence_b`;
}

symbol => `ast::Symbol` {
	core_symbol           => `reduce_symbol_a`;
	core_symbol ':' IDENT => `reduce_symbol_b`;
}

core_symbol => `ast::Symbol` {
	primary_symbol          => `reduce_core_symbol_a`;
	primary_symbol '?'      => `reduce_core_symbol_b`;
	repetition_sequence '*' => `reduce_core_symbol_c`;
	repetition_sequence '+' => `reduce_core_symbol_d`;
}

repetition_sequence => `ast::RepSequence` {
	primary_symbol                => `reduce_repetition_sequence_a`;
	'(' sequence ';' sequence ')' => `reduce_repetition_sequence_b`;
}

primary_symbol => `ast::Symbol` {
	IDENT            => `reduce_primary_symbol_a`;
	'(' sequence ')' => `reduce_primary_symbol_b`;
}
