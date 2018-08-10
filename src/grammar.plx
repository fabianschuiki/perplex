token IDENT;
token 'token';
token 'epsilon';
token '.';
token ':';
token ',';
token ';';
token '|';

desc : desc item | item | desc ';' | ';' ;
item : token_decl | rule_decl ;
token_decl : 'token' IDENT ';' ;
rule_decl : IDENT ':' rule_list ';' ;
rule_list : rule_list '|' sequence | sequence | rule_list '|' 'epsilon' | 'epsilon' ;
sequence : sequence IDENT | IDENT ;
