all: lex.yy.c
	gcc -o checker lex.yy.c -lfl

lexer.yy.c: checker.l
	flex checker.l 
	
