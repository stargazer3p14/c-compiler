/*
 *	LEXER.C
 *	-------
 *
 *	Lexical (tokens) analyzer and parser for C.
 *	Reference - the ISO C 9899:1999 (C99) Standard.
 *
 *	The lexical parser is based on chapter 6.4 of the ISO C Standard (with the exceptions of unsupported tokens). 
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"lexer.h"


extern	int	err_count;

list_t	*curr_file;
int	line = 1, pos = 1;
static char	*error[] = { "Bad token", "Bad comment", "Unmatched string" };
char	*glob_src, *src;


void	*alloc( size_t size )
{
	void	*p;

	p = malloc( size );
	//p = GlobalLock( GlobalAlloc( 0, size ) );

	if ( NULL == p )
	{
		fprintf( stderr, "Out of memory\n" );
		abort();
	}

	return	p;
}


static	void	report_lex_error( int	err_num )
{
	fprintf( stderr, "%s: Lexical error on line %d (%d): %s('%c')\n", curr_file->item->token, line, pos, error[ err_num ], *src );

	++err_count;
}


static char	*res_word[] = { "if", "while", "for", "do", "switch", "struct", "union",
				"enum", "typedef", "int", "long", "unsigned", "float", "double", "char",
				"case", "break", "continue", "break", "goto", "default", "void", "sizeof",
				"auto", "const", "else", "extern", "inline", "register", "restrict",
				"return", "short", "signed", "static", "volatile", "__asm", "__endasm", "defined" };

static int	res_id[] = { IF, WHILE, FOR, DO, SWITCH, STRUCT, UNION, ENUM, TYPEDEF, INT,
				LONG, UNSIGNED, FLOAT, DOUBLE, CHAR, CASE, BREAK, CONTINUE, BREAK, GOTO,
				DEFAULT, VOID, SIZEOF, AUTO, CONST, ELSE, EXTERN, INLINE, REGISTER,
				RESTRICT, RETURN, SHORT, SIGNED, STATIC, VOLATILE, ASM, ENDASM, DEFINED };

/*
 *	Returns token adequate for longest string match.
 */
token_t	*get_token( int flags )	// char *src )
{
	int	i = 0;
	token_t	*tok;
	int	j;
	size_t	sz;

	if ( src[ 0 ] == 0 )
		return	NULL;

	tok = alloc( sizeof( *tok ) );

	/*
	 * Choose one of automata to parse the token
	 */

	//	Number that starts with '0' (octal/hex/0)
	if ( src[ 0 ] == '0' )
	{
		if ( src[ 1 ] == 'x' )
		{
			for ( i = 2; ishex( src[ i ] ); ++i )
				;
			tok -> id = NUM_HEX;
		}
		else if ( isoct( src[ 1 ] ) )
		{
			for ( i = 2; isoct( src[ i ] ); ++i )
				;
			tok -> id = NUM_OCT;
		}
		else
		{
			tok -> id = NUM;
			i = 1;
		}

		// Add suffixes.
		if ( ( src[ i ] == 'u' || src[ i ] == 'U' ) && 
			( src[ i + 1 ] == 'l' || src[ i ] == 'L' ) || 
			( src[ i ] == 'l' || src[ i ] == 'L' ) &&
			( src[ i + 1 ] == 'l' || src[ i ] == 'U' ) )
				i += 2;
		else if ( src[ i ] == 'u' || src[ i ] == 'U' || 
			src[ i ] == 'l' || src[ i ] == 'L' )
				++i;
	}

	//	Number (int or real)
	else if ( isnum( src[ 0 ] ) )
	{
		tok -> id = NUM;
		for ( i = 1; isnum( src[ i ] ); ++i )
			;

		if ( src[ i ] == '.' )
		{
			++i;
			while ( isnum( src[ i ] ) )
				++i;
			tok -> id = REAL;

			if ( src[ i ] == 'e' || src[ i ] == 'E' )
				goto	exp_form;
		}
		else if ( src[ i ] == 'e' || src[ i ] == 'E' )
		{
exp_form:
			++i;
			if ( src[ i ] == '+' || src[ i ] == '-' )
				++i;
			while ( isnum( src[ i ] ) )
				++i;
			tok -> id = REAL;
		}
		else
		{
			// It is a num, add suffixes.
			if ( ( src[ i ] == 'u' || src[ i ] == 'U' ) && 
				( src[ i + 1 ] == 'l' || src[ i ] == 'L' ) || 
				( src[ i ] == 'l' || src[ i ] == 'L' ) &&
				( src[ i + 1 ] == 'l' || src[ i ] == 'U' ) )
					i += 2;
			else if ( src[ i ] == 'u' || src[ i ] == 'U' || 
				src[ i ] == 'l' || src[ i ] == 'L' )
					++i;
		}
	}

	// ID or keyword
	else if ( isalunder( src[ 0 ] ) )
	{
		tok -> id = ID;
		for ( i = 1; is_alnum( src[ i ] ); ++i )
			;

		// Check for reserved words

		for ( j = 0; j < sizeof( res_id ) / sizeof( int ); ++j )
		{
			if ( sz = strlen( res_word[ j ] ) )
				if ( !strncmp( src, res_word[ j ], sz ) && !is_alnum( src[ sz ] ) )
					tok -> id = res_id[ j ];

			// 'defined' is a keyword only in preprocessor conditional directives.
			if ( tok->id == DEFINED && !( flags & FL_DEFINED_KEYWD ) )
				tok->id = ID;
		}
	}
	
	// White-space
	else if ( is_wspace( src[ 0 ] ) )
	{
		tok -> id = WSPACE;
		for ( i = 1; is_wspace( src[ i ] ); ++i )
			;
	}

	// C-style comment
	else if ( src[ 0 ] == '/' && src[ 1 ] == '*' )
	{
		for ( i = 2; src[ i ] != 0 && src[ i + 1 ] != 0 && !( src[ i ] == '*' && src[ i + 1 ] == '/' ); ++i )
			;

		if ( src[ i ] == 0 || src[ i + 1 ] == 0 )
		{
			free( tok );
			report_lex_error( BAD_COMMENT );
			return	NULL;
		}

		i += 2;
		tok -> id = WSPACE;
	}

	// C++-style comment
	else if ( src[ 0 ] == '/' && src[ 1 ] == '/' )
	{
		for ( i = 0; src[ i ] != '\n' && src[ i ] != 0; ++i )
			;
		tok -> id = WSPACE;
	}

	// Non-alphanumeric characters (operators, parentheses etc.)
	else
	{
		switch ( src[ 0 ] )
		{
		default:
			free( tok );
			if ( src[ 0 ] != 0 )
				report_lex_error( BAD_TOKEN );
			return	NULL;
		case '(':
			tok -> id = LPAREN;
			i = 1;
			break;
		case ')':
			tok -> id = RPAREN;
			i = 1;
			break;
		case '{':
			tok -> id = LFBRACE;
			i = 1;
			break;
		case '}':
			tok -> id = RFBRACE;
			i = 1;
			break;
		case '[':
			tok -> id = LSUBSCR;
			i = 1;
			break;
		case ']':
			tok -> id = RSUBSCR;
			i = 1;
			break;
		case '<':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = LESSEQ;
				i = 2;
			}
			else if ( src[ 1 ] == '<' )
			{
				if ( src[ 2 ] == '=' )
				{
					tok -> id = SHLASSIGN;
					i = 3;
				}
				else
				{
					tok -> id = SHL;
					i = 2;
				}
			}
			else
			{
				tok -> id = LESS;
				i = 1;
			}
			break;
		case '>':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = MOREEQ;
				i = 2;
			}
			else if ( src[ 1 ] == '>' )
			{
				if ( src[ 2 ] == '=' )
				{
					tok -> id = SHRASSIGN;
					i = 3;
				}
				else
				{
					tok -> id = SHR;
					i = 2;
				}
			}
			else
			{
				tok -> id = MORE;
				i = 1;
			}
			break;
		case ':':
			tok -> id = COLON;
			i = 1;
			break;
		case ';':
			tok -> id = EOSTM;
			i = 1;
			break;
		case '~':
			tok -> id = BNOT;
			i = 1;
			break;
		case '!':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = NEQ;
				i = 2;
			}
			else
			{
				tok -> id = LNOT;
				i = 1;
			}
			break;
		case '#':
			tok -> id = PPROC;
			i = 1;
			break;
		case '$':
			tok -> id = DOLLAR;
			i = 1;
			break;
		case '%':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = MODASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = MOD;
				i = 1;
			}
			break;
		case '^':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = XORASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = XOR;
				i = 1;
			}
			break;
		case '&':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = ANDASSIGN;
				i = 2;
			}
			else if ( src[ 1 ] == '&' )
			{
				tok -> id = LAND;
				i = 2;
			}
			else
			{
				tok -> id = AND;
				i = 1;
			}
			break;
		case '*':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = MULASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = STAR;
				i = 1;
			}
			break;
		case '-':
			if ( src[ 1 ] == '-' )
			{
				tok -> id = MINUSMINUS;
				i = 2;
			}
			else if ( src[ 1 ] == '>' )
			{
				tok -> id = DEREF;
				i = 2;
			}
			else if ( src[ 1 ] == '=' )
			{
				tok -> id = MINUSASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = MINUS;
				i = 1;
			}
			break;
		case '+':
			if ( src[ 1 ] == '+' )
			{
				tok -> id = PLUSPLUS;
				i = 2;
			}
			else if ( src[ 1 ] == '=' )
			{
				tok -> id = PLUSASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = PLUS;
				i = 1;
			}
			break;
		case '\\':
			tok -> id = BACKSLASH;
			i = 1;
			break;
		case '|':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = ORASSIGN;
				i = 2;
			}
			else if ( src[ 1 ] == '|' )
			{
				tok -> id = LOR;
				i = 2;
			}
			else
			{
				tok -> id = OR;
				i = 1;
			}
			break;
		case '\'':
			tok -> id = CHCONST;
			i = 1;
			if ( src[ i ] == '\'' || src[ i ] == '\n' )
			{
				// Error: missing character in constant
			}
			else if ( src[ i ] == '\\' )
			{
				// Escape sequence
				++i;
				if ( isoct( src[ i ] ) )
				{
					while ( isoct( src[ i ] ) )
						++i;
				}
				else if ( src[ i ] == 'x' )
				{
					++i;
					while ( ishex( src[ i ] ) )
						++i;
				}
				else if ( src[ i ] == '\'' || src[ i ] == '\?' || src[ i ] == '\"' || src[ i ] == 'a' ||
					src[ i ] == 'b' || src[ i ] == 'f' || src[ i ] == 'n' || src[ i ] == 'r' || src[ i ] == 't' || src[ i ] == 'v' )
				{
					++i;
				}
				else
				{
					// Error: bad escape sequence
				}
			}
			else
			{
				++i;
			}

			// If error didn't happen
			if ( src[ i ] != '\'' )
			{
				// Error: bad symbols in character constant
			}
			++i;
			break;
		case '\"':
			for ( i = 1; src[ i ] != '\"'; )
			{
				char	*sss;

				sss = src + i;

				if ( src[ i ] == 0 )
				{
					// Error: unterminated string literal
				}
				if ( src[ i ] == '\n' )
				{
					// Error: newline in string literal
				}
				else if ( src[ i ] == '\\' )
				{
					// Escape sequence
					++i;
					if ( isoct( src[ i ] ) )
					{
						while ( isoct( src[ i ] ) )
							++i;
					}
					else if ( src[ i ] == 'x' )
					{
						++i;
						while ( ishex( src[ i ] ) )
							++i;
					}
					else if ( src[ i ] == '\'' || src[ i ] == '\?' || src[ i ] == '\"' || src[ i ] == 'a' ||
						src[ i ] == 'b' || src[ i ] == 'f' || src[ i ] == 'n' || src[ i ] == 'r' || src[ i ] == 't' || src[ i ] == 'v' )
					{
						++i;
					}
					else
					{
						// Error: bad escape sequence
					}
				}
				else
				{
					++i;
				}
			}
			++i;
			tok -> id = STRCONST;
			break;
		case '/':
			if ( src[ 1 ] == '=' )
			{
				tok -> id = DIVASSIGN;
				i = 2;
			}
			else
			{
				tok -> id = DIV;
				i = 1;
			}
			break;
		case '?':
			tok -> id = QUEST;
			i = 1;
			break;
		case ',':
			tok -> id = COMMA;
			i = 1;
			break;
		case '.':
			tok -> id = MEMBERACC;
			i = 1;
			if ( src[ 1 ] == '.' && src[ 2 ] == '.' )
			{
				tok->id = ELLIPSIS;
				i = 3;
			}
			break;
		case '=':
			tok -> id = ASSIGN;
			i = 1;
			if ( src[ 1 ] == '=' )
			{
				tok -> id = EQ;
				i = 2;
			}
			break;
		}
	}

	tok -> token = alloc( i + 1 );
	memmove( tok -> token, src, i );
	tok -> token[ i ] = '\0';
	return	tok;
}


char	*token[] = { "ID", "NUM", "NUM_OCT", "NUM_HEX", "REAL", "COMMA", "LPAREN", "RPAREN", "LFBRACE",
	"RFBRACE", "LSUBSCR", "RSUBSCR", "SEMICOLON", "EOSTM", "WSPACE", "BNOT", "LNOT", "PPROC", "DOLLAR", "MOD",
	"XOR", "AND", "STAR", "MINUS", "PLUS", "EQ", "OR", "BACKSLASH", "DIV", "QUEST", "LESS", "MORE",
	"IF", "WHILE", "FOR", "DO", "SWITCH", "STRUCT", "UNION", "ENUM", "TYPEDEF", "INT", "LONG", "UNSIGNED",
	"FLOAT", "DOUBLE", "CHAR", "ASSIGN", "CHCONST", "STRCONST", "CASE", "BREAK", "CONTINUE", "DEFAULT", "GOTO",
	"VOID", "SIZEOF", "PLUSPLUS", "MINUSMINUS", "DEREF", "MEMBERACC", "AUTO", "CONST", "ELSE", "EXTERN", "INLINE",
	"REGISTER", "RESTRICT", "RETURN", "SHORT", "SIGNED", "STATIC", "VOLATILE",
	"SHL", "SHR", "NEQ", "MOREEQ", "LESSEQ", "MODASSIGN", "MULASSIGN", "DIVASSIGN", "PLUSASSIGN", "MINUSASSIGN",
	"SHLASSIGN", "SHRASSIGN", "ANDASSIGN", "ORASSIGN", "XORASSIGN", "LAND", "LOR" };


#if 0
int	main( int argc, char **argv )
{
	FILE *f;
	unsigned long	flen;
	list_t	*token_list = NULL, *tl1 = NULL;
	token_t	*tok;

	f = fopen( argv[ 1 ], "rt" );
	fseek( f, 0, SEEK_END );
	flen = ftell( f );
	fseek( f, 0, SEEK_SET );
	glob_src = src = alloc( flen + 1 );
	memset( src, 0, flen + 1 );
	fread( src, 1, flen, f );
	fclose( f );

	while ( *src != 0 )
	{
		tok = get_token();	// src );

		if ( tok != NULL )
		{
			char	*p;

			tok -> line = line;
			tok -> pos = pos;

			for ( p = tok -> token; *p != 0; ++p )
			{
				if ( *p == '\n' )
				{
					++line;
					pos = 1;
				}
				else
					++pos;			// Tab cannot be set.
			}

			if ( token_list == NULL )
				token_list = tl1 = alloc( sizeof ( *tl1 ) );
			else
				tl1 = tl1 -> next = alloc( sizeof( *tl1 ) );
			tl1 -> item = tok;
			tl1 -> next = NULL;

			src += strlen( tok -> token );
		}
		else
			++src;
	}

	for ( tl1 = token_list; tl1 != NULL; tl1 = tl1 -> next )
	{
		tok = tl1 -> item;
		printf( "'%s' -> '%s'\n", token[ tok -> id ], tok -> token );
	}

	for ( tl1 = token_list; tl1 != NULL; tl1 = token_list )
	{
		token_list = tl1 -> next;
		tok = tl1 -> item;
		free( tok -> token );
		free( tok );
		free( tl1 );
	}
	free( glob_src );
}
#endif