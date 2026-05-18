/*
 *	LEXER.H
 *	-------
 *
 *	Lexical (tokens) analyzer and parser for C - header.
 *	Reference - the ISO C 9899:1999 (C99) Standard.
 *
 *	_Bool, _Complex, _Imaginary are not supported.
 *	Universal character names are not supported.
 *
 *	Supported lexical tokens: keyword, identifiers, constants, string literals, punctuators (operators)
 *	and comments.
 *
 *	Header names, macros conditional inclusion, character constant substitutions and multi-line strings concatenation 
 *	(preprocessing tokens) are left for preprocessing stage.
 *
 *	White spaces are meanwhile present as tokens, but will be later discarded (meanwhile skipped).
 */

enum	{ ID, NUM, NUM_OCT, NUM_HEX, REAL, COMMA, LPAREN, RPAREN, LFBRACE,
	RFBRACE, LSUBSCR, RSUBSCR, COLON, EOSTM, WSPACE, BNOT, LNOT, PPROC, DOLLAR, MOD,
	XOR, AND, STAR, MINUS, PLUS, EQ, OR, BACKSLASH, DIV, QUEST, LESS, MORE,
	IF, WHILE, FOR, DO, SWITCH, STRUCT, UNION, ENUM, TYPEDEF, INT, LONG, UNSIGNED,
	FLOAT, DOUBLE, CHAR, ASSIGN, CHCONST, STRCONST, CASE, BREAK, CONTINUE, DEFAULT, GOTO, VOID, SIZEOF, PLUSPLUS,
	MINUSMINUS, DEREF, MEMBERACC, AUTO, CONST, ELSE, EXTERN, INLINE, REGISTER, RESTRICT, RETURN, SHORT, SIGNED,
	STATIC, VOLATILE, SHL, SHR, NEQ, MOREEQ, LESSEQ, MODASSIGN, MULASSIGN, DIVASSIGN, PLUSASSIGN, MINUSASSIGN, 
	SHLASSIGN, SHRASSIGN, ANDASSIGN, ORASSIGN, XORASSIGN, LAND, LOR, ELLIPSIS, DEFINED, ASM, ENDASM };

enum	{ BAD_TOKEN, BAD_COMMENT, BAD_STRING };

typedef	struct	token
{
	int	id;
	char	*token;
	int	line;
	int	pos;
}	token_t;


typedef	struct	token_list_t
{
	token_t *item;
	struct	token_list_t *next;
}	token_list_t;

typedef	token_list_t	list_t;

#define is_wspace( ch ) ( ( ch == ' ' ) || ( ch == '\t' ) || ( ch == '\r' ) || ( ch == '\n' ) )
#define isalunder( ch ) ( ( ch >= 'a' && ch <= 'z' ) || ( ch >= 'A' && ch <= 'Z' ) || ( ch == '_' ) )
#define isnum( ch ) ( ch >= '0' && ch <= '9' ) 
#define ishex( ch ) ( isnum( ch ) || ( ch >= 'A' && ch <= 'F' ) || ( ch >= 'a' && ch <= 'f' ) )
#define isoct( ch ) ( ch >= '0' && ch <= '7' ) 
#define	isfloat( ch )( isnum( ch ) || ch == '.' )
#define is_alnum( ch ) ( ( isalunder( ch ) ) || ( isnum( ch ) ) )
#define	isreal( ch ) ( isnum( ch ) || ( ch == '+' ) || ( ch == '-' ) || ( ch == 'e' ) || ( ch == 'E' ) || ( ch == '.' ) )


#define	FL_DEFINED_KEYWD	1

void	*alloc( size_t size );
token_t	*get_token( int flags );

