/*
 *	PARSER.C
 *	--------
 *
 *	Syntactical analyzer and parser for C.
 *	Reference - the ISO C 9899:1999 (C99) Standard.
 *
 *	Parses the input source stream according to the grammar detailed in the ISO C Standard.
 *
 *	Parsing is attempted with a recursive-descend.
 *
 *	The parser produces output in a GOTO-subset of the C language:
 *
 *		(o) only binary or ternary operations are allowed (1 dest, 1 or 2 sources)
 *		(o) only one operation per statement, no complex statements
 *		(o) goto (unconditional) is allowed
 *		(o) only if(id) / goto (conditional) constructs are allowed, no loops. if(id)
 *	compares id with 0, no other conditional gotos are allowed.
 *
 *	The C program (a tokenized source file) is a sequence of declarations:
 *
 *		Program: Declaration*
 *
 *		Declaration:	Pure-declaration | Definition
 *
 *		Pure-declaration:	Object-pure-declaration | Function-pure-declaration
 *
 *		Definition: Object-definition | Function-definition | Enumeration-definition
 *
 *		Function-definition: Local-Declaration* Statement*
 *
 *	Declarations are defined in chapter 6.5 of the Standard.
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"

#define	operator	oper

int	global_flags = 0;

extern	char	*glob_src, *src;
extern	char	*token[];
extern	unsigned	curr_addr;
extern	int	line, pos;
extern	char	*int_reg_name[];
extern	char	*short_reg_name[];
extern	char	*byte_reg_name[];

extern	list_t	*curr_file;

char	**_psrc = &src;

int	brace_level, fbrace_level, subscr_level;
list_t	*token_list = NULL, *tl1 = NULL, *tl2 = NULL;
token_list_t	*token_list_ptr;
token_t	*tok;
int	temp_level = 0, label_count = 0, var_count = 0;
int	max_temp_level_fp = 0, max_temp_level_int = 0;
int	temp_level_fp = 0, temp_level_int = 0;
int	lit_count;
char	line_buf[ 1024 ];
char	outf[ 256 ] = "a.out";
char	out_asmf[ 256 ] = "a.out.asm";
FILE	*out;
int		parsing_list = 0;		// Parsing comma-separated list, comma counts as end-of-expression
parse_tree_t	*root = NULL, *current = NULL;

unsigned	def_types = DFLT_CHAR_UNSIGNED;
char	**postfix_code = NULL;
int		postfix_code_count = 0;
int		qlabel_count = 0;			// Labels for "question" selection operator.

extern	sym_hash_t	*sym_hash[ SYM_HASH_SIZE ], *type_hash[ TYPE_HASH_SIZE ], *func_hash[ FUNC_HASH_SIZE ], *lit_hash[ LIT_HASH_SIZE ];

int		err_count = 0, warn_count = 0;
int		glob_error = 0;				// Error symbol - used for recovery from nested recursions.

//static int	line = 1, pos = 1;
char	*parse_error[] = { "Misplaced '}'", "Misplaced ']'", "Misplaced ')'",
	"Missing '}'", "Missing ']'", "Missing ')'", "Binary operand expected",
	"Identifier expected", "Structure/union member expected", "Missing '('",
	"Missing label in 'goto' statement", "Missing ';'", 
	"Missing 'while' in 'do/while statement", "Operator expected",
	"More than one storage class specified", "Incompatible type specifiers",
	"Missing comma", "Bad initializer", "Syntax error", "Initializer subscript too high",
	"Initializer of an aggregate type must be enclosed in '{}'",
	"Not a structure/union member", "Initializer is not a constant",
	"Exceeded maximum nesting level of declarations: 32", "Attempt to declare void object",
	"Initializer is not a list", "Missing initializer", 
	"Function cannot be a struct/union member", "Unexpected end-of-file",
	"'default' statement was already used in this switch",
	"'case' with this value was already used in this switch",
	"Missing 'while' condition in 'do' statement", "Misplaced 'break'",
	"Function must return a value", "'void' function returns a value",
	"Incompatible types for 'return' statement", "Missing '{'", "Missing label in 'goto'", "Undefined symbol",
	"Missing ':'", "Left of -> or . must be a structure/union", "Left of . points to struct/union, use ->", 
	"Left of -> is a struct/union object, use .", "Incompatible types" };


//	Reports an error.
void	report_error( int err_num, token_t *tok )
{
	fprintf( stderr, "%s: Syntax error on line %d (%d): token '%s', %s\n", curr_file->item->token, line, pos, 
		( tok != NULL && tok->token != NULL ) ? tok->token : "", parse_error[ err_num ] );

	++err_count;
}


void	report_error_tok( int err_num, token_t *tok )
{
	if ( tok != NULL )
	{
		line = tok->line;
		pos = tok->pos;
	}
	report_error( err_num, tok );
}


static	int	is_assign( int id )
{
	switch ( id )
	{
	case ASSIGN: case MODASSIGN: case MULASSIGN: case DIVASSIGN: 
	case PLUSASSIGN: case MINUSASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN: 
	case ORASSIGN: case XORASSIGN:
		return	1;
	default:
		return	0;
	}
}


static	int	is_binop( int id )
{
	switch ( id )
	{
	case COMMA: case MOD: case XOR: case AND: case STAR: case MINUS: case PLUS: case EQ:
	case OR: case DIV: case LESS: case MORE: case DEREF: case MEMBERACC: case SHL: case SHR:
	case NEQ: case MOREEQ: case LESSEQ: case ASSIGN: case MODASSIGN: case MULASSIGN: case DIVASSIGN: 
	case PLUSASSIGN: case MINUSASSIGN: case SHLASSIGN: case SHRASSIGN: case ANDASSIGN: 
	case ORASSIGN: case XORASSIGN: case LAND: case LOR: case QUEST:
		return	1;
	default:
		return	0;
	}
}


static	int is_unop( int id )
{
	switch ( id )
	{
	case BNOT: case LNOT: case STAR: case MINUS: case PLUSPLUS: case MINUSMINUS: case AND:
	case SIZEOF: case DEFINED:
		return	1;
	default:
		return	0;
	}
}


static	int	is_postfix( int id )
{
	switch ( id )
	{
	case PLUSPLUS: case MINUSMINUS: case DEREF: case MEMBERACC: case LSUBSCR: case LPAREN:
		return	1;
	default:
		return	0;
	}
}


/*
 *	Compares precedence of id1 and id2.
 *
 *	Returns:
 *		-1 if id1 has higher precedence than id2
 *		0 if they have the same precedence
 *		1 if id1 has lower precedence than id2
 *
 *		-100 if id1 operator isn't found in the precedence table
 *		-101 if id2 operator isn't found in the precedence table (id1 is).
 */
static	int	cmp_preced( int id1, int id2 )
{
	static	struct	
	{
		int	id;
		int	preced;
	}	preced_table[] = 
	{ 
		{ STAR, 1 }, { DIV, 1 }, { MOD, 1 },
		{ PLUS, 2 }, { MINUS, 2 },
		{ SHL, 3 }, { SHR, 3 },
		{ LESS, 4 }, { MORE, 4 }, { LESSEQ, 4 }, { MOREEQ, 4 },
		{ EQ, 5 }, { NEQ, 5 },
		{ AND, 6 },
		{ XOR, 7 },
		{ OR, 8 },
		{ LAND, 9 },
		{ LOR, 10 }, { QUEST, 10 },
		{ ASSIGN, 11 }, { MODASSIGN, 11 }, { MULASSIGN, 11 }, { DIVASSIGN, 11 }, { PLUSASSIGN }, { MINUSASSIGN },
			{ SHLASSIGN, 11 }, { SHRASSIGN, 11 }, { ANDASSIGN, 11 }, { ORASSIGN, 11 }, { XORASSIGN, 11 },
		{ COMMA, 12 }
	};

	int	i, j;

	// Assignment operators are special case - they associate right-to-left.
	if ( is_assign( id1 ) && is_assign( id2 ) )
		return	1;

	for ( i = 0; i < sizeof( preced_table ) / sizeof( preced_table[ 0 ] ); ++i )
		if ( preced_table[ i ].id == id1 )
			break;

	if ( i == sizeof( preced_table ) / sizeof( preced_table[ 0 ] ) )
		return	-100;

	for ( j = 0; j < sizeof( preced_table ) / sizeof( preced_table[ 0 ] ); ++j )
		if ( preced_table[ j ].id == id2 )
			break;

	if ( j == sizeof( preced_table ) / sizeof( preced_table[ 0 ] ) )
		return	-101;

	if ( preced_table[ i ].preced < preced_table[ j ].preced )
		return	-1;
	else if ( preced_table[ i ].preced == preced_table [ j ].preced )
		return	0;
	else
		return	1;
}


list_t	*skip_wsp( list_t *t )
{
	char	buf[ 1024 ];
	token_t	*tok;
	list_t	*this;

	if ( t == NULL || t->item == NULL )
		return	NULL;

	while ( t != NULL && t->item->id == WSPACE )
	{
		tok = t->item;
		if ( strncmp( tok->token, "// Including", strlen( "// Including" ) ) == 0 )
		{
			sscanf( tok->token, "// Including %s", buf );
			this = alloc( sizeof( *this ) ); 
			this->item = alloc( sizeof( *this->item ) );
			this->item->token = alloc( strlen( buf ) + 1 );
			strcpy( this->item->token, buf );
			*strstr( this->item->token, ".temp" ) = '\0';
			this->item->line = 1;
			this->item->pos = 1;
			this->next = curr_file;
			curr_file = this;
		}
		else if ( strncmp( tok->token, "// End of", strlen( "// End of" ) ) == 0 )
		{
			free( curr_file->item->token );
			free( curr_file->item );
			this = curr_file->next;
			free( curr_file );
			curr_file = this;
		}

		t = t->next;
	}

	return	t;
}


static	operand_t	make_operand( char *token, type_t *type )
{
	operand_t	op;

	op.token = token;
	op.type = type;

	return	op;
}


/*
 *	Commits operations delayed until sequence point (postfix unary...) and free
 *	the delayed list.
 */
void	commit_seqpoint( FILE *f )
{
	int	i;

	for ( i = 0; i < postfix_code_count; ++i )
	{
		fprintf( f, postfix_code[ i ] );
		free( postfix_code[ i ] );
	}

	free( postfix_code );
	postfix_code = NULL;
	postfix_code_count = 0;
}


void	init_parse_tree_node( parse_tree_t *node )
{
	node->token = NULL;
	node->id = -1;
	node->parent = NULL;
	node->right = NULL;
	node->left = NULL;
	node->operator = NULL;
	node->type = NULL;
	node->seq_point = 0;
}


/*
 *	Parses type specification for the cast operator.
 */
static	__inline	list_t	*get_type( list_t *t, record_t *ret_type )
{
	record_t	rt;

}


/*
 *	Parses an expression according to specification of chapter 6.5 of the ISO Standard.
 *
 *	The function is entered with fully tokenized source and t pointing to starting token;
 *	returns pointer to the closing token.
 *
 *	The algorithm used in recursive-descend; there is a DFA for each expression clause and
 *	a recursive call for each next expression of form 'op1 binop op2'.
 *
 *	Primary expressions, postfix expressions and unary operands are parsed when determining 
 *	an operand of binary exception.
 *
 *	The function returns when one of the end-of-expression tokens are encountered:
 *		';', ')', ']', '}'
 *
 *	The function is capable of detecting misplaced end-of-expression delimiters and
 *	unbalanced parentheses.
 *
 *	The function returns the effective type of expression result
 *
 *	Stage 1: grammatic parsing, no semantic analyzis.
 *
 *	Stage 2: add semantic analyzis - type checking, promotion rules etc.
 *
 *	ops_ready:	0 = neither operands nor operators are ready for the expression
 *				1 = 1st operand is ready
 *				2 = binary operator is ready
 *				4 = second operand is ready
 */
static	list_t	*get_bin_operand( parse_tree_t *curr, list_t *t, record_t *ret_type, int flags )
{
	int	id;
	record_t	rt, *rec;
	parse_tree_t	*this, *opr, *opnd;
	token_t	*tok;
	char	line_buf1[ 1024 ];
	int		save_parsing_list;
	function_type_t	*f;
	record_list_t	*rl;
	int	rv;
	int	prm_count = 1;
	int	args_size, arg_count;
	int	n;
	char	*params[ MAX_FUNC_PARAMS ];
	macro_t	*m;
	int	save_temp_level_int, save_temp_level_fp;


	tok = ( token_t* )t->item;
	id = tok->id;
	init_record( ret_type );

	if ( is_unop( id ) )
	{
	// Unary operators (6.3.3) associate right-to-left and are parsed recursively.
		parse_tree_t *opr, *opnd;

		opr = alloc( sizeof( *opr ) );
		init_parse_tree_node( opr );
		opr->token = alloc( strlen ( tok->token ) + 1 );
		strcpy( opr->token, tok->token );
		opr->id = id;
		curr->operator = opr;

		// Except for cast expression parsing. there's nothing to do individually.
		switch( id )
		{
		default:
			break;

		case AND:
			id = id;
			break;

		case LPAREN:
			// Cast.

			line_buf1[ 0 ] = '\0';

			t = skip_wsp( t->next );
			while ( tok->id != RPAREN )
			{
				if ( NULL == t )
				{
					report_error( MISSING_RPAREN, tok );
					glob_error = 1;
					return	t;
				}

				tok = ( token_t* )t->item;
				strcat( line_buf1, tok->token );

				if ( skip_wsp( t->next ) == NULL && ( ( token_t* )skip_wsp( t->next )->item )->id == RPAREN )
					break;
			}

			free( opr->token );
			opr->token = alloc( strlen( line_buf1 ) + 1 );
			strcpy( opr->token, line_buf1 );
			opr->id = CAST_EXPR;

			//
			//	if parsing integer constant and casting to an integer, allow arithmetic 
			//	constants
			//
			if ( flags == PARSE_INTCONST )
			{
				// Check if cast to an integer.
				flags = PARSE_ARITHCONST;
			}

			break;

		case DEFINED:
			opnd = alloc( sizeof( *opnd ) );
			init_parse_tree_node( opnd );
			t = get_bin_operand( opnd, skip_wsp( t -> next ), &rt, flags );

			if ( glob_error )
				return	t;

			curr->type = alloc( sizeof( *curr->type ) );
			init_record( curr->type );
			curr->type->type.type_id = TID_BASIC;
			curr->type->type.type_ref.id = T_INT;
			curr->type->value_valid = 1;
			curr->type->is_var = 0;

			m = lookup_macro( opnd->token );

			if ( m != NULL )
				*( int* )( curr->type->value ) = 1;
			else
				*( int* )( curr->type->value ) = 0;
			free( opnd );

			return	t;
		}

		opnd = alloc( sizeof( *opnd ) );
		init_parse_tree_node( opnd );
		curr->right = opnd;
		opnd->parent = curr;
		t = get_bin_operand( curr->right, skip_wsp( t -> next ), &rt, flags );

		if ( glob_error )
			return	t;

		if ( curr->oper->id == AND )
		{
			rt.type.type_id = TID_BASIC;
			rt.type.type_ref.id = T_UINT;
		}
		*ret_type = rt;

		return	t;
	} // if ( unary operator )

	if ( id == LPAREN )
	{
	// Primary expression (6.3.1.1) - () expression
		++brace_level;
		save_parsing_list = parsing_list;
		parsing_list = 0;
		t = parse_expression( curr, NULL, skip_wsp( t -> next ), &rt, DFA_SEL_LEFT, flags );
		parsing_list = save_parsing_list;

		if ( glob_error )
			return	t;

		tok = ( token_t* )t->item;
		id = tok->id;

		if ( id != RPAREN )
		{
			report_error_tok( MISSING_RPAREN, tok );
			glob_error = 1;
			return	t;
		}
		else
		{
			t = skip_wsp( t -> next );

			if ( NULL == t )
				return	t;

			tok = ( token_t* )t->item;
			id = tok->id;
		}
	}
	else if ( id == STRCONST )
	{
		record_t lit, *l;
		ptr_list_t	*pl;

		init_record( &lit );
		lit.name = alloc( strlen( t->item->token ) + 1 );
		strcpy( lit.name, t->item->token );

		if ( ( l = lookup_symbol( t->item->token, LIT_HASH_SIZE, lit_hash ) ) == NULL )
		{
			lit.addr = curr_addr;
			*( unsigned* )lit.value = lit_count;
			add_symbol( &lit, LIT_HASH_SIZE, lit_hash );
			l = lookup_symbol( t->item->token, LIT_HASH_SIZE, lit_hash );
			curr_addr += strlen( t->item->token ) + 1;
			++lit_count;
		}

		*ret_type = lit;

		ret_type->value_valid = 1;
		ret_type->type.type_id = TID_STRCONST;
		ret_type->type.type_ref.id = T_CHAR;
		ret_type->is_var = 0;
		pl = alloc( sizeof( *pl ) );
		pl->qual = 0;
		pl->grouping = 0;
		pl->next = NULL;
		ret_type->type.ptr_list = pl;
		*( unsigned* )ret_type->value = l->addr;
		*( unsigned* )ret_type->value = lit_count;
		curr->type = alloc( sizeof( *curr->type ) );
		*curr->type = *ret_type;

		sprintf( line_buf1, "__lit%d", *( unsigned* )l->value );
		curr->token = alloc( strlen( line_buf1 ) + 1 );
		strcpy( curr->token, line_buf1 );

		return	skip_wsp( t->next );
	}
	else if ( id == NUM || id == NUM_OCT || id == NUM_HEX || id == REAL || id == CHCONST )
	{
		char	format[ 10 ] = "%s";
		int	l;

		curr->token = alloc( strlen( tok->token ) + 1 );
		strcpy( curr->token, tok->token );
		curr->id = id;

		l = strlen( tok->token );
		ret_type->type.type_id = TID_NUMCONST;
		ret_type->value_valid = 1;
		if ( id == NUM )
		{
			if ( ( tok->token[ l - 2 ] == 'u' || tok->token[ l - 2 ] ) == 'U' &&
				( tok->token[ l - 1 ] =='l' || tok->token[ l - 1 ] == 'L' ) )
			{
				sprintf( format, "%%lu" );
				ret_type->type.type_ref.id = T_ULONG;
			}
			else if ( tok->token[ l - 1 ] == 'u' || tok->token[ l - 1 ] == 'U' )
			{
				sprintf( format, "%%u" );
				ret_type->type.type_ref.id = T_UINT;
			}
			else if ( tok->token[ l - 1 ] == 'l' || tok->token[ l - 1 ] == 'L' )
			{
				sprintf( format, "%%l" );
				ret_type->type.type_ref.id = T_LONG;
			}
			else
			{
				sprintf( format, "%%d" );
				ret_type->type.type_ref.id = T_INT;
			}
		}
		else if ( id == NUM_OCT )
		{
			if ( ( tok->token[ l - 2 ] == 'u' || tok->token[ l - 2 ] ) == 'U' &&
				( tok->token[ l - 1 ] =='l' || tok->token[ l - 1 ] == 'L' ) )
			{
				ret_type->type.type_ref.id = T_UINT;
				sprintf( format, "%%lo" );
			}
			else if ( tok->token[ l - 1 ] == 'l' || tok->token[ l - 1 ] == 'L' )
			{
				ret_type->type.type_ref.id = T_ULONG;
				sprintf( format, "%%lo" );
			}
			else
			{
				ret_type->type.type_ref.id = T_UINT;
				sprintf( format, "%%o" );
			}
		}
		else if ( id == NUM_HEX )
		{
			if ( ( tok->token[ l - 2 ] == 'u' || tok->token[ l - 2 ] ) == 'U' &&
				( tok->token[ l - 1 ] =='l' || tok->token[ l - 1 ] == 'L' ) )
			{
				ret_type->type.type_ref.id = T_UINT;
				sprintf( format, "%%lX" );
			}
			else if ( tok->token[ l - 1 ] == 'l' || tok->token[ l - 1 ] == 'L' )
			{
				ret_type->type.type_ref.id = T_ULONG;
				sprintf( format, "%%lX" );
			}
			else
			{
				ret_type->type.type_ref.id = T_UINT;
				sprintf( format, "%%X" );
			}
		}
		else if ( id == REAL )
		{
			if ( strchr( tok->token, 'e' ) )
			{
				ret_type->type.type_ref.id = T_DOUBLE;
				sprintf( format, "%%e" );
			}
			else if ( strchr( tok->token, 'E' ) )
			{
				ret_type->type.type_ref.id = T_DOUBLE;
				sprintf( format, "%%E" );
			}
			else
			{
				ret_type->type.type_ref.id = T_DOUBLE;
				sprintf( format, "%%f" );
			}
		}
		else	// CHCONST
		{
			*( unsigned* ) ret_type->value = 0;
			ret_type->type.type_ref.id = T_INT;
			sprintf( format, "'%%c'" );
		}

		//ret_type->value = alloc( do_sizeof( ret_type ) );
		sscanf( tok->token, format, ( void* )ret_type->value );

		// MSDEV library has a weird error: sscanf for floating point converts into a 4-byte float instead of 8-byte double.
		if ( format[ 1 ] == 'f' || format[ 1 ] == 'e' || format[ 1 ] == 'E' )
		{
			*( double* )ret_type->value = *( float* )ret_type->value;
		}

		curr->type = alloc( sizeof( *curr->type ) );
		*curr->type = *ret_type;

		return	skip_wsp( t->next );
	}

	else if ( id == ID )
	{
	// Primary expression (6.3.1.1) - identifier.
		curr->token = alloc( strlen( tok->token ) + 1 );
		strcpy( curr->token, tok->token );
		curr->id = ID;

		//	Preprocessor is interested only in the symbol itself, symbol tables are all empty yet.
		if ( flags & PARSE_PPROC_CONDITION )
			goto	cont_id;

		curr->type = lookup_symbol( tok->token, SYM_HASH_SIZE, sym_hash );

		if ( curr->type == NULL )
		{
			// May be it's a function
			curr->type = lookup_symbol( tok->token, FUNC_HASH_SIZE, func_hash );

			if ( curr->type == NULL )
			{
				// May be it's implicit function declaration.
				if ( skip_wsp( t->next ) != NULL && skip_wsp( t->next )->item->id == LPAREN )
				{
					//
					// It's a function extern int f(...), implicit.
					// May be produce warning.
					//

					init_record( &rt );
					rt.name = alloc( strlen( tok->token ) + 1 );
					strcpy( rt.name, tok->token );
					rt.type.type_id = TID_FUNCTION;
					rt.type.ptr_list = NULL;
					rt.type.type_ref.type = f = alloc( sizeof( *f ) );
					init_record( &f->rv_type );

					f->rv_type.type.type_id = TID_BASIC;
					f->rv_type.type.ptr_list = NULL;
					f->rv_type.type.type_ref.id = T_INT;

					f->param_list = NULL;
					f->var_prm = 1;
					f->defined = 0;
					add_symbol( &rt, FUNC_HASH_SIZE, func_hash );
					curr->type = lookup_symbol( tok->token, FUNC_HASH_SIZE, func_hash );
				}
				else
				{
					// May be it's enum
					curr->type = lookup_symbol( tok->token, TYPE_HASH_SIZE, type_hash );

					if ( curr->type == NULL || curr->type->type.type_id != TID_ENUMERATOR )
					{
						// Error: undefined symbol
						report_error_tok( UNDEF_SYM, t->item );
						glob_error = 1;
						return	t;
					}
				}
			}
			curr->token = realloc( curr->token, strlen( curr->type->name ) + 1 );
			strcpy( curr->token, curr->type->name );
		}
		else if ( strncmp( curr->type->name + strlen( tok->token ), "+EBP", sizeof( "+EBP" ) - 1 ) == 0 )
		{
			curr->token = realloc( curr->token, strlen( curr->type->name ) + 1 );
			strcpy( curr->token, curr->type->name + strlen( tok->token ) + 1 );
		}
		else
		{
			if ( flags == PARSE_CONSTANT || flags == PARSE_INTCONST || flags == PARSE_ADDRCONST || 
					flags == PARSE_ARITHCONST || flags == PARSE_INIT )
			{
				// Error: not a constant
				report_error_tok( INIT_NOT_CONSTANT, t->item );
				glob_error = 1;
				return	t;
			}
			curr->token = realloc( curr->token, strlen( curr->type->name ) + 1 );
			strcpy( curr->token, curr->type->name );
		}

cont_id:
		if ( curr->type )
			*ret_type = *curr->type;
		t = skip_wsp( t -> next );

		if ( NULL == t )
			return	t;

		tok = ( token_t* )t->item;
		id = tok->id;
	}
	//	Postfix operators associate left-to-right and are parsed iteratively

	while ( is_postfix( id ) )
	{
		//++temp_level;

		// Get postfix operator
		opr = alloc( sizeof( *opr ) );
		init_parse_tree_node( opr );
		opr->token = alloc( strlen ( tok->token ) + 1 );
		strcpy( opr->token, tok->token );
		opr->id = id;

		// Get new 'this' node: __temp##temp_level = curr postfix_operator optional_operand
		this = alloc( sizeof( *this ) );
		memmove( this, curr, sizeof( *this ) );
		this->parent = curr;

		this->type = alloc( sizeof( *this->type ) );
		memmove( this->type, curr->type, sizeof( *this->type ) );

		curr->left = this;
		curr->operator = opr;
		opr->parent = curr;

		switch( id )
		{
		default:
			break;

		case DEREF:
		case MEMBERACC:
			if ( ret_type->type.type_id != TID_STRUCT && ret_type->type.type_id != TID_UNION )
			{
				// Error: not struct/union for '->' or '.'
				report_error_tok( DEREF_NOT_STRUCT, tok );
				glob_error = 1;
				return	t;
			}
			if ( ret_type->type.ptr_list != NULL && id == MEMBERACC )
			{
				// Error: operand is not struct / union.
				report_error_tok( PTR_TO_STRUCT, tok );
				glob_error = 1;
				return	t;
			}
			if ( id == DEREF && ( ret_type->type.ptr_list == NULL || ret_type->type.ptr_list->next != NULL ) )
			{
				// Error: operand is not a pointer to struct / union.
				report_error_tok( STRUCT_OBJECT, tok );
				glob_error = 1;
				return	t;
			}

			t = skip_wsp( t->next );
			if ( NULL == t || ( ( token_t* )t->item)->id != ID )
			{
				// Error: member ID expected
				report_error( MISSING_ID, tok );
				glob_error = 1;
				return	t;
			}
			tok = ( token_t* )t->item;
			opnd = alloc( sizeof( *opnd ) );
			init_parse_tree_node( opnd );
			opnd->token = alloc( strlen( tok->token ) + 1 );
			strcpy( opnd->token, tok->token );
			opnd->id = ID;
			curr->right = opnd;
			opnd->parent = curr;

			opnd->type = lookup_symbol( tok->token, SYM_HASH_SIZE, ( ( struct_type_t* )ret_type->type.type_ref.type )->sym_hash );

			curr->token = NULL;
			curr->id = -1;
			curr->type = opnd->type;
			//curr->type->name = NULL;
			break;

		case LSUBSCR:
			++subscr_level;
			//++temp_level;
			opnd = alloc( sizeof( *opnd ) );
			init_parse_tree_node( opnd );
			curr->right = opnd;
			opnd->parent = curr;
			t = parse_expression( opnd, NULL, skip_wsp( t -> next ), &rt, DFA_SEL_LEFT, flags );

			if ( glob_error )
				return	t;

			if ( NULL == t || ( ( token_t* )t->item)->id != RSUBSCR  )
			{
				report_error( MISSING_RSUBSCR, tok );
				--subscr_level;
				glob_error = 1;
				return	NULL;
			}
			//t = skip_wsp( t -> next );
			curr->token = NULL;
			curr->id = -1;
			curr->type = alloc( sizeof( *curr->type ) );
			init_record( curr->type );
			*curr->type = *curr->left->type;
			curr->type->name = NULL;

			//	Array
			if ( curr->type->dim_num != 0 )
			{
				--curr->type->dim_num;
				memmove( curr->type->dim, curr->type->dim + 1, curr->type->dim_num );
			}
			//	Pointer
			else
			{
				ptr_list_t	*ptr;

				ptr = curr->type->type.ptr_list;
				curr->type->type.ptr_list = curr->type->type.ptr_list->next;
				free( ptr );
			}

			break;

		case LPAREN:
			//
			//	Parse function call.
			//

			++brace_level;

			// curr->type must be TID_FUNCTION with pointer level no more than 1.
			if ( curr->type->type.type_id != TID_FUNCTION || 
				curr->type->type.ptr_list != NULL && curr->type->type.ptr_list->next != NULL )
			{
				// Error: designator is not a function or function pointer.
			}

			f = curr->type->type.type_ref.type;

			// Parse function's mandatory parameters.
			args_size = 0;
			arg_count = 0;

			save_temp_level_int = temp_level_int;
			save_temp_level_fp = temp_level_fp;
			for ( rl = f->param_list; t->item->id != RPAREN; rl = ( rl == NULL ? rl : rl->next ) )
			{
				parse_tree_t	*arg_root;

				memmove( params + 1, params, sizeof( char* ) * arg_count );
				arg_root = alloc( sizeof( *arg_root ) );
				init_parse_tree_node( arg_root );

				t = parse_expression( arg_root, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_ARGUMENT );

				if ( glob_error )
					return	t;

				if ( ( rl == 0 || rl->next == NULL ) && 0 == f->var_prm && t->item->id != RPAREN )
				{
					// Error: ')' expected.
					--brace_level;
					report_error_tok( MISSING_RPAREN, t->item );
					glob_error = 1;
					return	t;
				}

				if ( t->item->id == RPAREN )
				{
					if ( rl != NULL && rl->next != NULL )
					{
						// Error: too many parameters
					}
					else
					{
						--brace_level;
					}
				}
				else if ( t->item->id != COMMA )
				{
					--brace_level;
					report_error_tok( SYNTAX_ERROR, t->item );
					glob_error = 1;
					return	t;
					// Error: ',' or ')' expected.
				}

				assign_temp_types( arg_root );
				assign_temp_names( arg_root );
				calc_parse_tree( arg_root );

				if ( rl != NULL && !( rl->next == NULL && f->var_prm ) && ( rv = compat_types( arg_root->type, &rl->rec ) ) == TYPES_INCOMPAT )
				{
					report_error( INCOMPAT_TYPES, t->item );
					glob_error = 1;
					return	t;
					// Error: incompatible type for parameter prm_count
				}
				else if ( rv == TYPES_REQ_CAST )
				{
					// Warning: explicit cast is required for parameter prm_count
				}

				print_parse_tree( arg_root, out );

				//
				// Parameters are "pushed" in left-to-right order in intermediate code.
				// Intermediate-to-assembly translator will reverse them.
				// Parameters are dword-aligned.
				//
				n = do_sizeof( arg_root->type );
				if ( ( n & 3 ) != 0 )
					n = ( n & ~3 ) + 4;

				if ( rt.is_var == 0 && rt.type.ptr_list != NULL )
					sprintf( line_buf1, "__push_arg __offset %s %d\n", arg_root->token, n );
				else
					sprintf( line_buf1, "__push_arg %s %d\n", arg_root->token, n );

				params[ 0 ] = alloc( strlen( line_buf1 ) + 1 );
				strcpy( params[ 0 ], line_buf1 );

				destroy_parse_tree( arg_root );
				args_size += n;
				++arg_count;
				++temp_level_int;
				++temp_level_fp;
				++max_temp_level_int;
				++max_temp_level_fp;
			}

			// Sequence point (6.5.2.2)
			commit_seqpoint( out );

			temp_level_int = save_temp_level_int;
			temp_level_fp = save_temp_level_fp;

			//
			//	Aggregate types are returned on stack.
			//	They immediately follow the pushed arguments (pushed before).
			//
			if ( !is_scalar( &f->rv_type ) )
			{
				int	sz;

				fprintf( out, "__asm\n" );
				fprintf( out, "\tSUB\tESP, 0%08XH\n", ( ( sz = do_sizeof( &f->rv_type ) ) & 3 == 0 ) ? sz : ( sz & ~3 ) + 4 );
				fprintf( out, "__endasm\n" );
			}

			for ( n = 0; n < arg_count; ++n )
			{
				fprintf( out, params[ n ] );
				free( params[ n ] );
			}

			--brace_level;
			if ( curr->type->type.ptr_list != NULL )
				sprintf( line_buf1, "__call * %s\n", curr->token );
			else
				sprintf( line_buf1, "__call _%s\n", curr->token );
			fputs( line_buf1, out );

			curr->type = alloc( sizeof( *curr->type ) );
			*curr->type = f->rv_type;

			if ( f->rv_type.type.ptr_list != NULL || f->rv_type.type.type_id == TID_BASIC &&
				f->rv_type.dim_num == 0 && f->rv_type.type.type_ref.id < T_FLOAT )
			{
				sprintf( line_buf1, "__ret_val_int%d", do_sizeof( &f->rv_type ) );
			}
			else if ( f->rv_type.type.type_id == TID_BASIC && f->rv_type.dim_num )
			{
				sprintf( line_buf1, "__ret_val_fp%d", do_sizeof( &f->rv_type ) );
			}
			else
			{
				sprintf( line_buf1, "__ret_val$%d", do_sizeof( &f->rv_type ) );
			}
				
			if ( args_size != 0 )
			{
				fprintf( out, "__asm\n" );
				fprintf( out, "\tADD\tESP, 0%08XH\n", is_scalar( &f->rv_type ) ? args_size : args_size + do_sizeof( &f->rv_type ) );
				fprintf( out, "__endasm\n" );
			}

			curr->token = alloc( strlen( line_buf1 ) + 1 );
			strcpy( curr->token, line_buf1 );
			curr->id = ID;
			free( curr->left );
			curr->left = NULL;
			free( curr->oper );
			curr->oper = NULL;
			break;
		}

		if ( NULL == t || ( t = skip_wsp( t->next ) ) == NULL )
			break;

		tok = ( token_t* )t->item;
		id = tok->id;
	} // while (postfix expression)

	return	t;
}


static	list_t	*get_bin_operator( list_t *t, int *rv )
{
	return	skip_wsp( t -> next );
}


/*
 *	The DFA accepts an expression when one of end-of-expression delimiters is
 *	shifted in: EOSTM, RPAREN, RSUBSCR
 */
static	int	is_end_of_expr( int id, token_t *tok )
{
	switch( id )
	{
	case COMMA:
		if ( parsing_list == 0 )
			return	0;

		return	1;

	case EOSTM:
		return	1;

	case RPAREN:
		return	1;

	case RSUBSCR:
		return	1;

	case COLON:
		return	1;
	}

	return	0;
}


list_t	*skip_to_term( list_t *t )
{
	for ( ; t != NULL; t = t->next )
		if ( is_end_of_expr( t->item->id, t->item ) )
			return	t;

	// t == NULL
	fprintf( stderr, "Unexpected end-of-file\n" );
	exit( 0 );
	//return	t;
}


int	opr_id[] = { COMMA, MOD, XOR, AND, STAR, MINUS, PLUS, EQ, OR, DIV, LESS,  
		MORE, DEREF, MEMBERACC, SHL, SHR, NEQ, MOREEQ, LESSEQ, ASSIGN, MODASSIGN, 
		MULASSIGN, DIVASSIGN, PLUSASSIGN, MINUSASSIGN, SHLASSIGN, SHRASSIGN, 
		ANDASSIGN, ORASSIGN, XORASSIGN, LAND, LOR };
char	*opr_str[] = { ",", "%", "^", "&", "*", "-", "+", "==", "|", "/", "<", ">",
		"->", ".", "<<", ">>", "!=", ">=", "<=", "=", "%=", "*=", "/=", "+=", "-=",
		"<<=", ">>=", "&=", "|=", "^=", "&&", "||" };


char	*binopr2str( int id )
{
	int	i;

	for ( i = 0; i < sizeof( opr_id ) / sizeof( id ); ++i )
		if ( id == opr_id[ i ] )
			return	opr_str[ i ];

	return	NULL;
}


/*
 *	parse_bin_expression()
 *
 *	Recursively parses  *	This function shall be called with first operand already available.
 *	Source list-of-tokens pointer points to the binary operator.
 */
list_t	*parse_expression( parse_tree_t *curr, parse_tree_t *first_opnd, 
				list_t *t, record_t *ret_type, int dfa_sel, int flags )
{
	parse_tree_t	*opr, *opnd;
	int	state = DFA_START;
	int	id;
	token_t	*tok;
	record_t	rt;
	int	save_parsing_list;

	if ( flags == PARSE_ARGUMENT || flags == PARSE_INIT )
	{
		save_parsing_list = parsing_list;
		parsing_list = 1;
	}

	if ( dfa_sel != DFA_SEL_LEFT && dfa_sel != DFA_SEL_RIGHT )
	{
		fprintf( stderr, "Bad DFA selection\n", dfa_sel );
		return	t;
	}

	while ( state != DFA_ACCEPT )
	{
		//	EOF marker shifted in the middle of expression.
		if ( NULL == t )
		{
			if ( !( flags & PARSE_PPROC_CONDITION ) && !( flags & PARSE_COND_OPER ) )
			{
				if ( brace_level > 0 )
				{
					report_error( MISSING_RPAREN, tok );
					brace_level = 0;
				}
				if ( fbrace_level > 0 )
				{
					report_error( MISSING_RFBRACE, tok );
					fbrace_level = 0;
				}
				if ( subscr_level > 0 )
				{
					report_error( MISSING_RSUBSCR, tok );
					subscr_level = 0;
				}
				glob_error = 1;
			}
			return	t;
		}

		tok = ( token_t* )t->item;
		id = tok->id;

		switch( state )
		{
		case DFA_START:
			if ( is_end_of_expr( id, tok ) )
				return	t;

			// Get first operand.
			if ( dfa_sel == DFA_SEL_LEFT )
			{
				opnd = alloc( sizeof( *opnd ) );
				init_parse_tree_node( opnd );
				t = get_bin_operand( opnd, t, &rt, flags );

				if ( glob_error )
					return	t;

				*ret_type = rt;
			}
			else
			{
				opnd = first_opnd;
			}
			curr->left = opnd;
			opnd->parent = curr;
			state = GOT_OPND1;
			break;

		case GOT_OPND1:
			if ( is_end_of_expr( id, tok ) )
			{
				if ( opnd->token == NULL )
					curr->token = NULL;
				else
				{
					curr->token = alloc( strlen( opnd->token ) + 1 );
					strcpy( curr->token, opnd->token );
				}
				curr->id = opnd->id;
				curr->left = opnd->left;
				curr->operator = opnd->operator;
				curr->right = opnd->right;

				if ( opnd->type != NULL )
				{
					curr->type = alloc( sizeof( *curr->type ) );
					*curr->type = *opnd->type;
				}

				if ( dfa_sel == DFA_SEL_LEFT )
				{
				}
				state = DFA_ACCEPT;
				break;
			}

			// Get binary operator.
			if ( !is_binop( id ) )
			{
				// EOSTM insertion assumed, glob_error is not asseted
				report_error_tok( MISSING_EOSTM, tok );
				glob_error = 1;
				return	t;
			}

			opr = alloc( sizeof( *opr ) );
			init_parse_tree_node( opr );
			opr->token = alloc( strlen( tok->token ) + 1 );
			strcpy( opr->token, tok->token );
			opr->id = tok->id;
			curr->operator = opr;
			opr->parent = curr;
			state = GOT_BINOP;

			if ( id == QUEST )
			{
				parse_tree_t	*cond_root;

				cond_root = alloc( sizeof( *cond_root ) );
				init_parse_tree_node( cond_root );
				t = parse_expression( cond_root, NULL, t, &rt, DFA_SEL_LEFT, flags | PARSE_COND_OPER );

				if ( glob_error )
					return	t;

				if ( NULL == t || t->item->id != COLON )
				{
					// Error - ':' expected
					report_error_tok( MISSING_COLON, tok );
					glob_error = 1;
					return	t;
				}

				opr->left = cond_root;

				cond_root = alloc( sizeof( *cond_root ) );
				init_parse_tree_node( cond_root );
				t = parse_expression( cond_root, NULL, t, &rt, DFA_SEL_LEFT, flags );
				opr->right = cond_root;

				if ( glob_error )
					return	t;
			}

			t = skip_wsp( t->next );
			break;

		case GOT_BINOP:
			if ( is_end_of_expr( id, tok ) )
			{
				report_error_tok( MISSING_BINOPND, tok );
				return	skip_to_term( t );
			}

			if ( curr->operator->id == LOR || curr->operator->id == LAND )
				curr->left->seq_point = 1;

			//
			//	Assignment invalidates the value. This is OK for this
			//	stage, an execution path analyzer shall check if there's
			//	only one way from all-valid paths to this assignment.
			//
			if ( is_assign( id ) )
				curr->left->type->value_valid = 0;

			// Get second operand.
			opnd = alloc( sizeof( *opnd ) );
			init_parse_tree_node( opnd );
			t = get_bin_operand( opnd, t, &rt, flags );

			if ( glob_error )
				return	t;

			*ret_type = rt;
			curr->right = opnd;
			opnd->parent = curr;
			state = GOT_OPND2;
			break;

		case GOT_OPND2:
			if ( is_end_of_expr( id, tok ) )
			{
				state = DFA_ACCEPT;
				break;
			}
			else
			{
				if ( !is_binop( id ) )
				{
					// EOSTM insertion assumed, glob_error is not asseted
					report_error_tok( MISSING_EOSTM, tok );
					glob_error = 1;
					return	t;
				}

				if ( dfa_sel == DFA_SEL_RIGHT )
				{
					if ( cmp_preced( curr->operator->id, id ) <= 0 )
					{
						state = DFA_ACCEPT;
						break;
					}
				}
				else	// dfa_sel == DFA_SEL_LEFT
				{
					if ( cmp_preced( curr->operator->id, id ) > 0 )
					{
						// Instead of right operand insert result of the next binary expression.
						opnd = alloc( sizeof( *opnd ) );
						init_parse_tree_node( opnd );

						t = parse_expression( opnd, curr->right, t, &rt, DFA_SEL_RIGHT, flags );

						if ( glob_error )
							return	t;

						opnd->parent = curr;
						curr->right = opnd;
						break;
					}
				}
			}

			if ( dfa_sel == DFA_SEL_RIGHT )
			{
				// Instead of right operand insert result of the next binary expression.
				opnd = alloc( sizeof( *opnd ) );
				init_parse_tree_node( opnd );

				t = parse_expression( opnd, curr->right, t, &rt, DFA_SEL_RIGHT, flags );

				if ( glob_error )
					return	t;

				opnd->parent = curr;
				curr->right = opnd;
			}
			else	// dfa_sel == DFA_SEL_LEFT
			{
				opnd = alloc( sizeof( *opnd ) );
				init_parse_tree_node( opnd );

				opnd->left = alloc( sizeof( *opnd->left ) );
				memmove( opnd->left, curr, sizeof( *curr ) );

				if ( curr->left != NULL )
					curr->left->parent = opnd->left;
				if ( curr->right != NULL )
					curr->right->parent = opnd->left;

				memmove( curr, opnd, sizeof( *curr ) );
				if ( curr->left->parent != NULL)
					curr->parent = curr->left->parent;
				free( opnd );

				state = GOT_OPND1;
			}
			break;
		}

		if ( t != NULL )
		{
			tok = ( token_t* )t->item;
			id = tok->id;
		}

	} // while()

	//
	// Normal return - due to DFA accept.
	//

	//
	//	If there is a binary expression, perform type promotions.
	//
	//	Assignment operators are excepted from type promotions: result always has type of
	//	left operand.
	//
	//	Aggregation operators [] and structure/union dereference operators . and -> are excepted 
	//	from type promotions. Result always has type of right operand
	//
	//	Pointer arithmetic gets special handling
	//
	if ( curr->left != NULL && curr->right != NULL && curr->left->type != NULL &&
			curr->right->type != NULL )
	{
		if ( curr->oper->id == MEMBERACC || curr->oper->id == DEREF )
		{
			curr->type = alloc( sizeof( *curr->type ) );
			*curr->type = *curr->right->type;
		}
		else if ( curr->left->type->type.ptr_list != NULL )
		{
			if ( curr->oper->id == PLUS )
			{
				record_t	desig;
				parse_tree_t	*this;
				char	buf[ 256 ];

				if ( !is_scalar( curr->right->type ) || curr->right->type->type.type_ref.id > T_ULONG )
				{
					// Error: pointer may be added only to a scalar
				}

				desig = *curr->left->type;
				desig.type.ptr_list = desig.type.ptr_list->next;

				this = alloc( sizeof( *this ) );
				init_parse_tree_node( this );
				this->left = curr->right;
				curr->right->parent = this;
				curr->right = this;
				this->parent = curr;
				//this->type = alloc( sizeof( *this->type ) );
				//init_record( this->type );

				this->right = alloc( sizeof( *this->right ) );
				init_parse_tree_node( this->right );
				this->right->parent = this;
				this->right->type = alloc( sizeof( *this->right->type ) );
				init_record( this->right->type );
				*( unsigned* )this->right->type->value = do_sizeof( &desig );
				sprintf( buf, "%lu", *( unsigned* )this->right->type->value );
				this->right->token = alloc( strlen( buf ) + 1 );
				strcpy( this->right->token, buf );
				this->right->id = NUM;
				this->right->type->type.type_id = TID_BASIC;
				this->right->type->type.type_ref.id = T_INT;
				this->right->type->is_var = 0;
				this->right->type->value_valid = 1;

				this->oper = alloc( sizeof( *this->oper ) );
				init_parse_tree_node( this->oper );
				this->oper->token = alloc( sizeof( "*" ) );
				strcpy( this->oper->token, "*" );
				this->oper->parent = this;

			}
			else if ( curr->oper->id == MINUS )
			{
				if ( !compat_types( curr->left->type, curr->right->type ) )
				{
					// Error: incompatible types for pointer subtraction
				}
			}
			else if ( curr->oper->id != EQ && curr->oper->id != NEQ )
			{
				// Error: for pointer arithmetic only comparison for equality, addition and subtraction are allowed
			}
		}
		else if ( curr->oper->id != ASSIGN && curr->oper->id != LSUBSCR )
		{
			if ( !compat_types( curr->left->type, curr->right->type ) )
			{
				// Error: incompatible types (at least cast is required)
			}

			curr->type = alloc( sizeof( *curr->type ) );
			init_record( curr->type );
			curr->type->type.type_id = curr->left->type->type.type_id;
			if ( curr->left->type->type.type_ref.id > curr->right->type->type.type_ref.id ||
				is_assign( curr->oper->id ) )
				curr->type->type.type_ref.id = curr->left->type->type.type_ref.id;
			else
				curr->type->type.type_ref.id = curr->right->type->type.type_ref.id;

			// If at least one operand is a variable, the result is a variable.
			if ( curr->left->type->is_var || curr->right->type->is_var )
				curr->type->is_var = 1;

			if ( curr->left->type->value_valid && curr->right->type->value_valid )
				curr->type->value_valid = 1;
		}
	}

	if ( flags == PARSE_ARGUMENT || flags == PARSE_INIT )
		parsing_list = save_parsing_list;

	return	t;
}


/*
 *	Returns whether 'p' is a terminator node.
 */
int	is_leaf( parse_tree_t *p )
{
	if ( p->left == NULL && p->operator == NULL && p->right == NULL )
		return	1;
	return	0;
}


void	assign_temp_types( parse_tree_t *p )
{
	int	id1 = -1, id2 = -1;

	if ( is_leaf( p ) )
		return;

	if ( p->left )
	{
		assign_temp_types( p->left );
	}
	if ( p->right )
	{
		assign_temp_types( p->right );
	}

	if ( p->type == NULL )
	{
		p->type = alloc( sizeof( *p->type ) );
		init_record( p->type );

		if ( p->oper != NULL && ( p->oper->id == DEREF || p->oper->id == MEMBERACC ) )
		{
			*p->type = *p->right->type;
			p->id = ID;
			return;
		}

		if ( NULL == p->left && AND == p->oper->id )
		{
			p->type->type.type_id = TID_BASIC;
			p->type->type.type_ref.id = T_UINT;
		}
		else if ( p->left != NULL && p->left->type != NULL && ( id1 = p->left->type->type.type_ref.id ) == T_FLOAT || 
			id1 == T_DOUBLE || id1 == T_LONGDOUBLE )
		{
			p->type->type.type_id = TID_BASIC;
			p->type->type.type_ref.id = id1;
		}
		else if ( p->right != NULL && p->right->type != NULL && ( id2 = p->right->type->type.type_ref.id ) == T_FLOAT || 
			id2 == T_DOUBLE || id2 == T_LONGDOUBLE )
		{
			p->type->type.type_id = TID_BASIC;
			if ( id2 > id1 )						// If rank is greater
				p->type->type.type_ref.id = id2;
		}
		else
		{
			// Composite types will be handled later.
			p->type->type.type_id = TID_BASIC;
			p->type->type.type_ref.id = id1;
			if ( id2 > id1 )
				p->type->type.type_ref.id = id2;
		}

		if ( p->left != NULL && p->left->type->type.ptr_list != NULL && !( p->right != NULL && p->right->type->type.ptr_list != NULL ) )
			p->type->type.ptr_list = p->left->type->type.ptr_list;
		else if	( p->right != NULL && p->right->type->type.ptr_list != NULL && !( p->left != NULL && p->left->type->type.ptr_list != NULL ) )
			p->type->type.ptr_list = p->right->type->type.ptr_list;
		else if ( p->right != NULL && p->right->type->type.ptr_list != NULL && p->left != NULL && p->left->type->type.ptr_list != NULL )
		{
			p->type->type.type_id = TID_BASIC;
			p->type->type.type_ref.id = T_ULONG;
		}


		p->id = ID;
	}
}


void	assign_temp_names( parse_tree_t *p )
{
	char	temp_name[ 256 ];
	int	id1 = -1, id2 = -1;

	if ( is_leaf( p ) )
		return;

	if ( p->token == NULL )
	{
		if ( !is_scalar( p->type ) )
		{
			sprintf( temp_name, "__temp_%s", id1 == TID_STRUCT ? "struct" : "union" );
		}
		else if ( is_float( p->type ) )
		{
			sprintf( temp_name, "__temp_fp%d", temp_level_fp );
		}
		else
		{
			sprintf( temp_name, "__temp_int%d", temp_level_int );

			// Composite types will be handled later.
		}

		p->token = alloc( strlen( temp_name ) + 1 );
		strcpy( p->token, temp_name );
		p->id = ID;
	}

	if ( p->left )
	{
		if ( !is_leaf( p->left ) )
		{
			assign_temp_names( p->left );
		}
	}
	if ( p->right )
	{
		if ( !is_leaf( p->right ) )
		{
			id2 = p->right->type->type.type_ref.id;
			if ( p->left && p->left->type )
				id1 = p->left->type->type.type_ref.id;
			else
				id1 = id2;

			if ( ( id1 == T_FLOAT || id1 == T_DOUBLE || id1 == T_LONGDOUBLE ) &&
				( id2 == T_FLOAT || id2 == T_DOUBLE || id2 == T_LONGDOUBLE ) )
			{
				++temp_level_fp;
				if ( max_temp_level_fp < temp_level_fp )
					max_temp_level_fp = temp_level_fp;
				assign_temp_names( p->right );
				--temp_level_fp;
			}
			else if ( !( id1 == T_FLOAT || id1 == T_DOUBLE || id1 == T_LONGDOUBLE ) &&
				!( id2 == T_FLOAT || id2 == T_DOUBLE || id2 == T_LONGDOUBLE ) )
			{
				++temp_level_int;

				if ( max_temp_level_int < temp_level_int )
					max_temp_level_int = temp_level_int;
				assign_temp_names( p->right );
				--temp_level_int;
			}
			else
			{
				assign_temp_names( p->right );
			}
		}
	}
}


/*
 *	Prints intermediate code instructions needed to convert src type to dest.
 */
void	print_conversion( parse_tree_t *dest, parse_tree_t *src, FILE *f )
{
	int	sz_dest, sz_src;
	int	id, id2;

	sz_dest = do_sizeof( dest->type );
	sz_src = do_sizeof( src->type );

	if ( dest->type->type.ptr_list != NULL )
	{
		if ( sz_src == sz_dest )
		{
			fprintf( f, "%s <- %s\n", dest->token, src->token );
		}
		else
		{
			id = src->type->type.type_ref.id;
			if ( id == T_CHAR || id == T_SHORT )
				fprintf( f, "%s <- %s ( UINT 4 <= INT %d )\n", dest->token, src->token, sz_src );
			else
				fprintf( f, "%s <- %s ( UINT 4 <= UINT %d )\n", dest->token, src->token, sz_src );
		}
	}

	else if ( src->type->type.ptr_list != NULL )
	{
		if ( sz_src == sz_dest )
		{
			fprintf( f, "%s <- %s\n", dest->token, src->token );
		}
		else
		{
			id = dest->type->type.type_ref.id;
			if ( id == T_CHAR || id == T_SHORT )
				fprintf( f, "%s <- %s ( INT %d <= UINT 4 )\n", dest->token, src->token, sz_src );
			else
				fprintf( f, "%s <- %s ( UINT %d <= UINT 4 )\n", dest->token, src->token, sz_src );
		}
	}

	// Both src and dest are non-pointers.
	else if ( dest->type->type.type_ref.id >= T_FLOAT )
	{
		if ( src->type->type.type_ref.id >= T_FLOAT )
		{
			if ( sz_src == sz_dest )
				fprintf( f, "%s <- %s\n", dest->token, src->token );
			else
				fprintf( f, "%s <- %s ( FP %d <= FP %d )\n", dest->token, src->token, sz_dest, sz_src );

		}
		else
		{
			id = dest->type->type.type_ref.id;
			fprintf( f, "%s <- %s ( FP %d <= %s %d )\n", dest->token, src->token, sz_dest, 
				( id == T_CHAR || id == T_SHORT || id == T_INT || id == T_LONG ) ? "INT" : "UINT",
				sz_src );
		}
	}
	else
	{
		if ( src->type->type.type_ref.id >= T_FLOAT )
		{
			id = dest->type->type.type_ref.id;
			fprintf( f, "%s <- %s ( %s %d <= FP %d )\n", dest->token, src->token, 
				( id == T_CHAR || id == T_SHORT || id == T_INT || id == T_LONG ) ? "INT" : "UINT",
				sz_dest, sz_src );
		}
		else
		{
			if ( sz_src == sz_dest )
				fprintf( f, "%s <- %s\n", dest->token, src->token );
			else
			{
				id = src->type->type.type_ref.id;
				id2 = dest->type->type.type_ref.id;

				fprintf( f, "%s <- %s ( %s %d <= %s %d )\n", dest->token, src->token,
					( id2 == T_CHAR || id2 == T_SHORT || id2 == T_INT || id2 == T_LONG ) ? "INT" : "UINT",
					sz_dest,
					( id == T_CHAR || id == T_SHORT || id == T_INT || id == T_LONG ) ? "INT" : "UINT",
					sz_src );
			}
		}
	}
}


void	print_value( record_t *rec, FILE *f )
{
	if ( rec->type.ptr_list != NULL )
		goto	uint_32;

	if ( rec->type.type_id == TID_ENUMERATOR || rec->type.type_id == TID_ENUM )
		goto	int_32;

	if ( rec->type.type_id != TID_BASIC && rec->type.type_id != TID_NUMCONST &&
		rec->type.type_id != TID_ADDRCONST )
		return;

	switch ( rec->type.type_ref.id )
	{
	default:
		break;
	case T_CHAR:
		fprintf( f, "%d", ( int )( *( signed char* )( rec->value ) ) );
		break;
	case T_UCHAR:
		fprintf( f, "%d", ( int )( *( unsigned char* )( rec->value ) ) );
		break;
	case T_SHORT:
		fprintf( f, "%d", ( int )( *( short* )( rec->value ) ) );
		break;
	case T_USHORT:
		fprintf( f, "%d", ( int )( *( unsigned short* )( rec->value ) ) );
		break;
	case T_INT:
int_32:
		fprintf( f, "%d", ( int )( *( int* )( rec->value ) ) );
		break;
	case T_UINT:
uint_32:
		fprintf( f, "%d", *( unsigned* )( rec->value ) );
		break;
	case T_LONG:
		fprintf( f, "%ld", *( long* )( rec->value ) );
		break;
	case T_ULONG:
		fprintf( f, "%lu", *( unsigned long* )( rec->value ) );
		break;
	case T_FLOAT:
		fprintf( f, "%#f", *( float* )( rec->value ) );
		break;
	case T_DOUBLE:
		fprintf( f, "%#g", *( double* )( rec->value ) );
		break;
	case T_LONGDOUBLE:
		fprintf( f, "%#G", *( long double* )( rec->value ) );
		break;
	}
}


int	is_scalar( record_t *rec )
{
	if ( rec->dim_num != 0 )
		return	0;

	if ( rec->type.ptr_list != NULL )
		return	1;

	if ( rec->type.type_id == TID_BASIC || rec->type.type_id == TID_ENUM || rec->type.type_id == TID_ENUMERATOR || 
		rec->type.type_id == TID_NUMCONST || rec->type.type_id == TID_ADDRCONST || rec->type.type_id == TID_STRCONST )
		return	1;

	return	0;
}


int	is_float( record_t *rec )
{
	if ( !is_scalar( rec ) )
		return	0;

	if ( rec->type.ptr_list != NULL )
		return	0;

	if ( rec->type.type_ref.id >= T_FLOAT )
		return	1;

	return	0;

}


int	is_signed( record_t *rec )
{
	if ( !is_scalar( rec ) )
		return	0;

	if ( rec->type.ptr_list != NULL )
		return	0;

	if ( is_float( rec ) )
		return	0;

	if ( rec->type.type_ref.id == T_CHAR || rec->type.type_ref.id == T_SHORT ||
		rec->type.type_ref.id == T_INT || rec->type.type_ref.id == T_LONG )
		return	1;

	return	0;
}


/*
 *	Prints parse tree using DFS.
 */
void	print_parse_tree( parse_tree_t *p, FILE *f )
{
	record_t	*rec1 = NULL, *rec2 = NULL, *rec3 = NULL;
	int	compat;
	int	ii;

	if ( is_leaf( p ) )
		return;

	if ( p->oper == NULL )
		return;

	if ( p->left == NULL && p->oper->id == AND )
	{
		int	n;

		//
		// Calculate address of the right operand.
		//
		// & operand is valid only in one of the following cases:
		//	(o)	right operand is a leaf (terminator)
		//	(o)	right operand is result of * operator
		//	(o)	right operator is result of [], -> or . operator
		//

		if ( is_leaf( p->right ) )
		{
			fprintf( f, "%s <- & %s [ UINT4 ]\n", p->token, p->right->token );
		}
		else if ( p->right->oper->id == STAR && p->right->left == NULL )
		{
			print_parse_tree( p->right->right, f );
			fprintf( f, "%s <- %s [ UINT4 ]\n", p->token, p->right->right->token );
		}
		else if ( p->right->oper->id == MEMBERACC || p->right->oper->id == DEREF || p->right->oper->id == LSUBSCR )
		{
			print_parse_tree( p->right, f );
			fprintf( f, "%s <- __temp_addr [ UINT4 ]\n", p->token );
		}

		return;
	}

	else if ( p->oper->id == QUEST )
	{
		int	lab1, lab2;

		print_parse_tree( p->left, f );
		fprintf( f, "if ( !%s [ %s%d ] ) goto %__qlabel%d\n", is_float( p->type ) ? "FP" : "INT", do_sizeof( p->type ), p->left->token, qlabel_count );
		lab1 = qlabel_count;
		++qlabel_count;
		print_parse_tree( p->oper->left, f );
		fprintf( f, "%s <- %s\n", p->token, p->oper->left->token );
		lab2 = qlabel_count;
		++qlabel_count;
		fprintf( f, "__qlabel%d:\n", lab1 );
		print_parse_tree( p->oper->right, f );
		fprintf( f, "%s <- %s\n", p->token, p->oper->right->token );
		fprintf( f, "__qlabel%d:\n", lab2 );
		return;
	}

	else if ( ( p->oper->id == PLUSPLUS || p->oper->id == MINUSMINUS ) && p->left != NULL )
	{
		char	buf[ 1024 ];

		postfix_code = realloc( postfix_code, ++postfix_code_count * sizeof( *postfix_code ) );
		sprintf( buf, "++ %s [ %s%d ]\n", 
			p->left->token, 
			is_float( p->left->type ) ? "FP" : is_signed( p->left->type ) ? "INT" : "UINT",
			do_sizeof( p->left->type ) );
		postfix_code[ postfix_code_count - 1 ] = alloc( strlen( buf ) + 1 );
		strcpy( postfix_code[ postfix_code_count - 1 ], buf );
		return;
	}

	else if ( ( p->oper->id == MEMBERACC || p->oper->id == MEMBERACC ) && is_leaf( p->left ) )
	{
		// Size [ UINT1 ] doesn't matter, 'cause address is taken
		fprintf( f, "__temp_addr <- & %s [ UINT4 ]\n", p->left->token );
	}
	else if ( p->oper->id == LSUBSCR /*&& is_leaf( p->left )*/ )
	{
		int	sz;
		record_t	rec;

		rec = *p->left->type;

		//	Array name - convert to pointer.
		if ( p->left->type->dim_num != 0 )
			fprintf( f, "__temp_addr <- & %s [ UINT4 ]\n", p->left->token );
		//	Assign pointer.
		else
			fprintf( f, "__temp_addr <- %s [ UINT4 ]\n", p->left->token );
	}

	if ( p->left )
	{
		print_parse_tree( p->left, f );

		// Sequence point (6.5.13/14/15/17).
		if ( p->left->seq_point )
			commit_seqpoint( f );
	}

	if ( p->right )
		print_parse_tree( p->right, f );

	//
	//	Aggregate types.
	//
	if ( p->oper->id == MEMBERACC || p->oper->id == DEREF || p->oper->id == LSUBSCR )
	{
		int	n;

		if ( p->oper->id == MEMBERACC )
		{
			if ( p->left->type->type.type_id == TID_STRUCT )
			{
			 	fprintf( f, "__asm\n" );
				fprintf( f, "\tADD\tDWORD PTR [__temp_addr], %d\n", do_offsetof( p->left->type, p->right->type ) );
			 	fprintf( f, "__endasm\n" );
			}
		}
		else if ( p->oper->id == DEREF )
		{
			if ( p->left->type->type.type_id == TID_STRUCT )
			{

			 	fprintf( f, "__asm\n" );
				fprintf( f, "\tPUSH\tEAX\n" );

				if ( sscanf( p->left->token, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
					fprintf( f, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
				else
					fprintf( f, "\tMOV\tEAX, DWORD PTR [%s]\n", p->left->token );
				fprintf( f, "\tADD\tEAX, %u\n", do_offsetof( p->left->type, p->right->type ) );
				fprintf( f, "\tMOV\tDWORD PTR [__temp_addr], EAX\n" );
				fprintf( f, "\tPOP\tEAX\n" );
			 	fprintf( f, "__endasm\n" );
			}
		}
		else	// LSUBSCR
		{
			int	sz;
			record_t	rec;

			rec = *p->left->type;
			if ( rec.dim_num > 0 )
			{
				--rec.dim_num;
				if ( rec.dim_num > 0 )
				{
					rec.dim = realloc( rec.dim, rec.dim_num * sizeof( *rec.dim ) );
					memmove( rec.dim, p->left->type->dim + 1, rec.dim_num * sizeof( *rec.dim ) );
				}
			}
			else
			{
				rec.type.ptr_list = rec.type.ptr_list->next;
			}

			//	Subscript to an array or pointer.
			fprintf( f, "__asm\n" );
			fprintf( f, "\tPUSH\tEAX\n" );
			fprintf( f, "\tPUSH\tEDX\n" );
			if ( sscanf( p->right->token, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
			{
				fprintf( f, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
			}
			else
			{
				if ( isnum( p->right->token[ 0 ] ) )
				{
					fprintf( f, "\tMOV\tEAX, %s\n", p->right->token );
				}
				else
				{
					if ( isnum( p->right->token[ 0 ] ) )
						fprintf( f, "\tMOV\tEAX, %s\n", p->right->token );
					else						
						fprintf( f, "\tMOV\tEAX, DWORD PTR [%s]\n", p->right->token );
				}
			}
			fprintf( f, "\tMOV\tEDX, %u\n", do_sizeof( &rec ) );
			fprintf( f, "\tMUL\tEDX\n" );
			fprintf( f, "\tADD\tDWORD PTR [__temp_addr], EAX\n" );
			fprintf( f, "\tPOP\tEDX\n" );
			fprintf( f, "\tPOP\tEAX\n" );
			fprintf( f, "__endasm\n" );
		}

		fprintf( f, "%s <- __temp_addr [ UINT4 ]\n", p->token );

		p->token = realloc( p->token, strlen( p->token ) + 2 );
		memmove( p->token + 1, p->token, strlen( p->token ) + 1 );
		p->token[ 0 ] = '#';

		return;
	}


	//
	//	Scalar types.
	//
	//	In operation dest <- src1 @ src2 at least one of src1 or src2 must match dest type: BPC doesn't
	//	allow all three to be terminators.
	//
	if ( p->left != NULL && do_sizeof( p->type ) != do_sizeof( p->left->type ) )
	{
		print_conversion( p, p->left, out );
		fprintf( f, "%s <- %s %s %s [ %s%d ]\n", p->token, p->token,	
			p->operator ? p->operator->token : "NOP", p->right ? p->right->token : "",
			!is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
			do_sizeof( p->type ) );

		if ( is_assign( p->oper->id ) )
		{
			fprintf( f, "%s <- %s [ %s%d ]\n", p->left->token, p->token,
				!is_scalar( p->left->type ) ? "$" : !is_float( p->left->type ) ? "INT" : "FP", do_sizeof( p->left->type ) );
		}

	}
	else if ( p->right != NULL && do_sizeof( p->type ) != do_sizeof( p->right->type ) )
	{
		print_conversion( p, p->right, out );
		if ( is_assign( p->oper->id ) && p->oper->id != ASSIGN )
		{
			fprintf( f, "%s <- %s ", p->token, p->left->token );
			for ( ii = 0; p->oper->token[ ii ] != '='; ++ii )
				fputc( p->oper->token[ ii ], f );
			fputc( ' ', f );
			fprintf( f, "%s [ %s%d ]\n", p->token, !is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
			fprintf( f, "%s <- %s = %s [ %s%d ]\n", p->token, p->left->token, p->token, !is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
		}
		else
		{
			fprintf( f, "%s <- %s %s %s [ %s%d ]\n", p->token, p->left ? p->left->token : "",
				p->operator ? p->operator->token : "NOP", p->token,
				!is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
		}
	}
	else
	{

		if ( is_assign( p->oper->id ) && p->oper->id != ASSIGN )
		{

			//	Print arithmetic-logic
			fprintf( f, "%s <- %s ", p->left->token, p->left->token );
			for ( ii = 0; p->oper->token[ ii ] != '='; ++ii )
				fputc( p->oper->token[ ii ], f );
			fputc( ' ', f );

			if ( p->right->type->value_valid && !p->right->type->is_var  )
				print_value( p->right->type, f );
			else
				fprintf( f, "%s", p->right && p->right->token ? p->right->token : "NULL" );

			fprintf( f, " [ %s%d ]", !is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
			fprintf( f, "\n" );

			// Print assignment
			fprintf( f, "%s <- %s ", p->token, p->left && p->left->token ? p->left->token : "NULL" );
			fprintf( f, " [ %s%d ]", !is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
			fprintf( f, "\n" );
		}

		else
		{
			fprintf( f, "%s <- ", p->token );

			//	May be unary operator
			if ( p->left && p->left->type )
			{
				fprintf( f, "%s", p->left && p->left->token ? p->left->token : "NULL" );
			}
				
			fprintf( f, " %s ", p->operator && p->operator->token ? p->operator->token : "NOP" );

			if ( p->right->type->value_valid && !p->right->type->is_var  )
				print_value( p->right->type, f );
			else
				fprintf( f, "%s", p->right && p->right->token ? p->right->token : "NULL" );

			fprintf( f, " [ %s%d ]", !is_scalar( p->type ) ? "$" : !is_float( p->type ) ? "INT" : "FP",
				do_sizeof( p->type ) );
			fprintf( f, "\n" );
		}
	}

#if 0
	if ( p->type->value_valid )
	{
		fprintf( out, "__value=" );
		print_value( p->type, f );
		fprintf( out, "\n" );
	}
#endif
}


/*
 *	Destroys parse tree using DFS
 */
void	destroy_parse_tree( parse_tree_t *p )
{
	if ( NULL == p )
		return;

	destroy_parse_tree( p->left );
	destroy_parse_tree( p->operator );
	destroy_parse_tree( p->right );

//
// There is some nasty leak, so that p->token is already freed! Must take care later.
//
//	free( p->token );
//	free( p );
}


void parse_local_decl()
{
}


void	parse_function()
{
	//list_t	*tl2;

	while ( fbrace_level > 0 )
	{
		parse_local_decl();
		tl1 = parse_statement( tl1 );
	}
}


/*
 *	Initialize record to invalid values.
 */
void	init_record( record_t *rec )
{
	rec->name = NULL;
	rec->type.type_id = -1;
	rec->type.type_ref.id = -1;
	rec->type.ptr_list = NULL;
	rec->dim_num = 0;
	rec->dim = NULL;
	rec->is_var = 0;
	rec->value_valid = 0;
	rec->addr = 0xFFFFFFFF;
	//rec->value = NULL;
}


#define	do_unop_for_basic( type, dest, opnd, unop )	\
	do	\
	{	\
		switch ( unop )	\
		{	\
		case BNOT:	\
			( type )( dest ) = ~( type )( opnd );	\
			break;	\
		case LNOT:	\
			( type )( dest ) = !( type )( opnd );	\
			break;	\
		case MINUS:	\
			( type )( dest ) = -( type )( opnd );	\
			break;	\
		case PLUSPLUS:	\
			( type )( dest ) = ++( type )( opnd );	\
			break;	\
		case MINUSMINUS:	\
			( type )( dest ) = --( type )( opnd );	\
			break;	\
		}	\
	}	while ( 0 )


#define	do_unop_for_basic_fp( type, dest, opnd, unop )	\
	do	\
	{	\
		switch ( unop )	\
		{	\
		case LNOT:	\
			( type )( dest ) = !( type )( opnd );	\
			break;	\
		case MINUS:	\
			( type )( dest ) = -( type )( opnd );	\
			break;	\
		case PLUSPLUS:	\
			( type )( dest ) = ++( type )( opnd );	\
			break;	\
		case MINUSMINUS:	\
			( type )( dest ) = --( type )( opnd );	\
			break;	\
		}	\
	}	while ( 0 )


void	do_unop( parse_tree_t *t )
{
	// Unary operation on pointers
	if ( t->right->type->type.ptr_list != NULL )
	{
	}

	if ( t->oper->id == DEFINED )
	{
		macro_t	*m;

		t->type->type.type_id = TID_BASIC;
		t->type->type.type_ref.id = T_INT;
		t->type->value_valid = 1;

		m = lookup_macro( t->right->token );

		if ( m != NULL )
			*( int* )( t->type->value ) = 1;
		else
			*( int* )( t->type->value ) = 0;
	}

	if ( t->oper->id == STAR )
	{
	}

	if ( t->oper->id == AND )
	{
	}

	if ( t->oper->id == SIZEOF )
	{
		t->type->type.type_id = TID_BASIC;
		t->type->type.type_ref.id = T_UINT;
		t->type->value_valid = 1;
		*( unsigned* )( t->type->value ) = do_sizeof( t->right->type );
	}

	// Unary operation on different types.
	switch ( t->right->type->type.type_id )
	{
	case TID_BASIC:
	case TID_NUMCONST:
		switch ( t->right->type->type.type_ref.id )
		{
		case T_CHAR:
			do_unop_for_basic( char, *( char* )t->type->value, *( char* )t->right->type->value, t->oper->id );
			break;
		case T_UCHAR:
			do_unop_for_basic( unsigned char, *( unsigned char* )t->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
			break;
		case T_SHORT:
			do_unop_for_basic( short, *( short* )t->type->value, *( short* )t->right->type->value, t->oper->id );
			break;
		case T_USHORT:
			do_unop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
			break;
		case T_INT:
			do_unop_for_basic( int, *( int* )t->type->value, *( int* )t->right->type->value, t->oper->id );
			break;
		case T_UINT:
			do_unop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->right->type->value, t->oper->id );
			break;
		case T_LONG:
			do_unop_for_basic( long, *( long* )t->type->value, *( long* )t->right->type->value, t->oper->id );
			break;
		case T_ULONG:
			do_unop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
			break;
		case T_FLOAT:
			do_unop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->right->type->value, t->oper->id );
			break;
		case T_DOUBLE:
			do_unop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->right->type->value, t->oper->id );
			break;
		case T_LONGDOUBLE:
			do_unop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->right->type->value, t->oper->id );
			break;
		}
		break;
	case TID_STRUCT:
		break;
	case TID_UNION:
		break;
	case TID_FUNCTION:
		break;
	}
}


void	do_postfix( parse_tree_t *t )
{
}


#define	assign_src( dest, src )	\
	do	\
	{	\
		switch ( src->type.type_ref.id )	\
		{	\
			{	\
			case T_CHAR:	\
				( dest ) = *( char* )( src->value );	\
				break;	\
			case T_UCHAR:	\
				( dest ) = *( unsigned char* )( src->value );	\
				break;	\
			case T_SHORT:	\
				( dest ) = *( short* )( src->value );	\
				break;	\
			case T_USHORT:	\
				( dest ) = *( unsigned short* )( src->value );	\
				break;	\
			case T_INT:	\
				( dest ) = *( int* )( src->value );	\
				break;	\
			case T_UINT:	\
				( dest ) = *( unsigned* )( src->value );	\
				break;	\
			case T_LONG:	\
				( dest ) = *( long* )( src->value );	\
				break;	\
			case T_ULONG:	\
				( dest ) = *( unsigned long* )( src->value );	\
				break;	\
			case T_FLOAT:	\
				( dest ) = *( float* )( src->value );	\
				break;	\
			case T_DOUBLE:	\
				( dest ) = *( double* )( src->value );	\
				break;	\
			case T_LONGDOUBLE:	\
				( dest ) = *( long double* )( src->value );	\
				break;	\
			}	\
		}	\
	}	while ( 0 )



void	do_assign( record_t *dest, record_t *src )
{
	if ( compat_types( dest, src ) == 0 )
	{
		// Error: incompatible types assignment
	}

	if ( dest->type.ptr_list != NULL )
	{
		// Assign to a pointer
	}

	switch( dest->type.type_id )
	{
	case TID_BASIC:
		switch ( dest->type.type_ref.id )
		{
		case T_CHAR:
			assign_src( *( char * )( dest->value ), src );
			break;
		case T_UCHAR:
			assign_src( *( unsigned char * )( dest->value ), src );
			break;
		case T_SHORT:
			assign_src( *( short * )( dest->value ), src );
			break;
		case T_USHORT:
			assign_src( *( unsigned short * )( dest->value ), src );
			break;
		case T_INT:
			assign_src( *( int * )( dest->value ), src );
			break;
		case T_UINT:
			assign_src( *( unsigned * )( dest->value ), src );
			break;
		case T_LONG:
			assign_src( *( long * )( dest->value ), src );
			break;
		case T_ULONG:
			assign_src( *( unsigned long * )( dest->value ), src );
			break;
		case T_FLOAT:
			assign_src( *( float * )( dest->value ), src );
			break;
		case T_DOUBLE:
			assign_src( *( double * )( dest->value ), src );
			break;
		case T_LONGDOUBLE:
			assign_src( *( long double * )( dest->value ), src );
			break;
		}
		break;
	case TID_ENUM:
		assign_src( *( unsigned * )( dest->value ), src );
		break;
	case TID_STRUCT:
		break;
	case TID_UNION:
		break;
	}
}


#define	do_binop_for_basic( type, dest, opnd1, opnd2, binop )	\
	do	\
	{	\
		switch ( binop )	\
		{	\
		case STAR:	\
			( type )( dest ) = ( type )( opnd1 ) * ( type )( opnd2 );	\
			break;	\
		case DIV:	\
			if ( ( type )( opnd2 ) != 0 )	\
				( type )( dest ) = ( type )( opnd1 ) / ( type )( opnd2 );	\
			break;	\
		case MOD:	\
			if ( ( type )( opnd2 ) != 0 )	\
				( type )( dest ) = ( type )( opnd1 ) % ( type )( opnd2 );	\
			break;	\
		case PLUS:	\
			( type )( dest ) = ( type )( opnd1 ) + ( type )( opnd2 );	\
			break;	\
		case MINUS:	\
			( type )( dest ) = ( type )( opnd1 ) - ( type )( opnd2 );	\
			break;	\
		case SHL:	\
			( type )( dest ) = ( type )( opnd1 ) << ( type )( opnd2 );	\
			break;	\
		case SHR:	\
			( type )( dest ) = ( type )( opnd1 ) >> ( type )( opnd2 );	\
			break;	\
		case LESS:	\
			( type )( dest ) = ( type )( opnd1 ) < ( type )( opnd2 );	\
			break;	\
		case LESSEQ:\
			( type )( dest ) = ( type )( opnd1 ) <= ( type )( opnd2 );	\
			break;	\
		case MORE:	\
			( type )( dest ) = ( type )( opnd1 ) > ( type )( opnd2 );	\
			break;	\
		case MOREEQ:\
			( type )( dest ) = ( type )( opnd1 ) >= ( type )( opnd2 );	\
			break;	\
		case EQ:	\
			( type )( dest ) = ( type )( opnd1 ) == ( type )( opnd2 );	\
			break;	\
		case NEQ:	\
			( type )( dest ) = ( type )( opnd1 ) != ( type )( opnd2 );	\
			break;	\
		case AND:	\
			( type )( dest ) = ( type )( opnd1 ) & ( type )( opnd2 );	\
			break;	\
		case XOR:	\
			( type )( dest ) = ( type )( opnd1 ) ^ ( type )( opnd2 );	\
			break;	\
		case OR:	\
			( type )( dest ) = ( type )( opnd1 ) | ( type )( opnd2 );	\
			break;	\
		case LAND:	\
			( type )( dest ) = ( type )( opnd1 ) && ( type )( opnd2 );	\
			break;	\
		case LOR:	\
			( type )( dest ) = ( type )( opnd1 ) || ( type )( opnd2 );	\
			break;	\
		case ASSIGN: \
			( dest ) = ( opnd1 ) = ( opnd2 );	\
			break;	\
		}	\
	}	while ( 0 )


#define	do_binop_for_basic_fp( type, dest, opnd1, opnd2, binop )	\
	do	\
	{	\
		switch ( binop )	\
		{	\
		case STAR:	\
			( type )( dest ) = ( type )( opnd1 ) * ( type )( opnd2 );	\
			break;	\
		case DIV:	\
			( type )( dest ) = ( type )( opnd1 ) / ( type )( opnd2 );	\
			break;	\
		case PLUS:	\
			( type )( dest ) = ( type )( opnd1 ) + ( type )( opnd2 );	\
			break;	\
		case MINUS:	\
			( type )( dest ) = ( type )( opnd1 ) - ( type )( opnd2 );	\
			break;	\
		case LESS:	\
			( type )( dest ) = ( type )( opnd1 ) < ( type )( opnd2 );	\
			break;	\
		case LESSEQ:\
			( type )( dest ) = ( type )( opnd1 ) <= ( type )( opnd2 );	\
			break;	\
		case MORE:	\
			( type )( dest ) = (type )( opnd1 ) > ( type )( opnd2 );	\
			break;	\
		case MOREEQ:\
			( type )( dest ) = ( type )( opnd1 ) >= ( type )( opnd2 );	\
			break;	\
		case EQ:	\
			( type )( dest ) = ( type )( opnd1 ) == ( type )( opnd2 );	\
			break;	\
		case NEQ:	\
			( type )( dest ) = ( type )( opnd1 ) != ( type )( opnd2 );	\
			break;	\
		case LAND:	\
			( type )( dest ) = ( type )( opnd1 ) && ( type )( opnd2 );	\
			break;	\
		case LOR:	\
			( type )( dest ) = ( type )( opnd1 ) || ( type )( opnd2 );	\
			break;	\
		case ASSIGN: \
			( dest ) = ( opnd1 ) = ( opnd2 );	\
			break;	\
		}	\
	}	while ( 0 )


void	do_binop( parse_tree_t *t )
{
	int	id;

	switch ( t->left->type->type.type_ref.id )
	{
		case T_CHAR:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( char, *( char* )t->type->value, *( char* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( unsigned char, *( unsigned char* )t->type->value, *( char* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( short, *( short* )t->type->value, *( char* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( char* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( int, *( int* )t->type->value, *( char* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( char* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( char* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( char* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( char* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( char* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( char* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_UCHAR:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( unsigned char, *( unsigned char* )t->type->value, *( unsigned char* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( unsigned char, *( unsigned char* )t->type->value, *( unsigned char* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( short, *( short* )t->type->value, *( unsigned char* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned char* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( int, *( int* )t->type->value, *( unsigned char* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned char* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( unsigned char* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned char* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( unsigned char* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( unsigned char* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( unsigned char* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_SHORT:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( short, *( short* )t->type->value, *( short* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( short, *( short* )t->type->value, *( short* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( short, *( short* )t->type->value, *( short* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned short, *( short* )t->type->value, *( short* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( int, *( int* )t->type->value, *( short* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( short* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( short* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( short* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( short* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( short* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( short* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_USHORT:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned short* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned short* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned short* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned short, *( unsigned short* )t->type->value, *( unsigned short* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( int, *( int* )t->type->value, *( unsigned short* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned short* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( unsigned short* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned short* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( unsigned short* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( unsigned short* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( unsigned short* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_INT:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( int, *( int* )t->type->value, *( int* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( int, *( int* )t->type->value, *( int* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( int, *( int* )t->type->value, *( int* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( int, *( int* )t->type->value, *( int* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( int, *( int* )t->type->value, *( int* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( int* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( int* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( int* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( int* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( int* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( int* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_UINT:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned, *( unsigned* )t->type->value, *( unsigned* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( unsigned* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( unsigned* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( unsigned* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( unsigned* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_LONG:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( long, *( long* )t->type->value, *( long* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( long* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( long* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( long* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_ULONG:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( char* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned char* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( short* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned short* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( int* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( long* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( long* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic( unsigned long, *( unsigned long* )t->type->value, *( unsigned long* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( unsigned long* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( unsigned long* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( unsigned long* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_FLOAT:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( float, *( float* )t->type->value, *( float* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( float* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( float* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_DOUBLE:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( double, *( double* )t->type->value, *( double* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( double* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
		case T_LONGDOUBLE:
			switch ( t->right->type->type.type_ref.id )
			{
			case T_CHAR:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( char* )t->right->type->value, t->oper->id );
				break;
			case T_UCHAR:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( unsigned char* )t->right->type->value, t->oper->id );
				break;
			case T_SHORT:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( short* )t->right->type->value, t->oper->id );
				break;
			case T_USHORT:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( unsigned short* )t->right->type->value, t->oper->id );
				break;
			case T_INT:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( int* )t->right->type->value, t->oper->id );
				break;
			case T_UINT:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( unsigned* )t->right->type->value, t->oper->id );
				break;
			case T_LONG:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( long* )t->right->type->value, t->oper->id );
				break;
			case T_ULONG:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( unsigned long* )t->right->type->value, t->oper->id );
				break;
			case T_FLOAT:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( float* )t->right->type->value, t->oper->id );
				break;
			case T_DOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( double* )t->right->type->value, t->oper->id );
				break;
			case T_LONGDOUBLE:
				do_binop_for_basic_fp( long double, *( long double* )t->type->value, *( long double* )t->left->type->value, *( long double* )t->right->type->value, t->oper->id );
				break;
			}
			break;
	}
}


unsigned	do_offsetof( record_t *struc, record_t *member )
{
	unsigned size = 0;
	int	i;

	sym_hash_t	*s;

	for ( s = ( ( struct_type_t* )struc->type.type_ref.type )->sym_head; s != NULL; s = s->next )
	{
		if ( strcmp( s->type.name, member->name ) == 0 )
			return	size;
		size += do_sizeof( &s->type );
	}

	return	-1;
}


/*
 *	In BPC, size_t is equivalent to unsigned int.
 */
unsigned	do_sizeof( record_t *rec )
{
	unsigned size = 1;
	int	i;
	record_t	ar;

	if ( rec->type.ptr_list != NULL )
		goto	int_size;

	switch( rec->type.type_id )
	{
	case TID_BASIC: case TID_NUMCONST:
		switch ( rec->type.type_ref.id )
		{
		case T_CHAR: case T_UCHAR:
			size = 1;
			break;
		case T_SHORT: case T_USHORT:
			size = 2;
			break;
		case T_INT: case T_UINT:
int_size:
			size = 4;
			break;
		case T_LONG: case T_ULONG:
			size = 4;
			break;
		case T_FLOAT:
			size = 4;
			break;
		case T_DOUBLE:
			size = 8;
			break;
		case T_LONGDOUBLE:
			size = 10;
			break;
		}
		break;

	case TID_ENUM:
	case TID_ENUMERATOR:
		size = 4;
		break;

	case TID_STRUCT:
		{
			sym_hash_t	*s;

			size = 0;

			for ( s = ( ( struct_type_t* )rec->type.type_ref.type )->sym_head; s != NULL ; s = s->next )
				size += do_sizeof( &s->type );
		}
		break;

	case TID_UNION:
		for ( i = 0, size = 0; i < SYM_HASH_SIZE; ++i )
		{
			sym_hash_t	*s;
			unsigned	size1;

			for ( s = ( ( struct_type_t* )rec->type.type_ref.type )->sym_hash[ i ]; s != NULL; s = s->next )
			{
				size1 = do_sizeof( &s->type );
				if ( size < size1 )
					size = size1;
			}
		}
		break;

	}

	if ( rec->dim_num != 0 )
	{
		for ( i = 0; i < rec->dim_num; ++i )
			size *= rec->dim[ i ];
	}

	return	size;
}


/*
 *	Calculates value for root of the parse tree that represents constant expression.
 */
void	calc_parse_tree( parse_tree_t *t )
{
	if ( is_leaf( t ) )
		return;

	// If simple assignment
	if ( t->left != NULL && t->oper == NULL && t->right == NULL )
	{
		calc_parse_tree( t->left );
		t->type->type = t->left->type->type;
		t->type->value_valid = t->left->type->value_valid;
		memmove( t->type->value, t->left->type->value, sizeof( t->type->value ) );
	}

	// If prefix expression
	if ( t->left == NULL && t->oper != NULL && t->right != NULL )
	{
		calc_parse_tree( t->right );
		t->type->value_valid = t->right->type->value_valid;
		do_unop( t );
	}

	// If postfix expression
	else if ( t->left != NULL && t->oper != NULL && t->right == NULL )
	{
		calc_parse_tree( t->left );
		t->type->value_valid = t->left->type->value_valid;
		do_postfix( t );
	}

	else if ( t->left != NULL && t->oper != NULL && t->right != NULL )
	{
		calc_parse_tree( t->left );
		calc_parse_tree( t->right );
		t->type->value_valid = t->left->type->value_valid && t->right->type->value_valid;
		do_binop( t );
	}
}


void	print_record( record_t *rec )
{
	int	i;
	struct_type_t	*st;
	function_type_t	*fn;
	record_list_t *rl;
	enum_type_t *en;
	enumerator_list_t *el;
	enumerator_type_t *enmt;

	switch( rec->type.type_id )
	{
	case TID_STRUCT:
		printf( "var %s : \nSTRUCT\n", rec->name ? rec->name : "NULL" );
		goto	struct_or_union;
	case TID_UNION:
		printf( "var %s : \nUNION\n", rec->name ? rec->name : "NULL" );
struct_or_union:
		st = rec->type.type_ref.type;
		printf( "TAG: %s\n", st->tag != NULL ? st->tag : "NULL" );
		printf( "addr = %08X\n", rec->addr );
		printf( "MEMBERS:\n" );

		if ( st->sym_hash != NULL )
			for ( i = 0; i < SYM_HASH_SIZE; ++i )
				if ( st->sym_hash[ i ] != NULL )
					print_record( &st->sym_hash[ i ]->type );

		printf( "/MEMBERS\n\n" );

		printf( "TYPES:\n" );

		if ( st->type_hash != NULL )
			for ( i = 0; i < SYM_HASH_SIZE; ++i )
				if ( st->type_hash[ i ] != NULL )
					print_record( &st->type_hash[ i ]->type );

		printf( "/TYPES\n" );

		break;

	case TID_FUNCTION:
		fn = rec->type.type_ref.type;
		printf( "\nFUNCTION %s: returns type#%d (\n", rec->name ? rec->name : "NULL", fn->rv_type.type.type_ref.id );
		for ( rl = fn->param_list; rl != NULL; rl = rl->next )
			printf( "param#%d, name=%s,\n", rl->rec.type.type_ref.id, rl->rec.name ? rl->rec.name : "NOT GIVEN" );
		if ( !fn->defined )
			printf( "Extern\n" );
		if ( fn->var_prm )
			printf( "Variable parameters follow\n" );
		break;

	case TID_ENUM:
		printf( "var %s : ENUM\n", rec->name ? rec->name : "NULL" );
		printf( "addr = %08X\n", rec->addr );
		en = rec->type.type_ref.type;
		printf( "TAG: %s\n", en->tag != NULL ? en->tag : "NULL" );
		printf( "ENUMERATORS:\n" );

		if ( en->enum_list != NULL )
			for ( el = en->enum_list; el != NULL; el = el->next )
				printf( "%s=%d ", el->enumerator->name, el->enumerator->value );
		printf( "\n" );

		// Fall through
	case TID_BASIC:
		printf( "var %s : type#%d\n", rec->name ? rec->name : "NULL", rec->type.type_ref.id );
		printf( "addr = %08X\n", rec->addr );

		if ( rec->value_valid && rec->is_var )
		{
			printf( "value valid; %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X\n",
				rec->value[ 0 ], rec->value[ 1 ], rec->value[ 2 ], rec->value[ 3 ],
				rec->value[ 4 ], rec->value[ 5 ], rec->value[ 6 ], rec->value[ 7 ],
				rec->value[ 8 ], rec->value[ 9 ] );
		}
		break;

	case TID_ENUMERATOR:
		printf( "var %s : ENUMERATOR\n", rec->name ? rec->name : "NULL" );
		enmt = rec->type.type_ref.type;
		printf( "value=%d\n", enmt->value );
		break;

	}

	printf( "spec=%08X\n", rec->spec );

	if ( rec->type.ptr_list != NULL )
	{
		ptr_list_t *p;

		printf( "POINTER; " );
		printf( "addr = %08X\n", rec->addr );
		for ( i = 0, p = rec->type.ptr_list; p != NULL; p = p->next, ++i )
			printf( "grouping=%d, qual=%08X->", p->grouping, p->qual );
		printf( "\nIndirection level = %d\n", i );
	}

	if ( rec->dim_num != 0 )
	{
		int	i;

		printf( "ARRAY; " );

		for ( i = 0; i < rec->dim_num; ++i )
			printf( "[%d]", rec->dim[ i ] );

		printf( "\n" );
	}

	printf("\n" );
}


void	parse_global()
{
	list_t	*t;
	int	i;
//	record_t	rt;

	//	Test parsing of expressions.
	t = token_list;

	root = alloc( sizeof( *root ) );
	init_parse_tree_node( root );

	while ( t != NULL )
	{
		record_t src_rec;
		list_t	*this;
		token_t	*tok;
		char	buf[ 1024 ];

		tok = t->item;
		if ( strncmp( tok->token, "// Including", strlen( "// Including" ) ) == 0 )
		{
			sscanf( tok->token, "// Including %s", buf );
			this = alloc( sizeof( *this ) ); 
			this->item = alloc( sizeof( *this->item ) );
			this->item->token = alloc( strlen( buf ) + 1 );
			strcpy( this->item->token, buf );
			*strstr( this->item->token, ".temp" ) = '\0';
			this->item->line = 1;
			this->item->pos = 1;
			this->next = curr_file;
			curr_file = this;
		}
		else if ( strncmp( tok->token, "// End of", strlen( "// End of" ) ) == 0 )
		{
			free( curr_file->item->token );
			free( curr_file->item );
			this = curr_file->next;
			free( curr_file );
			curr_file = this;
		}

		init_record( &src_rec );
		t = parse_declaration( t, PARSE_GLOBAL, &src_rec );

		if ( t == NULL )
			continue;

		//	Initalization will be handled later.
		if ( t->item->id != EOSTM && t->item->id != RFBRACE )
		{
		//	report_error_tok( MISSING_EOSTM, t->item );
			//t = skip_to_term( t );
			//break;
		}
		else
		{
			t = skip_wsp( t->next );
		}

	} // while (not EOF)

	if ( global_flags & GFL_PRINT_TABLES )
	{
		printf( "TYPES TABLE:\n\n" );

		for ( i = 0; i < TYPE_HASH_SIZE; ++i )
		{
			sym_hash_t *h;

			if ( type_hash[ i ] != NULL )
			{
				for ( h = type_hash[ i ]; h != NULL; h = h->next )
				{
					print_record( &h->type );
				}
			}
		}

		printf( "SYMBOL TABLE:\n\n" );

		for ( i = 0; i < SYM_HASH_SIZE; ++i )
		{
			sym_hash_t *h;

			if ( sym_hash[ i ] != NULL )
			{
				for ( h = sym_hash[ i ]; h != NULL; h = h->next )
				{
					print_record( &h->type );
				}
			}
		}

		printf( "FUNCTIONS TABLE:\n\n" );

		for ( i = 0; i < FUNC_HASH_SIZE; ++i )
		{
			sym_hash_t *h;

			if ( func_hash[ i ] != NULL )
			{
				for ( h = func_hash[ i ]; h != NULL; h = h->next )
				{
					print_record( &h->type );
				}
			}
		}
	}
}


/*
 *	Concatenate literals and reap off the trailing '"' that remains from lexer.
 */
static	__inline	concat_literals( list_t *t )
{
	list_t *t1, *t2;

	while ( t != NULL )
	{
		if ( t->item->id == STRCONST )
		{
			t->item->token[ strlen( t->item->token ) - 1 ] = '\0';
			t1 = skip_wsp( t->next );
			while( t1->item->id == STRCONST )
			{
				t->item->token = realloc( t->item->token, strlen( t->item->token ) + strlen( t1->item->token ) + 1 );
				strcpy( t->item->token, t1->item->token );
				t2 = t->next;
				t->next = t;
				t = t->next;

				for ( t = t2; t2 != t1; )
				{
					t2 = t2->next;
					free( t->item->token );
					free( t->item );
					free( t );
					t = t2;
				}
				t1 = skip_wsp( t->next );
			}
			memmove( t->item->token, t->item->token + 1, strlen( t->item->token ) );
		}
		t = skip_wsp( t->next );
	}
}


void	parser_entry( char *srcf )
{
	FILE *f;
	unsigned long	flen;

	FILE *dumpf;
	char	pp_name[ 256 ];

	f = fopen( srcf, "rt" );
	if ( NULL == f )
	{
		fprintf( stderr, "Source file '%s' not found\n", srcf );
		exit( 0 );
	}
	fclose( f );

	sprintf( pp_name, "%s.pp", srcf );
	preprocess( srcf, pp_name );

	f = fopen( pp_name, "rb" );
	fseek( f, 0, SEEK_END );
	flen = ftell( f );
	fseek( f, 0, SEEK_SET );
	glob_src = src = alloc( flen + 1 );
	memset( src, 0, flen + 1 );
	fread( src, 1, flen, f );
	fclose( f );

	//freopen( "dumpfile", "wt", stdout );

	curr_file = alloc( sizeof( *curr_file ) );
	curr_file->item = alloc( sizeof( *curr_file->item ) );
	curr_file->item->token = alloc( strlen( srcf ) + 1 );
	strcpy( curr_file->item->token, srcf );
	curr_file->item->line = 1;
	curr_file->item->pos = 1;
	curr_file->next = NULL;

	while ( *src != 0 )
	{
		list_t	*this;
		char	buf[ 1024 ];

		tok = get_token( 0 );	// src );

		if ( tok != NULL )
		{
			char	*p;


			if ( strncmp( tok->token, "// Including", strlen( "// Including" ) ) == 0 )
			{
				sscanf( tok->token, "// Including %s", buf );
				this = alloc( sizeof( *this ) ); 
				this->item = alloc( sizeof( *this->item ) );
				this->item->token = alloc( strlen( buf ) + 1 );
				strcpy( this->item->token, buf );
				*strstr( this->item->token, ".temp" ) = '\0';
				this->item->line = 1;
				this->item->pos = 1;
				this->next = curr_file;
				curr_file = this;
			}
			else if ( strncmp( tok->token, "// End of", strlen( "// End of" ) ) == 0 )
			{
				free( curr_file->item->token );
				free( curr_file->item );
				this = curr_file->next;
				free( curr_file );
				curr_file = this;
			}

			for ( p = tok -> token; *p != 0; ++p )
			{
				if ( *p == '\n' )
				{
					++curr_file->item->line;
					curr_file->item->pos = 1;
				}
				else
				{
					++curr_file->item->pos;			// Tab cannot be set.
				}
			}

			line = curr_file->item->line;
			pos = curr_file->item->pos;

			tok -> line = line;
			tok -> pos = pos;

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

	// Dump tokens
	if ( global_flags & GFL_PRINT_TOKENS )
	{
		for ( tl1 = token_list; tl1 != NULL; tl1 = tl1 -> next )
		{
			tok = tl1 -> item;
			printf( "'%s' -> '%s' at line: %d, pos: %d\n", token[ tok -> id ], tok -> token, tok -> line, tok -> pos );
		}
	}

	concat_literals( token_list );

	out = fopen( outf, "wt" );
	parse_global();
	fclose( out );

	if ( 0 == err_count )
		generate_asm( outf, out_asmf );

	for ( tl1 = token_list; tl1 != NULL; tl1 = token_list )
	{
		token_list = tl1 -> next;
		tok = tl1 -> item;
		free( tok -> token );
		free( tok );
		free( tl1 );
	}
	free( glob_src );

	printf( "Compilation %s : %d errors\n", err_count == 0 ? "succeeded" : "failed", err_count );
}
