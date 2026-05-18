/*
 *	PARSER.H
 *	--------
 *
 *	Syntactical analyzer and parser for C - header.
 *	Reference - the ISO C 9899:1999 (C99) Standard.
 */

#include	<stdio.h>
#include	"lexer.h"

enum	errors	{ BAD_RFBRACE, BAD_RSUBSCR, BAD_RPAREN, MISSING_RFBRACE, MISSING_RSUBSCR,
			MISSING_RPAREN, MISSING_BINOPND, MISSING_ID, BAD_MEMBER, MISSING_LPAREN,
			MISSING_GOTO_LABEL, MISSING_EOSTM, MISSING_WHILE_IN_DO,
			MISSING_BINOPR, STORAGE_SPEC, TYPE_SPEC, MISSING_COMMA,
			BAD_INITIALIZER, SYNTAX_ERROR, SUBSCRIPT_TOO_HIGH, AGGREGATE_INIT, NOT_MEMBER,
			INIT_NOT_CONSTANT, MAX_NEST_LEVEL_EXC, VOID_OBJECT, INIT_NOT_LIST, MISSING_INIT,
			FUNC_MEMBER, BAD_EOF, DEFAULT_USED, CASE_USED, MISSING_WHILE, BAD_BREAK,
			MUST_RETURN, VOID_RETURNS, INCOMPAT_RETURN, MISSING_LFBRACE, MISSING_LABEL, UNDEF_SYM,
			MISSING_COLON, DEREF_NOT_STRUCT, PTR_TO_STRUCT, STRUCT_OBJECT, INCOMPAT_TYPES };

//
//	Basic types. The types are enumerated in such a way that higher numbed denotes higher
//	conversion rank. Also, signed integer types come first, so that the conversion is first
//	tried for signed integers, as required by the standard.
//
//	Reference:	Types (6.1.2.5)
//
enum	types	{ T_CHAR, T_UCHAR, T_SHORT, T_USHORT, T_INT, T_UINT, T_LONG, T_ULONG, T_LONGLONG, T_ULONGLONG,
	T_FLOAT, T_DOUBLE, T_LONGDOUBLE, T_VOID };

//	Specifies derived types and other types.
enum	type_id	{ TID_BASIC, TID_VOID, TID_ENUM, TID_ARRAY, TID_STRUCT, TID_UNION, TID_FUNCTION, TID_PTR, TID_TYPEDEF, 
		TID_ENUMERATOR, TID_NUMCONST, TID_ADDRCONST, TID_STRCONST };


#define	CAST_EXPR	1000


#define	DFA_START		0
#define	GOT_OPND1		1
#define	GOT_BINOP		2
#define	GOT_OPND2		3
#define	DFA_ACCEPT		4

//
//	DFA works for left-associativity: linearly process binary operators of 
//	non-increasing precedence.
//
#define	DFA_SEL_LEFT	0
//
//	DFA works for right-associativity: linearly process binary operators of 
//	increasing precedence.
//
#define	DFA_SEL_RIGHT	1


typedef	struct
{
	int	id;
}	basic_type_t;	// Includes TID_VOID, TID_ENUM


// For typedef tables
typedef	union
{
	int	id;				// For basic types
	void	*type;		// For derived types.
}	type_ref_t;


#define	GROUP_LEFT		0
#define	GROUP_RIGHT		1

typedef	struct ptr_list_t
{
	struct ptr_list_t	*next;
	unsigned qual;
	int	grouping;		// 0 - group toward specifiers, 1 - toward identifier.
} ptr_list_t;


typedef	struct
{
	int	type_id;
	type_ref_t	type_ref;
	ptr_list_t	*ptr_list;
}	type_t;


typedef	struct	array_type_t
{
	type_t	elem_type;
	int     dim_num;		// Number of dimensions: 0 = not an array
	int     *dim;			// Array of dimensions
}	array_type_t;


typedef	struct	struct_type_t
{
	char	*tag;
	struct sym_hash_t	**sym_hash, **type_hash;
	struct sym_hash_t	*sym_head, *sym_tail;		// Members of the structure must appear in order.
}	struct_type_t;


typedef	struct	enumerator_type_t
{
	char	*name;
	int		value;
}	enumerator_type_t;


typedef	struct	enumerator_list_t
{
	enumerator_type_t	*enumerator;
	struct	enumerator_list_t *next;
}	enumerator_list_t;


typedef	struct	enum_type_t
{
	char	*tag;
	enumerator_list_t *enum_list;
}	enum_type_t;


typedef	struct	operand_t
{
	char	*token;
	type_t	*type;
}	operand_t;


typedef	struct	pointer_type_t
{
	unsigned	spec;
	type_t	type;
}	pointer_type_t;


//	Generic record type.
typedef	struct	record_t
{
	char    *name;
	type_t	type;
	unsigned spec;			// Specifiers set that were used during declaration.
	int     dim_num;		// Number of dimensions: 0 = not an array
	int     *dim;			// [dim_num] Dimensions (has a sense only if dim_num != 0 ). -1 means variable-length.
	int		value_valid;	// Non-0 if value[] contents can be used in a constant expression
	unsigned char	value[ 10 ];	// Value of this record - 10 is the longest sizeof( scalar type ).
	int		is_var;			// If 0 then it's a constant - named (enumerator) or numeric.
	unsigned addr;			// Address: absolute for statics, relative to frame base address - for block scope
}	record_t;


typedef	struct	parse_tree_t
{
	char	*token;		// Where applicable
	int		id;			// Where applicable.
	record_t	*type;	// Type
	struct	parse_tree_t *parent;		// Daddy
	struct	parse_tree_t *left, *oper, *right;	// subexpression for result
	int		seq_point;	// If true then after this node commit sequence point.
}	parse_tree_t;


typedef	struct	record_list_t
{
	record_t	rec;
	struct	record_list_t	*next;
}	record_list_t;


typedef	struct	function_type_t
{
	struct	record_t	rv_type;
	struct record_list_t	*param_list;			// List of mandatory params
	int	var_prm;					// Non-0 if variable number of params is allowed after mandatory params
	int	defined;
}	function_type_t;


typedef	struct	sym_hash_t
{
	record_t	type;
	struct  sym_hash_t *next;	// Chaining list for overlapping hashes.
}	sym_hash_t;


//	List of symbol hash tables (environments)
typedef	struct	hash_list_t
{
	sym_hash_t	**hash;
	struct	hash_list_t	*next;
	unsigned	env_size;
}	hash_list_t;


//
//	Functions declarations
//
list_t	*parse_expression( parse_tree_t *curr, parse_tree_t *first_opnd, list_t *t, 
						  record_t *ret_type, int dfa_sel, int flags );
void	calc_parse_tree( parse_tree_t *t );
list_t	*skip_wsp( list_t *t );
void	add_symbol( record_t *type, int size, sym_hash_t **tbl );
void	add_type( record_t *type, sym_hash_t **tbl );
int		name2type( list_t *token, record_t *type );
record_t	*lookup_var( char *name );
record_t	*lookup_symbol( char *name, int size, sym_hash_t **tbl );
record_t	*lookup_type( record_t *type_rec, sym_hash_t **type_hash );
int	compat_types( record_t *rec1, record_t *rec2 );
void	assign_temp_types( parse_tree_t *p );
void	assign_temp_names( parse_tree_t *p );
void	destroy_parse_tree( parse_tree_t *p );
unsigned	do_sizeof( record_t *rec );
void	print_parse_tree( parse_tree_t *p, FILE *f );
int	hash_func( char *name, int size );
void	init_parse_tree_node( parse_tree_t *node );
void	preprocess( char *src, char *dest );
token_list_t *parse_initializer( token_list_t *t, record_t *dest, int control_flags );
token_list_t *parse_declaration( token_list_t *t, int control_flags, record_t *dest );
char	*binopr2str( int id );
int	is_leaf( parse_tree_t *p );
list_t	*parse_statement( list_t *t );
void	init_record( record_t *rec );
void	generate_asm( char *src_file, char *dest_file );
unsigned	do_offsetof( record_t *struc, record_t *member );
void	commit_seqpoint( FILE *f );
void	report_error( int err_num, token_t *tok );
void	report_error_tok( int err_num, token_t *tok );
list_t	*skip_to_term( list_t *t );
void	add_env( void );
void	init_env( void );
void	remove_env( void );
void	free_env( void );
void	parser_entry( char *srcf );
int		is_scalar( record_t *rec );
int		is_float( record_t *rec );
int		is_signed( record_t *rec );
void	generate_init( sym_hash_t **sym_hash, int control_flags );



#define	SYM_HASH_SIZE		512
#define	TYPE_HASH_SIZE		512
#define	FUNC_HASH_SIZE		512
#define	LIT_HASH_SIZE		512

#define	SPEC_VOID			1
#define	SPEC_CHAR			2
#define	SPEC_INT			4
#define	SPEC_LONG			8
#define	SPEC_SHORT			0x10
#define	SPEC_SIGNED			0x20
#define	SPEC_UNSIGNED		0x40
#define	SPEC_FLOAT			0x80
#define	SPEC_DOUBLE			0x100
#define	SPEC_STRUCT			0x200
#define	SPEC_UNION			0x400
#define	SPEC_ENUM			0x800

#define	SPEC_TYPE			0xFFF

#define	SPEC_TYPEDEF		0x1000

#define	SPEC_AUTO			0x2000
#define	SPEC_EXTERN			0x4000
#define	SPEC_STATIC			0x8000
#define	SPEC_REGISTER		0x10000

#define	SPEC_STORAGE		0x1E000

#define	SPEC_CONST			0x20000
#define	SPEC_VOLATILE		0x40000
#define	SPEC_RESTRICT		0x80000

#define	SPEC_INLINE			0x100000

// For these it may be necessary to employ pointer_type_t (?)
#define	SPEC_CONST_PTR		0x200000
#define	SPEC_VOLATILE_PTR	0x400000

#define	DFLT_CHAR_UNSIGNED	1
//#define	DFLT_INT_UNSIGNED	2

#if 0

enum	{ PARSE_NORMAL, PARSE_CONSTANT, PARSE_INTCONST, PARSE_ARITHCONST,
			PARSE_ADDRCONST, PARSE_GLOBAL, PARSE_LOCAL, PARSE_MEMBER, PARSE_ARGUMENT, 
			PARSE_INIT, PARSE_SUBSCR, PARSE_CONDITION, PARSE_PPROC_CONDITION };		// Expression parse flag

#else

#define	PARSE_NORMAL		1
#define	PARSE_CONSTANT		(1<<1)
#define	PARSE_INTCONST		(1<<2)
#define	PARSE_ARITHCONST	(1<<3)
#define	PARSE_ADDRCONST		(1<<4)
#define	PARSE_GLOBAL		(1<<5)
#define	PARSE_LOCAL			(1<<6)
#define	PARSE_MEMBER		(1<<7)
#define	PARSE_ARGUMENT		(1<<8)
#define	PARSE_INIT			(1<<9)
#define	PARSE_SUBSCR		(1<<10)
#define	PARSE_CONDITION		(1<<11)
#define	PARSE_PPROC_CONDITION (1<<12)
#define	PARSE_COND_OPER		(1<<13)

#endif

enum	{ TYPES_INCOMPAT, TYPES_COMPAT, TYPES_REQ_CAST };		// Types compatibility

#define	CONT_VALID	1
#define	BREAK_VALID	2


#define	MACRO_HASH_SIZE		4096
#define	MAX_MACRO_SIZE		16384
#define	MAX_TOKEN_SIZE		4096
#define	MAX_FUNC_PARAMS		512

typedef	struct	macro_t
{
	char	*name;
	char	**prm_list;
	char	*text;
	struct macro_t	*next;
}	macro_t;

macro_t	*lookup_macro( char *name );

//
// Parser global flags
//

//	Print lexer's output (tokens list)
#define	GFL_PRINT_TOKENS	1
#define	GFL_PRINT_TABLES	2