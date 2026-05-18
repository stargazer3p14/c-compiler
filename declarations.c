/*
 *	DECLARATIONS.C
 *	--------------
 *
 *	Source for parsing declarations.
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"


extern	parse_tree_t	*root, *current;
extern	sym_hash_t	*sym_hash[ SYM_HASH_SIZE ], *type_hash[ TYPE_HASH_SIZE ], *func_hash[ FUNC_HASH_SIZE ];
extern	unsigned	def_types;
extern	FILE	*out;
extern	hash_list_t	*envlist_top, *envlist_bot, *typelist_top, *typelist_bot;
extern	int		fp_const_count;
extern	int		subscr_level;
extern	int		env_nest_count;

int	init_fbrace_level = 0;
unsigned	curr_addr = 0;

function_type_t	*curr_function;

extern	int		glob_error;				// Error symbol - used for recovery from nested recursions.

int		is_declspec( int id )
{
	switch ( id )
	{
	default:
		return	0;
	case CHAR: case INT: case LONG: case SHORT: case FLOAT: case DOUBLE:
	case CONST: case VOLATILE: case REGISTER: case RESTRICT:
	case VOID: case SIGNED: case UNSIGNED:
	case STRUCT: case UNION:
		return	1;
	}
}


/*
 *	Parses assignment for otherwise completed type.
 *
 *	On input t points to the start of constant expression.
 *	Meanwhile allow only immediate values. Later allow full parsing of a constant expression.
 */
token_list_t *parse_initializer( token_list_t *t, record_t *dest, int control_flags )
{
	record_t	rt;
	parse_tree_t	super_root;
	int	i;

	init_parse_tree_node( &super_root );
	super_root.type = dest;
	super_root.left = root;
	root->parent = &super_root;

	if ( NULL == t )
		return	t;

	if ( t->item->id == RFBRACE )
	{
		--init_fbrace_level;
		return	skip_wsp( t->next );
	}

	if ( t->item->id == LFBRACE )
	{
		++init_fbrace_level;
		t = parse_initializer( skip_wsp( t->next ), dest, control_flags );
	}

	if ( dest->dim_num > 0 )
	{
		// Parse array initialization
		int	curr_object = 0;
		record_t	sub_obj;
		int	i, j;
		unsigned count;
		unsigned elem_size = 0;

		fprintf( out, "\t%s\tLABEL\tBYTE\n", dest->name );

		init_record( &sub_obj );
		sub_obj = *dest;
		sub_obj.dim_num = 0;
		elem_size = do_sizeof( &sub_obj );

		sub_obj.dim_num = dest->dim_num - 1;

		for ( i = 1, count = 0; i < dest->dim_num; ++i )
			count += dest->dim[ i ] * elem_size;

		sub_obj.dim = alloc( count );

		for ( i = 0; i < dest->dim[ 0 ]; ++i )
		{
			if ( t->item->id == RFBRACE )
				return	t;

			if ( t->item->id == LSUBSCR )
			{
				parse_tree_t	*tr;
				record_t	rt;

				tr = alloc( sizeof( *tr ) );
				init_parse_tree_node( tr );
				t = parse_expression( tr, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_INTCONST );

				if ( t->item->id != RSUBSCR )
				{
					// Error: missing ']'
					report_error( MISSING_RSUBSCR, t->item );
					t = skip_to_term( t );
				}

				t = skip_wsp( t->next );
				if ( t->item->id != ASSIGN )
				{
					// Error: missing '='
					report_error( SYNTAX_ERROR, t->item );
					t = skip_to_term( t );
				}
				t = skip_wsp( t->next );

				assign_temp_types( tr );
				assign_temp_names( tr );
				calc_parse_tree( tr );
				sscanf( tr->type->value, "%u", &count );
				destroy_parse_tree( tr );

				if ( t->item->id == RFBRACE )
					continue;
				if ( t->item->id != COMMA )
				{
					// Error: missing ',' or '}'
					report_error( SYNTAX_ERROR, t->item );
					t = skip_to_term( t );
				}

				if ( count > dest->dim[ 0 ] )
				{
					// Error: initializer subscript too high
					report_error( SUBSCRIPT_TOO_HIGH, t->item );
					t = skip_to_term( t );
				}
				if ( control_flags & PARSE_GLOBAL || dest->spec & SPEC_STATIC )
					fprintf( out, "\tDB\t%u DUP (?)\n", do_sizeof( dest ) / dest->dim[ 0 ] * ( count - i ) );
				i = count;
			}

			t = parse_initializer( t, &sub_obj, control_flags | PARSE_MEMBER );

			if ( t->item->id == RFBRACE )
				continue;

			if ( t->item->id != COMMA )
			{
				// Error: ',' or '}' was expected
				report_error( SYNTAX_ERROR, t->item );
				t = skip_to_term( t );
			}
		}
	}

	if ( dest->type.ptr_list != NULL )
	{
		// Parse initalization of a pointer
		goto	parse_int_init;
	}

	switch( dest->type.type_id )
	{
		sym_hash_t	*member;

	case TID_BASIC:
	case TID_ENUM:										// probably enums must be set as basic ints by now
parse_int_init:
		current = root = alloc( sizeof( *root ) );
		t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_INIT );

		// Calculate value of the parse tree.
		calc_parse_tree( root );
		dest->value_valid = root->type->value_valid;

		//	Must do necessary cast.
		do_assign( dest, root->type );
		break;

	case TID_STRUCT: case TID_UNION:
		fprintf( out, "\t%s\tLABEL\tBYTE\n", dest->name );
		if ( init_fbrace_level == 0 )
		{
			// Error: initialization of aggregate types must be enclosed in '{}'
			report_error( SYNTAX_ERROR, t->item );
			t = skip_to_term( t );
		}

		for ( member = ( ( struct_type_t* )dest->type.type_ref.type )->sym_head; member != NULL; member = member->next )
		{
			if ( t->item->id == MEMBERACC )
			{
				t = skip_wsp( t->next );

				if ( !lookup_symbol( t->item->token, TYPE_HASH_SIZE, ( ( struct_type_t* )dest->type.type_ref.type )->sym_hash ) )
				{
					// Error: 't->token' is not a member of struct/union 'dest'
					report_error( NOT_MEMBER, t->item );
					t = skip_to_term( t );
				}

				if ( control_flags & PARSE_GLOBAL || dest->spec & SPEC_STATIC )
					fprintf( out, "\tDB\t%u DUP (?)\n", 
						do_offsetof( dest, lookup_symbol( t->item->token, TYPE_HASH_SIZE, ( ( struct_type_t* )dest->type.type_ref.type )->sym_hash ) ) - 
						do_offsetof( dest, &member->type ) );

				while ( strcmp( member->type.name, t->item->token ) )
					member = member->next;

				t = skip_wsp( t->next );
				if ( t->item->id != ASSIGN )
				{
					// Error: missing '='
					report_error( SYNTAX_ERROR, t->item );
					t = skip_to_term( t );
				}
				t = skip_wsp( t->next );
			}
			t = parse_initializer( t, &member->type, control_flags | PARSE_MEMBER );
		}

		break;

	case TID_FUNCTION:
		break;
	}

	return	t;
}


sym_hash_t	**local_sym_hash, **local_type_hash;


/*
 *	Parses declarations, according to 6.5
 *
 *	Meanwhile works with global scope only, local scopes will be added later.
 *
 *	This is a straight-forward automaton. It parses the declaration in straight-forward
 *	manner, according to specifications given in section 6.5. During parsing, it fills
 *	a type record. By the end it checks whether the type record is full and if it is
 *	adds the declared symbol to the symbol table.
 */
token_list_t *parse_declaration( token_list_t *t, int control_flags, record_t *dest )
{
	list_t	*head = NULL, *tail = NULL;		// Keep list of init-declarators.
	record_t	rec, *rec_src;
	int		done = 0;
	unsigned	spec = 0;
	int	i;
	struct_type_t *st;
	enum_type_t	*en;
	unsigned	temp_addr;
	sym_hash_t	**temp_sym_hash, **temp_type_hash;
	record_t func_rec;
	function_type_t	*ft;
	record_list_t *prm;
	ptr_list_t	*ptrl, *ptrl1;
	char	buf[ 1024 ];
	record_t	rt;

	init_record( &rec );

	//
	// First must come declaration-specifiers (stage 1).
	//
	while ( !done )
	{
		if ( t == NULL )
		{
			done = 1;
			continue;
		}

		switch ( t->item->id )
		{
		default:
			//	Some error.

			//report_error( SYNTAX_ERROR, t->item );
			//t = skip_to_term( t );
			break;

		case WSPACE:
			t = skip_wsp( t->next );
			continue;

		case STAR: case LPAREN: case EOSTM:
			done = 1;
			continue;

		case COMMA: case RPAREN:
			if ( control_flags == PARSE_ARGUMENT )
			{
				done = 1;
				continue;
			}
			report_error( SYNTAX_ERROR, t->item );
			t = skip_to_term( t );
			// Error: misplaced ',' or ')' in declaration.

		case ID:
			{
				record_t *r;

				r = lookup_symbol( t->item->token, TYPE_HASH_SIZE, type_hash );

				if ( NULL == r )
				{
					done = 1;
					continue;
				}
				spec |= r->spec & ~SPEC_TYPEDEF;

				//	Will need to check validity of resulting type.

				rec.type = r->type;
				break;
			}

		// Storage-class specifiers.
		case AUTO:
			if ( spec & SPEC_STORAGE )
			{
				report_error_tok( STORAGE_SPEC, t->item );
				return	skip_to_term( t );
			}
			spec |= SPEC_AUTO;
			break;

		case EXTERN:
			if ( spec & SPEC_STORAGE )
			{
				report_error_tok( STORAGE_SPEC, t->item );
				return	skip_to_term( t );
			}
			spec |= SPEC_EXTERN;
			break;

		case STATIC:
			if ( spec & SPEC_STORAGE )
			{
				report_error_tok( STORAGE_SPEC, t->item );
				return	skip_to_term( t );
			}
			spec |= SPEC_STATIC;
			break;

		case REGISTER:
			if ( spec & SPEC_STORAGE )
			{
				report_error_tok( STORAGE_SPEC, t->item );
				return	skip_to_term( t );
			}
			spec |= SPEC_REGISTER;
			break;

		//
		// Type specifiers.
		//
		//	Left out is: enum.
		//
		case VOID:
			if ( spec & SPEC_TYPE  )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_VOID;
			rec.type.type_id = TID_VOID;
			break;

		case CHAR:
			if ( spec & ( SPEC_TYPE ^ SPEC_SIGNED ^ SPEC_UNSIGNED ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_CHAR;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_SIGNED )
				rec.type.type_ref.id = T_CHAR;
			else if ( spec & SPEC_UNSIGNED )
				rec.type.type_ref.id = T_UCHAR;
			else if ( def_types & DFLT_CHAR_UNSIGNED )
				rec.type.type_ref.id = T_UCHAR;
			else
				rec.type.type_ref.id = T_CHAR;
			break;

		case SIGNED:
			if ( spec & ( SPEC_TYPE ^ SPEC_SIGNED ^ SPEC_CHAR ^ SPEC_SHORT ^ SPEC_INT ^ SPEC_LONG ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_SIGNED;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_CHAR )
				rec.type.type_ref.id = T_CHAR;
			else if ( spec & SPEC_INT )
				rec.type.type_ref.id = T_INT;
			else if ( spec & SPEC_LONG )
				rec.type.type_ref.id = T_LONG;
			else if ( spec & SPEC_SHORT )
				rec.type.type_ref.id = T_SHORT;
			else
				rec.type.type_ref.id = T_INT;
			break;

		case UNSIGNED:
			if ( spec & ( SPEC_TYPE ^ SPEC_UNSIGNED ^ SPEC_CHAR ^ SPEC_SHORT ^ SPEC_INT ^ SPEC_LONG ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_UNSIGNED;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_CHAR )
				rec.type.type_ref.id = T_UCHAR;
			else if ( spec & SPEC_INT )
				rec.type.type_ref.id = T_UINT;
			else if ( spec & SPEC_LONG )
				rec.type.type_ref.id = T_ULONG;
			else if ( spec & SPEC_SHORT )
				rec.type.type_ref.id = T_USHORT;
			else
				rec.type.type_ref.id = T_UINT;
			break;

		case INT:
			if ( spec & ( SPEC_TYPE ^ SPEC_SIGNED ^ SPEC_UNSIGNED ^ SPEC_SHORT ^ SPEC_LONG ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_INT;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_SIGNED )
			{
				if ( spec & SPEC_SHORT )
					rec.type.type_ref.id = T_SHORT;
				else if ( spec & SPEC_LONG )
					rec.type.type_ref.id = T_LONG;
				else
					rec.type.type_ref.id = T_INT;
			}
			else if ( spec & SPEC_UNSIGNED )
			{
				if ( spec & SPEC_SHORT )
					rec.type.type_ref.id = T_USHORT;
				else if ( spec & SPEC_LONG )
					rec.type.type_ref.id = T_ULONG;
				else
					rec.type.type_ref.id = T_UINT;
			}
			else
				rec.type.type_ref.id = T_INT;
			break;

		case SHORT:
			if ( spec & ( SPEC_TYPE ^ SPEC_INT ^ SPEC_SIGNED ^ SPEC_UNSIGNED ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_SHORT;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_UNSIGNED )
				rec.type.type_ref.id = T_USHORT;
			else
				rec.type.type_ref.id = T_SHORT;
			break;

		case FLOAT:
			if ( spec & SPEC_TYPE  )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_FLOAT;
			rec.type.type_id = TID_BASIC;
			rec.type.type_ref.id = T_FLOAT;

			break;

		case DOUBLE:
			if ( spec & ( SPEC_TYPE ^ SPEC_LONG ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_DOUBLE;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_LONG )
				rec.type.type_ref.id = T_LONGDOUBLE;
			else
				rec.type.type_ref.id = T_DOUBLE;

			break;

		case LONG:
			if ( spec & ( SPEC_TYPE ^ SPEC_INT ^ SPEC_SIGNED ^ SPEC_UNSIGNED ^ SPEC_DOUBLE ) )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_LONG;
			rec.type.type_id = TID_BASIC;

			if ( spec & SPEC_DOUBLE )
				rec.type.type_ref.id = T_LONGDOUBLE;
			else if ( spec & SPEC_UNSIGNED )
				rec.type.type_ref.id = T_ULONG;
			else
				rec.type.type_ref.id = T_LONG;

			break;

		case STRUCT:
			if ( spec & SPEC_TYPE )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_STRUCT;
			rec.type.type_id = TID_STRUCT;
			sprintf( buf, "__struct_" );
			goto	struct_or_union;

		case UNION:			   
			if ( spec & SPEC_TYPE )
			{
				report_error_tok( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_UNION;
			rec.type.type_id = TID_UNION;
			sprintf( buf, "__union_" );

struct_or_union:
			st = rec.type.type_ref.type = alloc( sizeof( struct_type_t ) );

			t = skip_wsp( t->next );

			if ( t->item->id == ID )
			{
				st->tag = alloc( strlen( t->item->token ) + 1 );
				strcpy( st->tag, t->item->token );
				t = skip_wsp( t->next );
				strcat( buf, st->tag );
			}
			else
				st->tag = NULL;

			if ( t->item->id == LFBRACE )
			{
				// Structure definition.

				st->sym_hash = alloc( SYM_HASH_SIZE * sizeof( sym_hash_t* ) );
				st->type_hash = alloc( TYPE_HASH_SIZE * sizeof( sym_hash_t* ) );

				for ( i = 0; i < SYM_HASH_SIZE; ++i )
					st->sym_hash[ i ] = NULL;

				for ( i = 0; i < TYPE_HASH_SIZE; ++i )
					st->type_hash[ i ] = NULL;

				// sym_head and sym_tail are alternative pointers to structure/union members as a list
				st->sym_head = st->sym_tail = NULL;

				temp_addr = curr_addr;
				curr_addr = 0;
				temp_sym_hash = local_sym_hash;
				temp_type_hash = local_type_hash;
				local_sym_hash = st->sym_hash;
				local_type_hash = st->type_hash;

				while ( t != NULL && t->item->id != RFBRACE )
				{
					record_t member_rec;

					t = parse_declaration( t, PARSE_MEMBER, &member_rec );

					if ( t->item->id != EOSTM )
					{
						report_error_tok( MISSING_EOSTM, t->item );
						//t = skip_to_term( t );
					}
					else
					{
						t = skip_wsp( t->next );
					}

					if ( 0 == ( member_rec.spec & SPEC_TYPEDEF ) )
					{
						member_rec.is_var = 1;
						add_symbol( &member_rec, SYM_HASH_SIZE, st->sym_hash );

						if ( NULL == st->sym_head )
							st->sym_head = st->sym_tail = alloc( sizeof( *st->sym_tail ) );
						else
							st->sym_tail = st->sym_tail->next = alloc( sizeof( *st->sym_tail ) );
						st->sym_tail->next = NULL;
						st->sym_tail->type = member_rec;
					}
					else
					{
						member_rec.is_var = 0;
						add_symbol( &member_rec, TYPE_HASH_SIZE, st->type_hash );
					}
				}

				local_type_hash = temp_type_hash;
				local_sym_hash = temp_sym_hash;
				curr_addr = temp_addr;

				if ( st->tag != NULL )
				{
					if ( control_flags == PARSE_MEMBER )
						add_type( &rec, local_type_hash );
					else
						add_type( &rec, type_hash );
				}

				t = skip_wsp( t->next );

				// Structure/union declaration may come without declaring a variable
				if ( t->item->id == EOSTM )
					return	t;
			}
			else
			{
				// Variable defintion of already defined structure type or incomplete type.
				record_t	*r = NULL;

				if ( control_flags == PARSE_MEMBER )
					//r = lookup_symbol( st->tag, TYPE_HASH_SIZE, local_type_hash );
					r = lookup_type( &rec, local_type_hash );

				if ( NULL == r )
				{
					//r = lookup_symbol( st->tag, TYPE_HASH_SIZE, type_hash );
					r = lookup_type( &rec, type_hash );
				}

				if ( NULL == r )
				{
					// Error: structure not defined
				}
				rec = *r;
			}

			done = 1;
			continue;
	
			break;

		case ENUM:
			// Enumeration definition.

			if ( spec & SPEC_TYPE )
			{
				report_error( TYPE_SPEC, t->item );
				return	skip_to_term( t );
			}

			spec |= SPEC_ENUM;
			rec.type.type_id = TID_ENUM;

			en = rec.type.type_ref.type = alloc( sizeof( enum_type_t ) );
			en->enum_list = NULL;

			t = skip_wsp( t->next );

			if ( t->item->id == ID )
			{
				en->tag = alloc( strlen( t->item->token ) + 1 );
				strcpy( en->tag, t->item->token );
				t = skip_wsp( t->next );
			}
			else
				en->tag = NULL;

			if ( t->item->id == LFBRACE )
			{
				enumerator_type_t	enumer;
				enumerator_list_t *l;

				enumer.name = NULL;
				enumer.value = -1;
				l = NULL;

				t = skip_wsp( t->next );

				while ( t != NULL && t->item->id != RFBRACE )
				{
					record_t	enumer_rec;

					init_record( &enumer_rec );

					if ( t->item->id != ID )
					{
						// Report error: ID expected.
						report_error( MISSING_ID, t->item );
						t = skip_to_term( t );
					}
					else
					{
						enumer_rec.name = alloc( strlen( t->item->token ) + 1 );
						strcpy( enumer_rec.name, t->item->token );

						if ( NULL == en->enum_list )
							l = en->enum_list = alloc( sizeof( *l ) );
						else
							l = l->next = alloc( sizeof( *l ) );
						l->next = NULL;
						enumer.name = alloc( strlen( t->item->token ) + 1 );
						strcpy( enumer.name, t->item->token );

						t = skip_wsp( t-> next );
						if ( t->item->id != ASSIGN )
							++enumer.value;
						else
						{
							t = skip_wsp( t->next );
							//
							// (!!!) Will change to parse constant expression.
							//	Check that return type is integer.
							//
							if ( sscanf( t->item->token, "%d", &enumer.value ) < 1 )
							{
								// Report error parsing the constant expression.
								report_error( INIT_NOT_CONSTANT, t->item );
								t = skip_to_term( t );
							}
							t = skip_wsp( t-> next );
						}

						l->enumerator = alloc( sizeof( enumerator_type_t ) );
						*l->enumerator = enumer;

						enumer_rec.type.type_id = TID_ENUMERATOR;
						enumer_rec.type.type_ref.type = l->enumerator;
						add_type( &enumer_rec, type_hash );

						if ( t->item->id == COMMA )
						{
							t = skip_wsp( t->next );
						}

						else if ( t->item->id != RFBRACE )
						{
							// Report syntax error.
							report_error( SYNTAX_ERROR, t->item );
							t = skip_to_term( t );
						}
					}
				}

			}

			if ( en->tag != NULL )
			{
				if ( control_flags == PARSE_MEMBER )
					add_type( &rec, local_type_hash );
				else
					add_type( &rec, type_hash );
			}

			break;

		case TYPEDEF:
			spec |= SPEC_TYPEDEF;
			break;

		// const & volatile will be supported later. Meanwhile all variables are treated as
		// volatile.

		case CONST:
			spec |= SPEC_CONST;
			break;

		case VOLATILE:
			spec |= SPEC_VOLATILE;
			break;

		//	Restrict type-checking is not supported in this version. Just recognized and ignored
		case RESTRICT:
			spec |= SPEC_RESTRICT;
			break;

		case INLINE:
			spec |= SPEC_INLINE;
			break;
		}

		t = skip_wsp( t->next );
	} // while ( declaration-specifiers )

	rec.spec = spec;

	//
	//	Now parse declarators (stage 2).
	//
	for ( done = 0; !done; t = skip_wsp( t->next ) )
	{
		int	br_level;			// grouping: 0 - grouping left (toward type specifiers), 
								// 1 - right (toward declarators).
		int	pointer_done;

		br_level = 0;

		for ( pointer_done = 0; !pointer_done; )
		{
			if ( t == NULL )	
				pointer_done =1;
			else if ( t->item->id == LPAREN )
			{
				if ( br_level == 32 )
				{
					// Error: maximum nesting level for a declaration is reached
					report_error( MAX_NEST_LEVEL_EXC, t->item );
					t = skip_to_term( t );
				}
				else
					++br_level;

				t = skip_wsp( t->next );
			}
			else if ( t->item->id == RPAREN )
			{
				if ( 0 == br_level )
				{
					//report_error( BAD_RPAREN, t->item );
					//t = skip_to_term( t );
				}
				else
					--br_level;

				if ( 0 == br_level )
				{
					pointer_done = 1;
					continue;
				}
				t = skip_wsp( t->next );
			}

			else if ( t->item->id == STAR )
			{
				// Pointer.
				ptr_list_t	*p;
				unsigned	qual;

				do
				{
					t = skip_wsp( t->next );
					qual = 0;

					if ( rec.type.ptr_list == NULL )
						p = rec.type.ptr_list = alloc( sizeof( ptr_list_t ) );
					else
						p = p->next = alloc( sizeof( ptr_list_t ) );

					p->next = NULL;

					if ( t == NULL )
						break;

					if ( t != NULL )
					{
						switch ( t->item->id )
						{
						default:
							break;
						case CONST:
							qual |= SPEC_CONST;
							t = skip_wsp( t->next );
							break;
						case VOLATILE:
							qual |= SPEC_VOLATILE;
							t = skip_wsp( t->next );
							break;
						case RESTRICT:
							qual |= SPEC_RESTRICT;
							t = skip_wsp( t->next );
							break;
						}

					}
					p->qual = qual;

					if ( br_level != 0 )
						p->grouping = GROUP_RIGHT;
					else
						p->grouping = GROUP_LEFT;

				}	while ( 0 );

				if ( p->grouping == GROUP_RIGHT && t->item->id == RPAREN )
				{
					if ( br_level > 0 )
						--br_level;

					rec_src = alloc( sizeof *rec_src );
					*rec_src = rec;
					*dest = *rec_src;
					t = skip_wsp( t->next );
					goto	id_done;
				}
			}
			else
				pointer_done = 1;
		}

		if ( spec & SPEC_STRUCT || spec & SPEC_UNION || spec & SPEC_ENUM )
		{
		}

		//
		// Function declaration is allowed only on global scope.
		//
		if ( t->item->id == ID )
		{
			if ( control_flags == PARSE_GLOBAL )
			{
				memset( &rec.value, 0, sizeof( rec.value ) );				// meaningful only for scalar types.
				rec.value_valid = 1;
			}
			rec_src = alloc( sizeof *rec_src );
			*rec_src = rec;
			rec_src->name = alloc( strlen( t->item->token ) + 1 );
			strcpy( rec_src->name, t->item->token );
			*dest = *rec_src;
			t = skip_wsp( t->next );

			if ( t->item->id == RPAREN && br_level > 0 )
			{
				while ( br_level > 0 )
				{
					if ( t->item->id != RPAREN )
					{
					// Error: after ID in declarator only closing RPARENs are allowed.
						report_error( SYNTAX_ERROR, t->item );
						t = skip_to_term( t );
					}

					--br_level;
					t = skip_wsp( t->next );
				}
			}
id_done:
			if ( t->item->id == LPAREN )
			{

				// Function.
				init_record( &func_rec );
				func_rec = *rec_src;

				func_rec.type.type_id = TID_FUNCTION;
				ft = func_rec.type.type_ref.type = alloc( sizeof( function_type_t ) );
				ft->rv_type = rec;
				ft->var_prm = 0;
				ft->defined = 0;
				prm = ft->param_list = NULL;

				// Reap all right-grouping pointers from rv_type and put them to function record.
				if ( rec.type.ptr_list != NULL )
				{
					if ( rec.type.ptr_list->grouping != 0 )
					{
						func_rec.type.ptr_list = rec.type.ptr_list;
						rec.type.ptr_list = NULL;
					}
					else
					{
						for ( ptrl = rec.type.ptr_list; ptrl != NULL; ptrl1 = ptrl, ptrl = ptrl->next )
						{
							if ( ptrl->next != NULL && ptrl->next->grouping != 0 )
							{
								func_rec.type.ptr_list = ptrl;
								ptrl1->next = NULL;
							}
						}
					}
				}
		
				for ( t = skip_wsp( t->next ); !done /*t->item->id != RPAREN*/; t = skip_wsp( t->next ) )
				{
					if ( t->item->id == ELLIPSIS )
					{
						ft->var_prm = 1;
						//break;
						goto	cont;
					}

					if ( prm == NULL )
						prm = ft->param_list = alloc( sizeof( *prm ) );
					else
						prm = prm->next = alloc( sizeof( *prm ) );

					prm->next = NULL;

					t = parse_declaration( t, PARSE_ARGUMENT, &prm->rec );

					if ( t->item->id != COMMA && t->item->id != RPAREN )
					{
						report_error( MISSING_COMMA, t->item );
						t = skip_to_term( t );
						break;
					}

cont:
					done = ( t->item->id == RPAREN );
				}
				*dest = func_rec;

				// RPAREN gets skipped.
				if ( t != NULL && t->item->id == LFBRACE )
				{
					char	line_buf[ 256 ];

					// Function body definition.
					fprintf( out, "__asm\n"
									".CODE\n"
									"__endasm\n" );
					ft->defined = 1;
					curr_function = func_rec.type.type_ref.type;
					sprintf( line_buf, "_%s::\n", func_rec.name );
					fputs( line_buf, out );

					t = parse_statement( t );

					{
						ptr_list_t	*pt;
						int	var = 0;

						if ( dest->type.type_id == TID_FUNCTION )
						{
							if ( NULL != func_rec.type.ptr_list )
							{
								for ( pt = func_rec.type.ptr_list; pt != NULL; pt = pt->next )
								{
									if ( pt->grouping == 1 )
									{
										var = 1;
										break;
									}
								}
							}
						}

						if ( 0 == var )
						{
							add_symbol( dest, FUNC_HASH_SIZE, func_hash );
						}
						else
						{
							if ( control_flags & PARSE_GLOBAL )
								add_symbol( dest, SYM_HASH_SIZE, sym_hash );
							else
								add_symbol( dest, SYM_HASH_SIZE, envlist_top->hash );
						}
					}

					return	t;
				}
				else if ( !( t != NULL && t->item->id == EOSTM ) )
				{
					// Error: missing ';' or '{'
					//report_error( SYNTAX_ERROR, t->item );
					//t = skip_to_term( t );
				}
			}
			else
			{
			//	Not a function: 'void' is illegal.
				if ( rec.type.type_id == TID_VOID && rec.type.ptr_list == NULL )
				{
					// Error: 'void' is not a valid object.
					report_error( VOID_OBJECT, t->item );
					t = skip_to_term( t );
				}
			}
			if ( t != NULL && t->item->id == LSUBSCR )
			{
				int	done = 0;
				int	size;
				int	i;
				record_t	rt;

				// it's an array
				for ( dest->dim_num = 0; !done; ++dest->dim_num )
				{
					dest->dim = realloc( dest->dim, sizeof( *dest->dim ) * ( dest->dim_num + 1 ) );

					t = skip_wsp( t->next );
					current = root = alloc( sizeof( *root ) );
					init_parse_tree_node( root );
					if ( t->item->id != RSUBSCR )
					{
						++subscr_level;
						t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_INTCONST );
						calc_parse_tree( root );
						dest->dim[ dest->dim_num + 1 ] = 0;
						dest->dim[ dest->dim_num ] = *( unsigned* )root->type->value;
					}
					else
					{
						dest->dim[ dest->dim_num ] = 1;
					}

					if ( t->item == NULL || t->item->id != RSUBSCR )
					{
						// Error: RSUBSCR expected
						report_error( MISSING_RSUBSCR, t->item );
						t = skip_to_term( t );
					}

					t = skip_wsp( t->next );
					if ( t->item->id != LSUBSCR )
						done = 1;
				}
			}

			free( rec_src );
		}
		else
		{
			*dest = rec;
		}

		if ( !( dest->type.type_id == TID_FUNCTION && dest->type.ptr_list == NULL ) && dest->type.type_id != TID_ENUMERATOR && dest->name != NULL )
		{
			dest->is_var = 1;
			//dest->value = alloc( do_sizeof( dest ) );
		}

		if ( NULL == t )
		{
			// Error: COMMA or EOSTM or RPAREN or ASSIGN expected.
			report_error( SYNTAX_ERROR, t->item );
			t = skip_to_term( t );
		}

		if ( t != NULL && t->item->id == RPAREN )
		{
			if ( control_flags == PARSE_ARGUMENT )
				return	t;
			else
			{
				// Error: misplaced '}'
				//report_error( SYNTAX_ERROR, t->item );
				//t = skip_to_term( t );
			}
		}

		//
		//	Parse assignment. Apply product to *dest.
		//
		if ( t != NULL && t->item->id == ASSIGN )
		{
			t = skip_wsp( t->next );
			if ( t != NULL )
			{
				if ( t->item->id != LFBRACE && ( dest->type.type_id == TID_STRUCT ||
					dest->type.type_id == TID_UNION || dest->dim_num != 0 ) )
				{
					// Error: initializer must be a list
					report_error( INIT_NOT_LIST, t->item );
					t = skip_to_term( t );
				}
				init_fbrace_level = 0;
				t = parse_initializer( t, dest, control_flags );
			}
			else
			{
				// Error: initializer expected.
				report_error( MISSING_INIT, t->item );
				t = skip_to_term( t );
			}


			dest->value_valid = 1;
		}

		if ( t == NULL || t->item->id != EOSTM && t->item->id != COMMA )
		{

			// Error: COMMA or EOSTM expected.
			report_error_tok( MISSING_EOSTM, t->item );
			return	t;
		}
		else
		{
			if ( control_flags == PARSE_ARGUMENT /*|| control_flags == PARSE_INIT*/ )
				return	t;
			// else proceed to next declarator.
			if ( 0 == ( dest->spec & SPEC_TYPEDEF ) )
			{
				ptr_list_t	*pt;
				int	var = 1;

				if ( dest->type.type_id == TID_FUNCTION )
				{
					var = 0;

					if ( NULL != func_rec.type.ptr_list )
					{
						for ( pt = func_rec.type.ptr_list; pt != NULL; pt = pt->next )
						{
							if ( pt->grouping == 1 )
							{
								var = 1;
								break;
							}
						}
					}
				}

				// It may be a pure struct declaration.
				if ( ( dest->spec & SPEC_TYPEDEF ) == 0 && var == 1 && dest->name != NULL )
				{
//					generate_init( dest, control_flags );
					dest->addr = curr_addr;
					curr_addr += do_sizeof( dest );
				}

				if ( control_flags == PARSE_MEMBER )
				{
					if ( dest->type.type_id == TID_FUNCTION && 0 == var )
					{
						// Error: function can't be a member.
						report_error( FUNC_MEMBER, t->item );
						t = skip_to_term( t );
					}
				}
				else
				{
					if ( dest->type.type_id == TID_FUNCTION && 0 == var )
					{
						add_symbol( dest, FUNC_HASH_SIZE, func_hash );
					}
					else if ( dest->name != NULL )		// It may be a pure struct declaration.
					{
						if ( control_flags & PARSE_GLOBAL )
							add_symbol( dest, SYM_HASH_SIZE, sym_hash );
						else
							add_symbol( dest, SYM_HASH_SIZE, envlist_top->hash );
					}
				}
			}
			else
			{
				dest->is_var = 0;
				if ( control_flags == PARSE_MEMBER )
					add_symbol( dest, TYPE_HASH_SIZE, local_type_hash );
				else
					add_symbol( dest, TYPE_HASH_SIZE, type_hash );
			}

			if ( t->item->id == EOSTM )
				return	t;	// Return t pointing at EOSTM

			rec.type.ptr_list = NULL;
			rec.dim_num = 0;
			rec.dim = NULL;
		}

	} // for ( !done )
	return	t;
}
