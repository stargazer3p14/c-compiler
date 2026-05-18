/*
 *	SYMBLOS.C
 *	---------
 *
 *	Symbol tables manager for Black Phantom's C.
 */

#include	<stdlib.h>
#include	<string.h>
#include	"parser.h"

extern	int	env_nest_count;
extern	function_type_t	*curr_function;

sym_hash_t	*sym_hash[ SYM_HASH_SIZE ], *type_hash[ TYPE_HASH_SIZE ], *func_hash[ FUNC_HASH_SIZE ], *lit_hash[ LIT_HASH_SIZE ];

int	char_is_signed = 0;

//
//	Envlist is actualy a stack (LIFO) list.
//	Environment lists are addede every time that a new block is started and the first
//	variable local to it is declared.
//	The caller must add and remove environments properly and keep stack aligned.
//
hash_list_t	*envlist_top = NULL, *envlist_bot = NULL, *typelist_top = NULL, *typelist_bot = NULL;


int	hash_func( char *name, int size )
{								    
	unsigned f = 0;

	while ( *name == '_' )
		++name;

	while ( *name != 0 )
		f += *name++;

	return	f % size;
}


void	init_hash_entry( sym_hash_t *entry )
{
	entry->type.name = NULL;
	entry->type.type.type_id = -1;
	entry->next = NULL;
}


void	init_env()
{
	hash_list_t	*p;
	int	i;

	p = envlist_top;
	p->hash = alloc( SYM_HASH_SIZE * sizeof( sym_hash_t* ) );

	for ( i = 0; i < SYM_HASH_SIZE; ++i )
		p->hash[ i ] = NULL;

	++env_nest_count;
}


void	add_env()
{
	hash_list_t	*p;

	p = alloc( sizeof( *p ) );
	p->hash = NULL;
	p->next = NULL;
	p->env_size = 0;

	if ( envlist_bot == NULL )
		envlist_top = envlist_bot = p;
	else
		p->next = envlist_top;
	envlist_top = p;
}


void	add_type_env()
{
	hash_list_t	*p;
	int	i;

	p = alloc( sizeof( p ) );
	p->hash = alloc( TYPE_HASH_SIZE * sizeof( sym_hash_t* ) );

	for ( i = 0; i < TYPE_HASH_SIZE; ++i )
		p->hash[ i ] = NULL;

	if ( typelist_bot == NULL )
		typelist_top = typelist_bot = p;
	else
		p->next = typelist_top;
	typelist_top = p;
}


void	free_env()
{
	int	i;
	sym_hash_t	*p1, *p2;
	hash_list_t	*l;

	for ( i = 0; i < SYM_HASH_SIZE; ++i )
	{
		for ( p1 = envlist_top->hash[ i ]; p1 != NULL; )
		{
			p2 = p1;
			p1 = p1->next;
			if ( p2->type.name != NULL )
				free( p2->type.name );
			free( p2 );
		}
	}

	envlist_top->hash = NULL;
	--env_nest_count;
}


void	remove_env()
{
	hash_list_t	*l;

	if ( envlist_top == NULL )
		return;

	l = envlist_top;
	envlist_top = envlist_top->next;
	free( l );

	if ( envlist_top == NULL )
		envlist_bot = NULL;
}


void	remove_type_env()
{
	int	i;
	sym_hash_t	*p1, *p2;
	hash_list_t	*l;

	if ( envlist_top == NULL )
		return;

	for ( i = 0; i < TYPE_HASH_SIZE; ++i )
	{
		for ( p1 = typelist_top->hash[ i ]; p1 != NULL; )
		{
			p2 = p1;
			p1 = p1->next;
			free( p2->type.name );
			free( p2 );
		}
	}
	l = typelist_top;
	typelist_top = typelist_top->next;
	free( l );

	if ( typelist_top == NULL )
		typelist_bot = NULL;
}


/*
 *	Add a tagged record.
 */
void	add_type( record_t *type, sym_hash_t **tbl )
{
	sym_hash_t	*p;
	unsigned	i;
	struct_type_t *st;
	enum_type_t *en;
	enumerator_type_t *enumer;

	switch ( type->type.type_id )
	{
	case TID_STRUCT: case TID_UNION:
		st = type->type.type_ref.type;
		if ( st->tag == NULL )
			return;

		i = hash_func( st->tag, TYPE_HASH_SIZE );
		goto	add_type_to_table;

	case TID_ENUM:
		en = type->type.type_ref.type;
		if ( en->tag == NULL )
			return;

		i = hash_func( en->tag, TYPE_HASH_SIZE );
		goto	add_type_to_table;

	case TID_ENUMERATOR:
		enumer = type->type.type_ref.type;
		if ( enumer->name == NULL )
			return;

		i = hash_func( enumer->name, TYPE_HASH_SIZE );

add_type_to_table:
		p = tbl[ i ];

		if ( p == NULL )
			p = tbl[ i ] = alloc( sizeof( sym_hash_t ) );
		else
		{
			while( p->next != NULL )
				p = p->next;

			p = p->next = alloc( sizeof( sym_hash_t ) );
		}

		init_hash_entry( p );
		p->type = *type;
	}
}


void	add_symbol( record_t *type, int size, sym_hash_t **tbl )
{
	sym_hash_t	*p;
	unsigned	i;

	i = hash_func( type->name, size );
	p = tbl[ i ];

	if ( p == NULL )
		p = tbl[ i ] = alloc( sizeof( sym_hash_t ) );
	else
	{
		while( p->next != NULL )
			p = p->next;

		p = p->next = alloc( sizeof( sym_hash_t ) );
	}

	init_hash_entry( p );
	p->type = *type;

	if ( tbl == sym_hash && type->name[ 0 ] != '_' )
	{
		p->type.name = alloc( strlen( type->name ) + 2 );
		sprintf( p->type.name, "_%s", type->name );
	}
}


/*
 *	name2type()
 */
int	name2type( list_t *token, record_t *type )
{
	record_t	res;
	token_t	*tok;

	tok = ( token_t* )token->item;

	res.type.type_id = -1;		// Invalid
	res.dim_num = 0;			// Not an array

	switch ( tok->id )
	{
	default:
		*type = res;
		return	0;

	case CHAR:
		res.type.type_id = char_is_signed ? T_CHAR : T_UCHAR;
		token = skip_wsp( token->next );
		break;

	case SIGNED:
		token = skip_wsp( token->next );
		if ( token == NULL )
		{
			res.type.type_id = T_INT;
			break;
		}

		tok = ( token_t* )token->item;
		switch ( tok->id )
		{
		default:
			// Error: misplaced "signed" keyword
			break;
		case INT:
			res.type.type_id = T_INT;
			token = skip_wsp( token->next );
			break;
		case SHORT:
			res.type.type_id = T_SHORT;
			token = skip_wsp( token->next );
			break;
		case CHAR:
			res.type.type_id = T_CHAR;
			token = skip_wsp( token->next );
			break;
		case LONG:
			res.type.type_id = T_LONG;
			token = skip_wsp( token->next );
			break;
		}

		break;

	case UNSIGNED:
		token = skip_wsp( token->next );
		if ( token == NULL )
		{
			res.type.type_id = T_UINT;
			break;
		}

		tok = ( token_t* )token->item;
		switch ( tok->id )
		{
		default:
			res.type.type_id = T_UINT;
			break;
		case INT:
			res.type.type_id = T_UINT;
			token = skip_wsp( token->next );
			break;
		case SHORT:
			res.type.type_id = T_USHORT;
			token = skip_wsp( token->next );
			break;
		case CHAR:
			res.type.type_id = T_UCHAR;
			token = skip_wsp( token->next );
			break;
		case LONG:
			res.type.type_id = T_ULONG;
			token = skip_wsp( token->next );
			break;
		}
		break;

	case INT:
		res.type.type_id = T_INT;
		token = skip_wsp( token->next );
		break;

	case SHORT:
		res.type.type_id = T_SHORT;
		token = skip_wsp( token->next );
		break;

	case FLOAT:
		res.type.type_id = T_FLOAT;
		token = skip_wsp( token->next );
		break;

	case DOUBLE:
		res.type.type_id = T_DOUBLE;
		token = skip_wsp( token->next );
		break;

	case LONG:
		token = skip_wsp( token->next );

		if ( token != NULL && ( ( token_t* )token->item )->id == DOUBLE )
		{
			res.type.type_id = T_LONGDOUBLE;
			token = skip_wsp( token->next );
			break;
		}
		else
		{
			res.type.type_id = T_ULONG;
			break;
		}

		break;
	}

	*type = res;
	return	1;
}


record_t	*lookup_symbol( char *name, int size, sym_hash_t **tbl )
{
	sym_hash_t	*p;
	hash_list_t	*l;
	record_list_t *rl;
	unsigned	offs = 0;
	char	line[ 256 ];
	char	new_name[ 1024 ];

	if ( tbl != sym_hash )
		goto	tbl_only;

	// Look in environments (local-var tables)
	for ( l = envlist_top; l != NULL; l = l->next )
	{
		// Put here because envlist_top may be NULL
		//if ( l == envlist_top )
		if ( 0 == offs )
			offs = -l->env_size;

		// May be empty environment
		if ( l->hash == NULL )
			continue;

		for ( p = l->hash[ hash_func( name, size ) ]; p != NULL; p = p->next )
		{
			if ( p->type.name != NULL )					// it could be a tagged structure.
			{
				if ( !strncmp( name, p->type.name, strlen( name ) ) )
				{
					if ( p->type.name[ strlen( name ) ] == '\0' ||
						strncmp( p->type.name + strlen( name ), "+EBP", sizeof( "+EBP" ) - 1 ) == 0 )
					{
						strcpy( new_name, name );
						sprintf( new_name + strlen( name ), "+EBP+0%08XH", offs + p->type.addr );
						p->type.name = realloc( p->type.name, strlen( new_name ) + 1 );
						strcpy( p->type.name, new_name );
						return	&p->type;
					}
				}
			}
		}

		offs += l->env_size + 4;
	}

	if ( NULL == curr_function )
		goto	tbl_only;
#if 1
	// Look in function's formal parameters
	for ( offs = 8, rl = curr_function->param_list; rl != NULL; rl = rl->next )
	{
		if ( rl->rec.type.type_id != -1 )
		{
			if ( !strncmp( name, rl->rec.name, strlen( name ) ) )
			{
				if ( rl->rec.name[ strlen( name ) ] == '\0' ||
					strncmp( rl->rec.name + strlen( name ), "+EBP", sizeof( "+EBP" ) - 1 ) == 0 )
				{
					strcpy( new_name, name );
					sprintf( new_name + strlen( name ), "+EBP+0%08XH", offs );
					rl->rec.name = realloc( rl->rec.name, strlen( new_name ) + 1 );
					strcpy( rl->rec.name, new_name );
					return	&rl->rec;
				}
			}

			offs += do_sizeof( &rl->rec );
		}
	}
#endif

tbl_only:
	// Look in static table
	for ( p = tbl[ hash_func( name, size ) ]; p != NULL; p = p->next )
	{
		if ( tbl == sym_hash && name[ 0 ] != '_' )
			sprintf( line, "_%s", name );
		else
			strcpy( line, name );
		if ( p->type.name != NULL )					// it could be a tagged structure.
		{
			if ( !strcmp( line, p->type.name ) )
			{
//				if ( tbl == sym_hash && name[ 0 ] != '_' )
//				{
//					name = realloc( name, strlen( line ) + 1 );
//					strcpy( name, line );
//				}

				return	&p->type;
			}
		}
	}

	return	NULL;
}


/*
 *	Lookup tagged type - struct, union or enum.
 */
record_t	*lookup_type( record_t *type_rec, sym_hash_t **type_hash )
{
	sym_hash_t	*p;
	struct_type_t	*st1, *st2;
	type_t *dest;
	enum_type_t	*en1, *en2;

	dest = &type_rec->type;

	switch ( dest->type_id )
	{
	case TID_STRUCT: case TID_UNION:
		st2 = dest->type_ref.type;
		if ( st2->tag == NULL )
			return	NULL;

		for ( p = type_hash[ hash_func( st2->tag, TYPE_HASH_SIZE ) ]; p != NULL; p = p->next )
		{
			st1 = p->type.type.type_ref.type;

			if ( p->type.type.type_id == dest->type_id )
			{

				if ( st1 == st2 )
					return	&p->type;

				if ( st1 != NULL && st2 != NULL )
				{
					if ( st1->tag == st2->tag )
						return	&p->type;
					if ( st1->tag != NULL && st2->tag != NULL && !strcmp( st1->tag, st2->tag ) )
						return	&p->type;
				}
			}
		}
		return	NULL;

	case TID_ENUM:
		en2 = dest->type_ref.type;
		if ( en2->tag == NULL )
			return	NULL;

		for ( p = type_hash[ hash_func( en2->tag, TYPE_HASH_SIZE ) ]; p != NULL; p = p->next )
		{
			en1 = p->type.type.type_ref.type;

			if ( p->type.type.type_id == dest->type_id )
			{

				if ( en1 == en2 )
					return	&p->type;

				if ( en1 != NULL && en2 != NULL )
				{
					if ( en1->tag == en2->tag )
						return	&p->type;
					if ( en1->tag != NULL && en2->tag != NULL && !strcmp( en1->tag, en2->tag ) )
						return	&p->type;
				}
			}
		}
		break;
	}

	return	NULL;
}


record_t	*lookup_var( char *name )
{
	return	lookup_symbol( name, SYM_HASH_SIZE, sym_hash );
}


record_t	*lookup_typedef( char *name )
{
	return	lookup_symbol( name, TYPE_HASH_SIZE, type_hash );
}


int	compat_types( record_t *rec1, record_t *rec2 )
{
	switch ( ( ( rec1->type.ptr_list != NULL ) << 1 ) +	( rec2->type.ptr_list != NULL ) )
	{
	case 0:
		// Both aren't pointers
		if ( rec1->type.type_id != rec2->type.type_id && 
			rec1->type.type_id != TID_BASIC && rec1->type.type_id != TID_NUMCONST && 
			rec2->type.type_id != TID_BASIC && rec2->type.type_id != TID_NUMCONST )
			return	TYPES_INCOMPAT;
		else 
			return	TYPES_COMPAT;	// Can be converted implicitly

	case 1:
		// rec2 is a pointer
		if (  (rec1->type.type_id == TID_BASIC || rec1->type.type_id == TID_NUMCONST ) && 
			rec1->type.type_ref.id < T_FLOAT )
				
				return	TYPES_REQ_CAST;		// Must be explicitly cast

		return	TYPES_INCOMPAT;

	case 2:
		// rec1 is a pointer
		if ( ( rec2->type.type_id == TID_BASIC || rec2->type.type_id == TID_NUMCONST ) && 
			rec2->type.type_ref.id < T_FLOAT )
				return	TYPES_REQ_CAST;		// Must be explicitly cast

		return	TYPES_INCOMPAT;

	case 3:
		// Both are pointer
		if ( rec1->type.type_id != rec2->type.type_id )
			return	TYPES_REQ_CAST;
		else if ( rec1->type.type_id == TID_BASIC )
		{
			if ( rec1->type.type_ref.id == rec2->type.type_ref.id )
				return	TYPES_COMPAT;
		}
		else
		{
			// (Later) check for full indentity of members.
			return	TYPES_REQ_CAST;
		}
	default:
		//	Something screwed up
		return	TYPES_INCOMPAT;
	}
}


