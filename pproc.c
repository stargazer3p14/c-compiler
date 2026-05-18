/*
 *	PPROC.C
 *	-------
 *
 *	Source file for the preprocessor.
 *
 *	The preprocessor passes the source file several times, performing the following phases
 *	described in 5.1.1.2, 1-6:
 *
 *	1) Replace trigraph sequences.
 *	2) Remove all backslashes, immediately followed by new-line
 *
 *	Macros with variable arguments are not supported (?)
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"

extern	char **inc_dir;
extern	int	inc_dirs;

extern	char **def_sym;
extern	int	def_syms;

// Flag to process initial definitions
int	first_time = 1;

macro_t	*macro_hash[ MACRO_HASH_SIZE ];

static __inline	void	init_macro( macro_t *src )
{
	src->name = NULL;
	src->prm_list = alloc( sizeof( *src->prm_list ) );
	*src->prm_list = NULL;
	src->text = NULL;
	src->next = NULL;
}


void	destroy_macro( macro_t *victim )
{
	char	**p;

	free( victim->name );

	for( p = victim->prm_list; *p != NULL; ++p )
		free( *p );
	free( victim->text );
}


/*
 *	Assumption: source object is alloc()ed.
 */
void	add_macro( macro_t *src )
{
	int	ind;

	ind = hash_func( src->name, MACRO_HASH_SIZE );
	src->next = macro_hash[ ind ];
	macro_hash[ ind ] = src;
}


/*
 *	macro is #undefed.
 */
void	remove_macro( char *name )
{
	macro_t	*p, *p1 = NULL;

	int	ind;
	ind = hash_func( name, MACRO_HASH_SIZE );

	for ( p = macro_hash[ ind ]; p != NULL; p = p->next, p1 = ( p1 == NULL ? macro_hash[ ind ] : p1->next ) )
	{

		if ( !strcmp( p->name, name ) )
		{
			if ( p == macro_hash[ ind ] )
				macro_hash[ ind ] = p->next;
			else
				p1->next = p->next;
			destroy_macro( p );
		}
	}
}


macro_t	*lookup_macro( char *name )
{
	macro_t	*p;

	for ( p = macro_hash[ hash_func( name, MACRO_HASH_SIZE ) ]; p != NULL; p = p->next )
		if ( !strcmp( p->name, name ) )
			return	p;

	return	p;
}


int	is_macro( const char *s )
{
	macro_t *m;
	char	line[ 4096 ];
	int	i;
	char	**p;
	int	count;

	for ( i = 0; is_alnum( *s ); ++s, ++i )
		line[ i ] = *s;
	line[ i ] = '\0';

	m = lookup_macro( line );
	if ( NULL == m )
		return	0;

	if ( *m->prm_list != NULL )
	{
		while ( isspace( *s ) )
			++s;
		if ( *s != '(' )
			return	0;

		++s;

		for( p = m->prm_list; *p != NULL; ++p )
		{
			while ( isspace( *s ) )
				++s;

			if ( !is_alnum( *s ) )
				return	0;

			while ( is_alnum( *s ) )
				++s;

			if ( *s == ')' )
			{
				if ( *( p + 1 ) )
					return	0;
				continue;
			}

			if ( *s != ',' )
				return	0;

			++s;
		}

		if ( *s != ')' )
			return	0;

		return	1;
	}
	else
	{
		while ( isspace( *s ) )
			++s;

		return	1;
	}
}


/*
 *	src points to the macro
 */
char	**prep_macro( macro_t *macro, const char *src, int definition )
{
	char **l;
	char **p;
	int	i = 0;
	char	line[ 4096 ];
	int	j;

	p = alloc( sizeof( *p ) );
	*p = NULL;

	// Copy macro's name
	for ( i = 0; is_alnum( src[ i ] ); ++i )
		line[ i ] = src[ i ];
	line[ i ] = '\0';
	src += i;
	macro->name = alloc( i + 1 );
	strcpy( macro->name, line );
	
	if ( *src != '(' )
		goto	parse_def;

	++src;			// Skip lparen

	// Parse and store parameters
	for ( i = 0, l = macro->prm_list; *src != ')'; ++i )
	{
		while ( isspace( *src ) )
			++src;

		for ( j = 0; is_alnum( *src ); ++j, ++src )
			line[ j ] = *src;
		line[ j ] = '\0';

		if ( line[ 0 ] != '\0' )
		{
			if ( !definition && NULL == l )
			{
				// Error: extra parameters in macro invokation
			}

			*( p + i ) = alloc( strlen( line ) + 1 );
			strcpy( *( p + i ), line );
			p = realloc( p, sizeof( *p ) * ( i + 2 ) );
			*( p + i + 1 ) = NULL;
		}

		while ( isspace( *src ) )
			++src;

		if ( *src == ',' )
			++src;						// Skip comma

		if ( *l != NULL )
			++l;
	}

	if ( *src == ')' )
		++src;

parse_def:
	if ( definition )
	{
		for ( i = 0; src[ i ] != '\n'; ++i )
			line[ i ] = src[ i ];
		line[ i ] = '\0';
		macro->text = alloc( i + 1 );
		strcpy( macro->text, line );
		macro->prm_list = p;
	}

	return	p;
}


/*
 *	Stores macro replacement with arg_list to dest
 *
 *	arg_list must be already checked to match the number of arguments for macro
 */
void	replace_macro( macro_t *macro, char **arg_list, char *dest )
{
	char **s, **d;
	char	arg[ MAX_TOKEN_SIZE ];
	char	*t;
	int	i;
	int	pounded = 0;
	macro_t	*m;
	char	*temp;
	char	**a;
	int	replaced;

	dest[ 0 ] = '\0';
	for ( t = macro->text; *t != 0; pounded = 0 )
	{
		replaced = 0;

		while ( is_wspace( *t ) )
			++t;

		if ( isalunder( *t ) )
		{
			temp = t;

			for ( i = 0; is_alnum( *t ); ++i )
				arg[ i ] = *t++;
			arg[ i ] = '\0';

			if ( pounded )
				strcat( dest, "#" );

			if ( arg_list != NULL )
			{
				for ( d = arg_list, s = macro->prm_list; *s != NULL; ++s, ++d )
				{
					if ( !strcmp( *s, arg ) )
					{
						if ( pounded )
							sprintf( dest + strlen( dest ), "\"%s\"", arg );
						else
							strcat( dest, *d );

						replaced = 1;
						break;
					}
				}
			}

			if ( !replaced )
				strcat( dest, arg );
		}
		else if ( isnum( *t ) )
		{
			for ( i = 0; is_alnum( *t ); ++t, ++i )
				arg[ i ] = *t;
			arg[ i ] = '\0';
			strcat( dest, arg );
		}
		else if ( *t == '#' )
		{
			if ( *( t + 1 ) == '#' )
				t += 2;					//		'##' is simply discarded
			else
				pounded = 1;
		}
		else
		{
			sprintf( dest + strlen( dest ), "%c", *t );
			++t;
		}
	}
}


static	__inline	void	replace_trigraph( FILE *in, FILE *out )
{
	char	line[ MAX_MACRO_SIZE ], *s;

	while ( !feof( in ) )
	{
		fgets( line, MAX_MACRO_SIZE - 1, in );
		for ( s = strchr( line, '?' ); s != NULL; s = strchr( s + 2, '?' ) )
		{
			if ( s[ 1 ] == '?' )
			{
				switch( s[ 2 ] )
				{
				default:
					break;
				case '=':
					s[ 0 ] = '#';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '(':
					s[ 0 ] = '[';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '/':
					s[ 0 ] = '\\';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '\'':
					s[ 0 ] = '^';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '<':
					s[ 0 ] = '{';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '!':
					s[ 0 ] = '|';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '>':
					s[ 0 ] = '}';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				case '-':
					s[ 0 ] = '~';
					memmove( s, s + 3, strlen( s + 3 ) + 1 );
					break;
				}
			}
		}
		fputs( line, out );
	}
}


static __inline	void	replace_backslash( FILE *in, FILE *out )
{
	char	line[ MAX_MACRO_SIZE ];

	while ( !feof( in ) )
	{
		fgets( line, MAX_MACRO_SIZE - 1, in );

		if ( line[ 0 ] != '\0' && line[ strlen( line ) - 2 ] == '\\' )
		{
			line[ strlen( line ) - 2 ] = '\0';
			fprintf( out, "%s", line );
		}
		else
		{
			fputs( line, out );
		}
	}
}


extern	char	*glob_src, *src;


/*
 *	Input:	source line points to constant expression, until end-of-line
 *
 *	Output: result of the evaluation.
 */
int	eval_condition( char *s )
{
	list_t	*hd = NULL, *tl = NULL;
	token_t	*tok;
	parse_tree_t *t;
	record_t	rt;
	char	*s1 = s;
	char	**p;
	macro_t	*m;

	// Source file must end at least with a newline
	while ( *s != '\0' )
	{
		src = s;
		tok = get_token( FL_DEFINED_KEYWD );

		if ( hd == NULL )
			hd = tl = alloc( sizeof( *hd ) );
		else
			tl = tl->next = alloc( sizeof( *tl ) );
		tl->item = tok;
		tl->next = NULL;

		s += strlen( tok->token );
	}

	t = alloc( sizeof( *t ) );
	init_parse_tree_node( t );
	parse_expression( t, NULL, hd, &rt, DFA_SEL_LEFT, PARSE_PPROC_CONDITION );
	assign_temp_types( t );
	assign_temp_names( t );
	calc_parse_tree( t );

	switch( t->type->type.type_ref.id )
	{
	default:
		// Some error
		return	0;
	case T_CHAR:
		return *( char* )t->type->value != 0;
	case T_UCHAR:
		return *( unsigned char* )t->type->value != 0;
	case T_SHORT:
		return *( short* )t->type->value != 0;
	case T_USHORT:
		return *( unsigned short* )t->type->value != 0;
	case T_INT:
		return *( int* )t->type->value != 0;
	case T_UINT:
		return *( unsigned int* )t->type->value != 0;
	case T_LONG:
		return *( long* )t->type->value != 0;
	case T_ULONG:
		return *( unsigned long* )t->type->value != 0;
	case T_FLOAT:
		return *( float* )t->type->value != 0;
	case T_DOUBLE:
		return *( double* )t->type->value != 0;
	case T_LONGDOUBLE:
		return *( long double* )t->type->value != 0;
	}
}


static	__inline void	process_line( const char *src, char *dest, int cond )
{
	int	i;
	char	macro_buf[ 256 ], macro_repl_text[ MAX_MACRO_SIZE ];
	char	**p;
	macro_t	*m;
	token_t	*tok;
	int	saw_defined = 0;

 	dest[ 0 ] = '\0';
 	while ( *src != '\0' )
 	{
 		if ( *src == '\"' )
 		{
 			i = strlen( dest );
 			do
 			{
 				if ( *src == '\\' && *( src + 1 ) == '\"' )
 				{
 					dest[ i++ ] = *src++;
 					dest[ i++ ] = *src++;
 					continue;
 				}
 				dest[ i++ ] = *src;
 				++src;
 			}
 			while ( *src != '\0' && *src != '\"' );
 			dest[ i++ ] = *src++;
 			dest[ i ] = '\0';

 		}
 		else if ( *src == '\'' )
 		{
 			i = strlen( dest );
 			do
 			{
 				if ( *src == '\\' && *( src + 1 ) == '\'' )
 				{
 					dest[ i++ ] = *src++;
 					dest[ i++ ] = *src++;
 					continue;
 				}
 				dest[ i++ ] = *src;
 				++src;
 			}
 			while ( *src != '\0' && *src != '\'' );
 			dest[ i++ ] = *src++;
 			dest[ i ] = '\0';
 		}			
 		else if ( *src == '/' && *( src + 1 ) == '/' )
 		{
 			i = strlen( dest );
 			while ( *src != '\0' && *src != '\n' )
			{
				dest[ i++ ] = *src;
 				++src;
			}
			dest[ i++ ] = *src;
 			dest[ i ] = '\0';
 		}
 		else if ( *src == '/*' )
 		{
 			i = strlen( dest );
 			while ( *src != '\0' && ( *src != '*' || *( src + 1 ) != '/' ) )
			{
				dest[ i++ ] = *src;
 				++src;
			}
			dest[ i++ ] = *src++;
			dest[ i++ ] = *src++;
 			dest[ i ] = '\0';
 		}
 		else if ( is_macro( src ) && !saw_defined )
 		{
 			for ( i = 0; is_alnum( src[ i ] ); ++i )
 				macro_buf[ i ] = src[ i ];
 			macro_buf[ i ] = '\0';
 
			m = lookup_macro( macro_buf );
			p = prep_macro( m, src, 0 );
			replace_macro( m, p, macro_repl_text );

			//	It may be necessary to process the line again, due to macro expansion
			strcpy( macro_buf, macro_repl_text );
			process_line( macro_buf, macro_repl_text, cond );

			strcat( dest, macro_repl_text );
			src += strlen( m->name );

			//	Skip parentheses for macros with params.
			if ( p != NULL && p[ 0 ] != NULL )
			{
				i = 0;
				do
				{
					tok = get_token( FL_DEFINED_KEYWD );
					if ( tok->id == LPAREN )
						++i;
					else if ( tok->id == RPAREN )
						--i;

					src += strlen( tok->token );
					free( tok );
				} while ( i != 0 );
			}
		}
		else if ( isalunder( *src ) )
		{
			//	'defined' keyword and the immediately following identifier remain.
			if ( cond && strncmp( src, "defined", sizeof( "defined" ) - 1 ) == 0 && !is_alnum( src[ sizeof( "defined" ) - 1 ] ) )
			{
				strcat( dest, "defined" );
				src += sizeof( "defined" ) - 1;
				saw_defined = 1;
			}
			else if ( !cond || saw_defined )
			{
				while ( is_alnum( *src ) )
				{
					sprintf( dest + strlen( dest ), "%c", *src );
					++src;
				}

				if ( saw_defined )
					saw_defined = 0;
			}
			else if ( cond )
			{
				//	Non-macro identifiers are converted to pp-number 0 (6.10.1-3).
				sprintf( dest + strlen( dest ), "0" );
				while ( is_alnum( *src ) )
					++src;
			}
		}
		else if ( isnum( *src ) )
		{
			while ( isalnum( *src ) )
			{
				sprintf( dest + strlen( dest ), "%c", *src );
				++src;
			}
		}
		else if ( isspace( *src ) )
		{
			while ( isspace( *src ) )
			{
				sprintf( dest + strlen( dest ), "%c", *src );
				++src;
			}
		}
		else
		{
			// Not a separately-processed characters
			sprintf( dest + strlen( dest ), "%c", *src );
			++src;
		}
	}
}


int	if_level = 0;
int	temp_num = 0;

int		skipping = 0;
int		prev_skipping = 0;
//int		skipping_level = 0;
int		else_seen = 0;


static void	process_directives( FILE *in, FILE *out )
{
	char	line[ MAX_MACRO_SIZE ], dest_line[ MAX_MACRO_SIZE ], macro_buf[ 256 ], macro_repl_text[ MAX_MACRO_SIZE ];
	char	*s, *t;
	char	directive[ 256 ];
	int		i, j;

	int		temp_skipping = 0;
	int		temp_else_seen = 0;
	int		this_skipping = 0;

	char	inc_name[ 256 ];
	char	fname[ 256 ];
	char	dest_name[ 256 ];
	FILE	*inc;
	char	**p;
	macro_t	*m;

	j = 0;
	temp_skipping = skipping;

	while ( !feof( in ) )
	{
		if ( first_time )
		{
			if ( j == def_syms )
				first_time = 0;
			else
				strcpy( line, def_sym[ j++ ] );
		}

		else
			fgets( line, MAX_MACRO_SIZE - 1, in );

		for ( s = line; is_wspace( *s ); ++s )
			;

		if ( *s != '#' )
		{
			if ( !skipping )
			{
				process_line( s, dest_line, 0 );
				fputs( dest_line, out );
			}
			else
			{
				fprintf( out, "// %s", line );
			}
			continue;
		}

		// it's a preprocessing directive

		fprintf( out, "// %s", s );

		for ( ++s; is_wspace( *s ); ++s )
			;

		for ( i = 0; isalpha( s[ i ] ); ++i )
			directive[ i ] = s[ i ];

		directive[ i ] = '\0';
		s += i;

		if ( !strcmp( directive, "if" ) )
		{
			if ( !skipping )
			{
				++if_level;

				while ( isspace( *s ) )
					++s;

				process_line( s, dest_line, 1 );
				s = dest_line;

				temp_skipping = prev_skipping;
				prev_skipping = skipping;

				if ( eval_condition( s ) == 0 )
				{
					skipping = 1;
					//++skipping_level;
				}
				else
				{
					skipping = 0;
				}

				process_directives( in, out );
				skipping = prev_skipping;
				prev_skipping = temp_skipping;
			}
			else
			{
				process_directives( in, out );
			}
		}
		else if ( !strcmp( directive, "ifdef" ) )
		{
			if ( !skipping )
			{
				++if_level;

				while ( isspace( *s ) )
					++s;

				temp_skipping = prev_skipping;
				prev_skipping = skipping;

				if ( !is_macro( s ) )
				{
					skipping = 1;
					//++skipping_level;
				}
				else
				{
					skipping = 0;
				}

				process_directives( in, out );
				skipping = prev_skipping;
				prev_skipping = temp_skipping;
			}
			else
			{
				process_directives( in, out );
			}
		}
		else if ( !strcmp( directive, "ifndef" ) )
		{
			if ( !skipping )
			{
				++if_level;

				temp_skipping = prev_skipping;
				prev_skipping = skipping;

				while ( isspace( *s ) )
					++s;

				if ( is_macro( s ) )
				{
					skipping = 1;
					//++skipping_level;
				}
				else
				{
					skipping = 0;
				}

				process_directives( in, out );
				skipping = prev_skipping;
				prev_skipping = temp_skipping;
			}
			else
			{
				process_directives( in, out );
			}
		}
		else if ( !strcmp( directive, "elif" ) && !prev_skipping )
		{
			if ( if_level == 0 )
			{
				// Error: misplaced #else
			}
			if ( else_seen != 0 )
			{
				// Error: #else already encountered
			}

			while ( isspace( *s ) )
				++s;

			process_line( s, dest_line, 1 );
			s = dest_line;

			if ( eval_condition( s ) == 0 )
			{
				skipping = 1;
			}
			else
			{
				skipping = 0;
			}
		}
		else if ( !strcmp( directive, "else" ) && !prev_skipping )
		{
			if ( if_level == 0 )
			{
				// Error: misplaced #else
			}
			if ( else_seen != 0 )
			{
				// Error: #else already encountered
			}
			skipping = !skipping;
			else_seen = 1;
		}
		else if ( !strcmp( directive, "endif" ) )
		{
			if ( if_level == 0 )
			{
				// Error: misplaced #endif
			}

			--if_level;

			return;
		}
		else if ( !strcmp( directive, "define" ) && !skipping )
		{
			macro_t	*m;

			while ( isspace( *s ) )
				++s;

			m = alloc( sizeof( *m ) );
			init_macro( m );
			prep_macro( m, s, 1 );
			add_macro( m );
		}
		else if ( !strcmp( directive, "include" ) && !skipping )
		{
			while ( isspace( *s ) )
				++s;

			if ( *s == '"' )
			{
				// Look first in current directory
				for ( i = 0, ++s; *s != '"'; ++s, ++i )
				{
					if ( *s == '\n' )
					{
						// Error: missing '"'
					}
					inc_name[ i ] = *s;
				}
				inc_name[ i ] = '\0';

				sprintf( fname, ".\\%s", inc_name );
				inc = fopen( fname, "rt" );
				if ( inc != NULL )
				{
					fclose( inc );
					goto	include_file;
				}
			}
			else
			{
				if ( *s != '<' )
				{
					// Error: #include must be followed by '"' or '<'
				}

				for ( i = 0, ++s; *s != '>'; ++s, ++i )
				{
					if ( *s == '\n' )
					{
						// Error: missing '"'
					}
					inc_name[ i ] = *s;
				}
				inc_name[ i ] = '\0';
			}

			for ( p = inc_dir; p - inc_dir < inc_dirs; ++p )
			{
				sprintf( fname, "%s\\%s", *p, inc_name );
				inc = fopen( fname, "rt" );
				if ( inc != NULL )
				{
					fclose( inc );
					goto	include_file;
				}
			}

			// Error (?): #included file not found
include_file:
			sprintf( dest_name, "%s.temp.%d", inc_name, temp_num++ );
			preprocess( fname, dest_name );
			inc = fopen( dest_name, "rt" );
			fprintf( out, "// Including %s\n", dest_name );
			while ( !feof( inc ) )
			{
				if ( fgets( dest_line, MAX_MACRO_SIZE - 1, inc ) != NULL )
					fputs( dest_line, out );
			}
			fprintf( out, "// End of %s\n", dest_name );
			fclose( inc );
		}
		else if ( !strcmp( directive, "error" ) && !skipping )
		{
			char	line[ 1024 ];

			while ( isspace( *s ) )
				++s;

			fprintf( stderr, "#error: %s\n", s );
			exit( 0 );
		}
		else
		{
			//	Unknown preprocessing directives are simply ignored.
			fprintf( out, "// %s", line );
		}
	}
}


void	preprocess( char *src, char *dest )
{
	FILE *fsrc, *fdest, *ftempsrc, *ftempdest;
	char	temp_src[ 256 ], temp_dest[ 256 ];
	char	buf[ 4096 ];
	long	len;
	int	phase = 1;

	// Phase 1 - replace trigraph sequences
	sprintf( temp_src, "%s", src );
	sprintf( temp_dest, "%s.%d", dest, phase );
	ftempsrc = fopen( temp_src, "rt" );
	ftempdest = fopen( temp_dest, "wt" );
	replace_trigraph( ftempsrc, ftempdest );
	fclose( ftempsrc );
	fclose( ftempdest );

	// Phase 2 - replace '\\' followed by '\n', concatenating lines.
	sprintf( temp_src, "%s.%d", dest, phase );
	++phase;
	sprintf( temp_dest, "%s.%d", dest, phase );
	ftempsrc = fopen( temp_src, "rt" );
	ftempdest = fopen( temp_dest, "wt" );
	replace_backslash( ftempsrc, ftempdest );
	fclose( ftempsrc );
	fclose( ftempdest );

	// Phase 3 - process preprocessing directives.
	sprintf( temp_src, "%s.%d", dest, phase );
	++phase;
	sprintf( temp_dest, "%s.%d", dest, phase );
	ftempsrc = fopen( temp_src, "rt" );
	ftempdest = fopen( temp_dest, "wt" );
	skipping = 0;
	else_seen = 0;
	process_directives( ftempsrc, ftempdest );
	fclose( ftempsrc );
	fclose( ftempdest );

	fsrc = fopen( temp_dest, "rt" );
	fdest = fopen( dest, "wt" );
	do
	{
		len = fread( buf, 1, 4096, fsrc );
		fwrite( buf, 1, len, fdest );
	}	while ( len == 4096 );
	fclose( fsrc );
	fclose( fdest );
	//rename( temp_dest, dest );
}
