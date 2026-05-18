/*
 *	STATEMENTS.C
 *	------------
 *
 *	Source for parsing statements.
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"


extern	parse_tree_t	*root, *current;
extern	sym_hash_t	*sym_hash[ SYM_HASH_SIZE ], *type_hash[ TYPE_HASH_SIZE ], *func_hash[ FUNC_HASH_SIZE ];
extern	unsigned	def_types;
extern	FILE	*out;
extern	char	line_buf[ 1024 ];
extern	int	temp_level_fp, temp_level_int;
extern	int	brace_level, fbrace_level, subscr_level;
extern	int	temp_level, label_count, var_count;
extern	function_type_t *curr_function;
extern	hash_list_t	*envlist_top, *envlist_bot, *typelist_top, *typelist_bot;
extern	unsigned	curr_addr;
extern	FILE	*src, *dest;

 
int	in_construct;
int	break_label_count, cont_label_count;

int	env_nest_count;
int	switch_count = 0;

extern	int		glob_error;				// Error symbol - used for recovery from nested recursions.

/*
 *	Rerturns t pointing to the next token after '{'
 */
static	__inline	list_t	*parse_lfbrace( list_t *t )
{
	record_t	rt;

	++fbrace_level;
	t = skip_wsp( t->next );
   	add_env();
   	envlist_top->env_size = 0;

   	//	May have own environment
   	if ( is_declspec( t->item->id ) )
   	{
   		unsigned	env_size = 0, tmp;

   		init_env();
   		curr_addr = 0;
   		while ( is_declspec( t->item->id ) )
   		{
   			t = parse_declaration( t, PARSE_LOCAL, &rt );
   			t = skip_wsp( t->next );
   			//tmp = do_sizeof( &rt );
   			//if ( ( tmp & 3 ) != 0 )
   			//	tmp = ( tmp & ~3 ) + 4;
   			//env_size += tmp;
   		}
   		env_size = curr_addr;
   		if ( ( env_size & 3 ) != 0 )
   			env_size = ( env_size & ~3 ) + 4;

   		envlist_top->env_size = env_size;

   		fprintf( out, "__asm\n" );
   		fprintf( out, "\tPUSH\tEBP\n" );
   		fprintf( out, "\tMOV\tEBP, ESP\n" );
   		fprintf( out, "\tSUB\tESP, 0%08XH\n", envlist_top->env_size );
   		fprintf( out, "__endasm\n" );

   		dest = out;
   		fprintf( out, "__asm\n" );
   		generate_init( envlist_top->hash, PARSE_LOCAL );
   		fprintf( out, "__endasm\n" );
   	}

	return	t;
}


list_t	*parse_statement( list_t *t )
{
#if 1
// Later

	list_t *tl2, *tl1;
	int	if_label_count, while_label_count, for_label_count, do_label_count, switch_label_count, case_label_count = -1;
	record_t	rt;
	parse_tree_t	*for_root, *switch_root;
	int	dflt_used = 0;
	int	done;
	int	i;
	int	n_cases = 0;
	int	temp_in_construct;
	unsigned *cases = NULL, *cases_label = NULL;

	if ( NULL == t )
		return	t;

	switch ( t->item->id )
	{
	case ASM:
		do
		{
			fprintf( out, t->item->token );
			t = t->next;
		} while ( t->item->id != ENDASM );
		fprintf( out, t->item->token );
		t = skip_wsp( t->next );
		break;
   	case ID:
   		tl1 = skip_wsp( t->next );

		if ( NULL == tl1 )
		{
			// Error: unexpected EOF
			report_error_tok( BAD_EOF, NULL );
			return	NULL;
		}
   		if ( tl1->item->id == COLON )
   		{
			sprintf( line_buf, "%s:\n", t->item->token );
			fputs( line_buf, out );
   			t = skip_wsp( t->next );

			if ( NULL == t )
				return	t;

   			// Labeled statement (6.6.1)
			return	parse_statement( t );
		}
		else
		{
			// Expression statement
			root = alloc( sizeof( *root ) );
			init_parse_tree_node( root );

			glob_error = 0;

			t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_NORMAL );

			if ( glob_error )
			{
				while ( t != NULL && t->item->id != EOSTM )
					t = t->next;

				glob_error = 0;
				return	t;
			}

			temp_level_int = temp_level_fp = 0;
			assign_temp_types( root );
			assign_temp_names( root );
			calc_parse_tree( root );
			print_parse_tree( root, out );

			// Sequence point (6.8.3)
			commit_seqpoint( out );
			return	t;
   		}
		break;			// Reached?

   	case LFBRACE:
   		;	// Compound statement (6.6.2)
		t = parse_lfbrace( t );
		do
		{
	   		t = parse_statement( t );

			if ( NULL == t )
				return	t;
			if ( t->item->id == EOSTM && NULL == ( t = skip_wsp( t->next ) ) )
			{
				// Error: missing '}'
				report_error_tok( MISSING_RFBRACE, t->item );
				return	t;
			}

		} while ( t->item->id != RFBRACE );
		goto	process_rfbrace;

   	case EOSTM:
   		;	// Empty statement (6.6.2)
		return	t;
   		// break;

   	case IF:
   		// if() selection statement (6.6.4.1)
		t = skip_wsp( t->next );
		if ( NULL == t || t->item->id != LPAREN )
		{
			// Error: missing '('
			report_error_tok( MISSING_LPAREN, t->item );
			t = skip_to_term( t );
			return	t;
		}
		if_label_count = label_count;
		++label_count;
		++brace_level;
		root = alloc( sizeof( *root ) );
		init_parse_tree_node( root );
		t = skip_wsp( t->next );
		t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_NORMAL );
		temp_level_int = temp_level_fp = 0;
		assign_temp_types( root );
		assign_temp_names( root );
		calc_parse_tree( root );
		print_parse_tree( root, out );

		// Sequence point (6.8.4)
		commit_seqpoint( out );

		if ( NULL == t || t->item->id != RPAREN )
		{
			// Error: missing ')'
			report_error_tok( MISSING_RPAREN, t->item );
			t = skip_to_term( t );
			return	t;
		}
		t = skip_wsp( t->next );

		sprintf( line_buf, "if ( !%s [ %s%d ] ) goto __label%d\n", root->token, is_float( root->type ) ? "FP" : "INT", do_sizeof( root->type ), if_label_count );
		fputs( line_buf, out );
		t = parse_statement( t );

		if ( NULL == t )
			break;

		if ( t->item->id == EOSTM )
			t = skip_wsp( t->next );
		if ( t != NULL && t->item->id == ELSE )
		{
			sprintf( line_buf, "goto __label%d\n", label_count );
			fputs( line_buf, out );
			sprintf( line_buf, "__label%d:\n", if_label_count );
			fputs( line_buf, out );
			if_label_count = label_count;
			++label_count;
			t = parse_statement( skip_wsp( t->next ) );
			sprintf( line_buf, "__label%d:\n", if_label_count );
			fputs( line_buf, out );
		}
		else
		{
			sprintf( line_buf, "__label%d:\n", if_label_count );
			fputs( line_buf, out );
		}
   		break;

   	case SWITCH:
		t = skip_wsp( t->next );
		if ( NULL == t || t->item->id != LPAREN )
			// Error: missing '('
			report_error_tok( MISSING_LPAREN, t->item );
		else
			t = parse_lfbrace( t );

		fputs( "__switch\n", out );
		switch_root = alloc( sizeof( *root ) );
		init_parse_tree_node( switch_root );
		t = parse_expression( switch_root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_NORMAL );

		assign_temp_types( switch_root );
		assign_temp_names( switch_root );
		calc_parse_tree( switch_root );
		print_parse_tree( switch_root, out );

		// Sequence point (6.8.4)
		commit_seqpoint( out );

		if ( t->item->id != RPAREN )
			report_error_tok( MISSING_RPAREN, t->item );
		else
			t = skip_wsp( t->next );

		if ( NULL == t || t->item->id != LFBRACE )
			// Error: missing '{'
			report_error_tok( MISSING_LFBRACE, t->item );
		switch_label_count = label_count;

		//
		// Switch reserver 2 labels:
		// +0 - for break outside closing '}', 
		// +1 - for 'default' branch
		//
		label_count += 3;
		temp_in_construct = in_construct;
		in_construct = BREAK_VALID;
		break_label_count = switch_label_count;

		t = skip_wsp( t->next );
		if ( NULL == t )
		{
			// Error: unexpected EOF
			report_error_tok( BAD_EOF, NULL );
			return	NULL;
		}

		fprintf( out, "goto __label%d\n", switch_label_count + 2 );

		for ( done = 0; !done; )
		{
			if ( t->item->id == DEFAULT )
			{
				if ( dflt_used != 0 )
				{
					// Error: 'default' is already defined
					report_error_tok( DEFAULT_USED, t->item );
					return	skip_to_term( t );
				}

				dflt_used = 1;
				sprintf( line_buf, "__label%d:\n", switch_label_count + 1 );
				fputs( line_buf, out );
				cases_label = realloc( cases_label, ( n_cases + 1 ) * sizeof( *cases_label ) );
				cases_label[ n_cases - 1 ] = switch_label_count + 1;
				t = skip_wsp( t->next );

				if ( t->item->id != COLON )
					report_error_tok( MISSING_COLON, t->item );
				else
					t = skip_wsp( t->next );
				continue;
			}
			else if ( t->item->id == CASE )
			{
				// Case.
				root = alloc( sizeof( *root ) );
				init_parse_tree_node( root );
				t = skip_wsp( t->next );
				if ( t == NULL )
				{
					// Error: unexpected EOF
					report_error_tok( BAD_EOF, NULL );
					return	NULL;
				}

				glob_error = 0;
				t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_INTCONST );

				if ( glob_error )
				{
					t = skip_to_term( t );
					glob_error = 0;
					continue;
				}

				assign_temp_types( root );
				assign_temp_names( root );
				calc_parse_tree( root );

				for ( i = 0; i < n_cases; ++i )
				{
					if ( cases[ i ] == *( unsigned* )root->type->value )
					{
						// Error: case cases[ i ] already defined
						report_error_tok( CASE_USED, t->item );
						return	skip_to_term( t );
					}
				}

				// If error didn't occur
				cases = realloc( cases, ++n_cases * sizeof( *cases ) );
				cases[ n_cases - 1 ] = *( unsigned* )root->type->value;
				case_label_count = label_count;
				++label_count;
				sprintf( line_buf, "__label%d:\n", case_label_count );
				fputs( line_buf, out );
				cases_label = realloc( cases_label, n_cases * sizeof( *cases_label ) );
				cases_label[ n_cases - 1 ] = case_label_count;

				if ( t->item->id != COLON )
					report_error_tok( MISSING_COLON, t->item );
				else
					t = skip_wsp( t->next );
				continue;
			}
			else if ( t->item->id == RFBRACE )
			{
				// Closing '}'

				// Output code for jump table
				fprintf( out, "__asm\n" );
				fprintf( out, "__label%d:\n", switch_label_count + 2 );
				fprintf( out, ".CONST\n" );

				fprintf( out, "__switch%d_cases\tLABEL\tDWORD\n", switch_count );
				fprintf( out, "\tDD\t" );
				for ( i = 0; i < n_cases; ++i )
				{
					fprintf( out, "%d", cases[ i ] );
					if ( i < n_cases - 1 )
						fprintf( out, ", " );
				}
				fprintf( out, "\n" );

				fprintf( out, "__switch%d_cases_label\tLABEL\tDWORD\n", switch_count );
				fprintf( out, "\tDD\t" );
				for ( i = 0; i < n_cases; ++i )
				{
					fprintf( out, "__label%d", cases_label[ i ] );
					if ( i < n_cases - 1 )
						fprintf( out, ", " );
				}
				fprintf( out, "\n" );

				fprintf( out, ".CODE\n" );

				//
				// Switch end-of-statement cannot occur in the middle of expression, therefore no need to 
				// save registers.
				//
				fprintf( out, "\t%s\tEAX, %s PTR [%s]\n", 
					switch_root->type->type.type_ref.id >= T_INT ? "MOV" : is_signed( root->type ) ? "MOVSX" : "MOVZX",
					switch_root->type->type.type_ref.id >= T_INT ? "DWORD" : switch_root->type->type.type_ref.id >= T_SHORT ? "WORD" : "BYTE",
					switch_root->token );
				fprintf( out, "\tMOV\tEBX, -1\n" );
				fprintf( out, "@@:\n" );
				fprintf( out, "\tINC\tEBX\n" );
				fprintf( out, "\tCMP\tEBX, %d\n", n_cases );
				fprintf( out, "\tJNB\t@F\n" );
				fprintf( out, "\tCMP\tEAX, DWORD PTR [__switch%d_cases][EBX*4]\n", switch_count );
				fprintf( out, "\tJNE\t@B\n" );
				fprintf( out, "\tJMP\tDWORD PTR [__switch%d_cases_label][EBX*4]\n", switch_count );

				fprintf( out, "@@:\n" );
				if ( dflt_used )
					fprintf( out, "\tJMP\t__label%d\n", switch_label_count + 1 );

				fprintf( out, "__endasm\n" );

				++switch_count;

				fprintf( out, "__label%d:\n", switch_label_count );

				free( cases );
				done = 1;
				continue;
			}
			else
			{
				// Statements
				t = parse_statement( t );

				if ( t->item->id == EOSTM || t->item->id == RFBRACE )
					t = skip_wsp( t->next );
			}
		}

		in_construct = temp_in_construct;
   		// switch() selection statement (6.6.4.2)
   		//break;
		goto	process_rfbrace;

   	case WHILE:
   		// while() selection statement (6.6.5.1)
		t = skip_wsp( t->next );

		if ( NULL == t )
		{
			report_error( MISSING_LPAREN, NULL );
			return	NULL;
		}

		if ( t->item->id != LPAREN )
		{
			// Error: missing '('
			report_error_tok( MISSING_LPAREN, t->item );
			t = skip_to_term( t );
			break;
		}
		while_label_count = label_count;
		label_count += 2;

		temp_in_construct = in_construct;
		in_construct = CONT_VALID | BREAK_VALID;			// break_label and cont_label are valid
		break_label_count = while_label_count + 1;
		cont_label_count = while_label_count;

		sprintf( line_buf, "__label%d:\n", while_label_count );
		fputs( line_buf, out );
		++brace_level;

		root = alloc( sizeof( *root ) );
		init_parse_tree_node( root );

		glob_error = 0;
		t = parse_expression( root, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_CONDITION );

		if ( glob_error )
		{
			t = skip_to_term( t );
			glob_error = 0;
			break;
		}

		temp_level_int = temp_level_fp = 0;
		assign_temp_types( root );
		assign_temp_names( root );
		calc_parse_tree( root );
		print_parse_tree( root, out );

		// Sequence point (6.8.5)
		commit_seqpoint( out );

		if ( NULL == t || t->item->id != RPAREN )
		{
			// Error: missing ')'
			report_error_tok( MISSING_RPAREN, t->item );
			t = skip_to_term( t );
		}
		t = skip_wsp( t->next );
		sprintf( line_buf, "if ( !%s [ %s%d ] ) goto __label%d\n", root->token, is_float( root->type ) ? "FP" : "INT", do_sizeof( root->type ), while_label_count );
		fputs( line_buf, out );
		t = parse_statement( t );
		sprintf( line_buf, "goto __label%d\n", while_label_count );
		fputs( line_buf, out );
		sprintf( line_buf, "__label%d:\n", while_label_count + 1 );
		fputs( line_buf, out );
		in_construct = temp_in_construct;
   		break;

   	case DO:
   		// do() selection statement (6.6.5.2)
		t = skip_wsp( t->next );
		if ( NULL == t )
		{
			// Error: statement
		}
		do_label_count = label_count;
		label_count += 2;
		temp_in_construct = in_construct;
		in_construct = CONT_VALID | BREAK_VALID;			// break_label and cont_label are valid
		break_label_count = do_label_count + 1;
		cont_label_count = do_label_count;

		sprintf( line_buf, "__label%d:\n", do_label_count );
		fputs( line_buf, out );

		t = parse_statement( t );

		if ( NULL == t )
		{
			report_error( MISSING_WHILE, NULL );
			return	NULL;
		}

		if( t->item->id != WHILE )
		{
			// Error: missing while () condition.
			report_error_tok( MISSING_WHILE, t->item );
			t = skip_to_term( t );
			break;
		}
		t = skip_wsp( t->next );

		if ( NULL == t )
		{
			report_error( MISSING_LPAREN, NULL );
			return	NULL;
		}

		if ( t->item->id != LPAREN )
		{
			// Error: missing '('
			report_error_tok( MISSING_LPAREN, t->item );
			t = skip_to_term( t );
			break;
		}
		++brace_level;
		root = alloc( sizeof( *root ) );
		init_parse_tree_node( root );

		glob_error = 0;
		t = parse_expression( root, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_CONDITION );

		if ( glob_error )
		{
			glob_error = 0;
			t = skip_to_term( t );
			break;
		}

		temp_level_int = temp_level_fp = 0;
		assign_temp_types( root );
		assign_temp_names( root );
		calc_parse_tree( root );
		print_parse_tree( root, out );

		// Sequence point (6.8.5)
		commit_seqpoint( out );

		if ( NULL == t )
		{
			report_error( MISSING_RPAREN, NULL );
			return	t;
		}

		if ( t->item->id != RPAREN )
		{
			// Error: missing ')'
			if ( t != NULL )
			{
				report_error_tok( MISSING_RPAREN, t->item );
				t = skip_to_term( t );
			}
			return	t;
		}
		t = skip_wsp( t->next );
		sprintf( line_buf, "if ( %s [ %s%d ] ) goto __label%d\n", root->token, is_float( root->type ) ? "FP" : "INT", do_sizeof( root->type ), do_label_count );
		fputs( line_buf, out );

		sprintf( line_buf, "__label%d:\n", break_label_count );
		fputs( line_buf, out );

		in_construct = temp_in_construct;
   		break;

   	case FOR:
   		// for() selection statement (6.6.5.3)
		t = skip_wsp( t -> next );
		if ( NULL == t || t->item->id != LPAREN )
		{
			// Error: missing '('
			report_error_tok( MISSING_LPAREN, t->item );
			t = skip_to_term( t );
		}

		//
		//	1: parse for() init statement
		//

		//
		// The following may be also initialization in C99!!!!! - will be added later.
		//
		++brace_level;

		root = alloc( sizeof( *root ) );
		init_parse_tree_node( root );

		glob_error = 0;
		t = parse_expression( root, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_CONDITION );

		if ( glob_error )
		{
			glob_error = 0;
			t = skip_to_term( t );
			return	t;
		}

		temp_level_int = temp_level_fp = 0;
		assign_temp_types( root );
		assign_temp_names( root );
		calc_parse_tree( root );
		print_parse_tree( root, out );

		// Sequence point (6.8.5.3)
		commit_seqpoint( out );

		if ( NULL == t )
		{
			report_error( MISSING_EOSTM, NULL );
			return	NULL;
		}

		if ( t->item->id != EOSTM )
		{
			// Error: missing ';'
			report_error_tok( MISSING_EOSTM, t->item );
			t = skip_to_term( t );
			break;
		}

		for_label_count = label_count;
		label_count += 3;

		temp_in_construct = in_construct;
		in_construct = CONT_VALID | BREAK_VALID;
		break_label_count = for_label_count + 1;
		cont_label_count = for_label_count + 2;

		fprintf( out, "__label%d:\n", for_label_count );

		//
		//	2: parse for() condition
		//
		root = alloc( sizeof( *root ) );
		init_parse_tree_node( root );
		t = parse_expression( root, NULL, skip_wsp( t->next ), &rt, DFA_SEL_LEFT, PARSE_CONDITION );
		assign_temp_types( root );
		assign_temp_names( root );
		calc_parse_tree( root );
		print_parse_tree( root, out );

		// Sequence point (6.8.5.3)
		commit_seqpoint( out );

		fprintf( out, "if ( !%s [ %s%d ] ) goto __label%d\n", root->token, is_float( root->type ) ? "FP" : "INT", do_sizeof( root->type ), for_label_count + 1 );

		if ( NULL == t )
		{
			report_error( MISSING_EOSTM, NULL );
			return	NULL;
		}

		if ( t->item->id != EOSTM )
		{
			// Error: missing ';'
			report_error_tok( MISSING_EOSTM, t->item );
			t = skip_to_term( t );
			break;
		}

		//
		//	3: parse for() committment (last) statement
		//
		t = skip_wsp( t -> next );
		
		if ( NULL == t )
		{
			// Error: missing '}'
			report_error( MISSING_RFBRACE, NULL );
			return	NULL;
		}
		for_root = alloc( sizeof( *for_root ) );
		init_parse_tree_node( for_root );
		t = parse_expression( for_root, NULL, t, &rt, DFA_SEL_LEFT, PARSE_CONDITION );

		if ( NULL == t )
		{
			report_error( MISSING_RFBRACE, NULL );
			return	NULL;
		}
		if ( t->item->id != RPAREN )
		{
			// Error: missing '}'
			report_error_tok( MISSING_RFBRACE, t->item );
			t = skip_to_term( t );
		}
		t = skip_wsp( t->next );
		t = parse_statement( t );

		//	Drop code generated for 'for_root' expression tree here.
		//sprintf( line_buf, "goto __label%d\n", cont_label_count );
		//fputs( line_buf, out );
		fprintf( out, "__label%d:\n", for_label_count + 2 );

		temp_level_int = temp_level_fp = 0;
		assign_temp_types( for_root );
		assign_temp_names( for_root );
		calc_parse_tree( for_root );
		print_parse_tree( for_root, out );

		// Sequence point (6.8.5.3)
		commit_seqpoint( out );

		fprintf( out, "goto __label%d\n", for_label_count );
		fprintf( out, "__label%d:\n", for_label_count + 1 );
		in_construct = temp_in_construct;
   		break;

   	case GOTO:
   		// jump statement(6.6.6.1)
		t = skip_wsp( t -> next );
		if ( NULL == t || t->item->id != ID )
		{
			// Error: missing label
			report_error_tok( MISSING_LABEL, t->item );
			t = skip_to_term( t );
		}
		sprintf( line_buf, "goto %s\n", t->item->token );
		fputs( line_buf, out );

		t = skip_wsp( t->next );
		if ( NULL == t )
		{
			report_error( MISSING_EOSTM, NULL );
			return	NULL;
		}
		if ( t->item->id != EOSTM )
		{
			// Error: missing ';'
			report_error_tok( MISSING_EOSTM, NULL );
			t = skip_to_term( t );
		}
   		break;

   	case CONTINUE:
   		;	// jump statement(6.6.6.2)
		if ( in_construct & CONT_VALID )
		{
			sprintf( line_buf, "goto __label%d\n", cont_label_count );
			fputs( line_buf, out );
		}
		else
		{
			// Error: misplaced 'break'
			report_error_tok( BAD_BREAK, t->item );
			t = skip_to_term( t );
		}
   		break;

   	case BREAK:
   		;	// jump statement(6.6.6.3)
		if ( in_construct & BREAK_VALID )
		{
			sprintf( line_buf, "goto __label%d\n", break_label_count );
			fputs( line_buf, out );
			t = skip_wsp( t->next );
		}
		else
		{
			// Error: misplaced 'break'
		}
   		break;

   	case RETURN:
		t = skip_wsp( t->next );

		if ( NULL == t )
		{
			// Error: missing ';'
		}

		if ( t->item->id == EOSTM )
		{
			if ( curr_function->rv_type.type.type_id != TID_VOID )
			{
				// Error/warning: function must return a value.
				report_error_tok( MUST_RETURN, t->item );
				t = skip_to_term( t );
			}
		}
		else
		{
			if ( curr_function->rv_type.type.type_id == TID_VOID )
			{
				// Error/warning: void function returns a value.
				report_error_tok( VOID_RETURNS, t->item );
				t = skip_to_term( t );
			}

			root = alloc( sizeof( *root ) );
			init_parse_tree_node( root );

			glob_error = 0;
			t = parse_expression( root, NULL, t, &rt, DFA_SEL_LEFT, 0 );

			if ( glob_error )
			{
				glob_error = 0;
				t = skip_to_term( t );
				break;
			}

			if ( !compat_types( root->type, &curr_function->rv_type ) )
			{
				// Error: function returns incompatible type to declared rv.
				report_error_tok( INCOMPAT_RETURN, t->item );
				t = skip_to_term( t );
			}

			assign_temp_types( root );
			assign_temp_names( root );
			calc_parse_tree( root );
			print_parse_tree( root, out );

			if ( is_scalar( &curr_function->rv_type ) )
			{
				fprintf( out, "__ret_val_%s%d <- %s [ %s%d ]\n", 
					( curr_function->rv_type.type.ptr_list == NULL || 
					curr_function->rv_type.type.type_ref.id < T_FLOAT ) ? "int" : "fp", 
					do_sizeof( &curr_function->rv_type ), root->token, ( curr_function->rv_type.type.ptr_list == NULL || 
					curr_function->rv_type.type.type_ref.id < T_FLOAT ) ? "INT" : "FP", 
					do_sizeof( &curr_function->rv_type )
					);
			}
			else
			{
				fprintf( out, "__ret_val$%d <- %s\n", do_sizeof( &curr_function->rv_type ), root->token );
			}
		}

		// Sequence point (6.8.6.4)
		commit_seqpoint( out );

		if ( envlist_top->env_size != 0 )
		{
			hash_list_t	*p;

			for ( p = envlist_top; p != NULL; p = p->next )
			{
				fprintf( out, "__asm\n" );
				fprintf( out, "\tADD\tESP, 0%08XH\n", p->env_size );
				fprintf( out, "\tPOP\tEBP\n" );
				fprintf( out, "__endasm\n" );
			}

		}

		fprintf( out, "__return\n" );
   		;	// jump statement(6.6.6.4)
   		break;

   	case RFBRACE:
process_rfbrace:
   		;	// End of compound statement or function
		if ( envlist_top->env_size != 0 )
		{
			fprintf( out, "__asm\n" );
			fprintf( out, "\tADD\tESP, 0%08XH\n", envlist_top->env_size );
			fprintf( out, "\tPOP\tEBP\n" );
			fprintf( out, "__endasm\n" );

			free_env();
		}
		remove_env();
		--fbrace_level;
   		if ( fbrace_level > 0 )
   			return	/*t;	*/skip_wsp( t -> next );
		else if ( fbrace_level == 0 )
		{
			fprintf( out, "__return\n" );
			return	t;
		}
		else
		{
			// Error: misplaced '}'
			report_error_tok( BAD_RFBRACE, t->item );
			t = skip_to_term( t );
		}
   		break;
	}

#endif
	return	t;
}


