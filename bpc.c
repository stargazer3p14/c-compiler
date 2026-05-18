/*
 *	BPC.C
 *	------
 *
 *	Main source file for the Black Phantom's C compiler. It's a front-end of the compiler, parses command-line
 *	arguments and invokes the subsequent translation stages processors.
 *	
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"


// Include directories
char **inc_dir = NULL;
int	inc_dirs = 0;

// Preprocessor commands to process at the beginning (once).
char **def_sym = NULL;
int	def_syms = 0;

extern char	outf[ 256 ], out_asmf[ 256 ];
extern	int	global_flags;

static	void	print_help()
{
	printf( 
			"Black Phantom's C compiler. Copyright (c) 2003\n\n" 
			"Command-line options:\n"
			"  -I<include_dir>\tAdd a directory to search-for-headers set\n"
			"  -D<macro>[=something]\tDefine something\n"
			"  -v\t\tVerbose output\n"
			"  -h\t\t\tPrint this help\n"
		);
}


int	main( int argc, char **argv )
{
	int	i, j;
	char	*src_file = NULL;
	char	buf[ 1024 ];
	char	*p;

	for ( i = 1; i < argc; ++i )
	{
		if ( argv[ i ][ 0 ] == '-' && argv[ i ][ 1 ] == 'I' )
		{
			if ( argv[ i ][ 2 ] == '\0' )
				++i, j = 0;
			else
				j = 2;

			++inc_dirs;
			inc_dir = realloc( inc_dir, inc_dirs * sizeof( *inc_dir ) );
			inc_dir[ inc_dirs - 1 ] = alloc( strlen( argv[ i ] + j ) + 1 );
				strcpy( inc_dir[ inc_dirs - 1 ], argv[ i ] + j );
		}
		else if ( argv[ i ][ 0 ] == '-' && argv[ i ][ 1 ] == 'D' )
		{
			char	*p;

			if ( argv[ i ][ 2 ] == '\0' )
				++i, j = 0;
			else
				j = 2;

			++def_syms;
			def_sym = realloc( def_sym, def_syms * sizeof( *def_sym ) );
			sprintf( buf, "#define %s\n", argv[ i ] + j );
			p = strchr( buf, '=' );
			if ( p != NULL )
				*p = ' ';
			def_sym[ def_syms - 1 ] = alloc( strlen( buf ) + 1 );
			strcpy( def_sym[ def_syms - 1 ], buf );
			
		}
		else if ( argv[ i ][ 0 ] == '-' && argv[ i ][ 1 ] == 'v' )
		{
			global_flags = GFL_PRINT_TOKENS | GFL_PRINT_TABLES;
		}

		else if ( argv[ i ][ 0 ] == '-' && argv[ i ][ 1 ] == 'h' )
		{
			print_help();
			return	EXIT_SUCCESS;
		}
		else
		{
			if ( NULL == src_file )
				src_file = argv[ i ];
		}
	}

	strcpy( outf, src_file );
	strcpy( out_asmf, src_file );
	if ( ( p = strstr( outf, ".c" ) ) || ( p = strstr( outf, ".C" ) ) )
	{
		strcpy( p, ".im" );
		p = strstr( out_asmf, ".c" );
		if ( NULL == p )
			p = strstr( out_asmf, ".C" );
		strcpy( p, ".asm" );
	}

	else
	{
		strcat( outf, ".im" );
		strcat( out_asmf, ".asm" );
	}

	parser_entry( src_file );

 	return	EXIT_SUCCESS;
}