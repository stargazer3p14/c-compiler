/*
 *	CODEGEN.C
 *	----------
 *
 *	Source for generating assembly output from intermediate code
 */

#include	<stdio.h>
#include	<string.h>
#include	<stdlib.h>
#include	"parser.h"


extern	parse_tree_t	*root, *current;
extern	sym_hash_t	*sym_hash[ SYM_HASH_SIZE ], *type_hash[ TYPE_HASH_SIZE ], *func_hash[ FUNC_HASH_SIZE ], *lit_hash[ LIT_HASH_SIZE ];
extern	unsigned	def_types;
extern	FILE	*out;
extern	char	line_buf[ 1024 ];
extern	int	temp_level_fp, temp_level_int;
extern	int	max_temp_level_fp, max_temp_level_int;
extern	int	brace_level, fbrace_level, subscr_level;
extern	int	temp_level, label_count, var_count;
extern	hash_list_t	*envlist_top, *envlist_bot, *typelist_top, *typelist_bot;

int		fp_const_count = 0;

char	*int_reg_name[] = { "EAX", "EBX", "ECX", "EDX", "ESI", "EDI" };
char	*short_reg_name[] = { "AX", "BX", "CX", "DX", "SI", "DI" };
char	*byte_reg_name[] = { "AL", "BL", "CL", "DL" };

FILE	*src, *dest;
char	line[ 1024 ];
sym_hash_t	*sh;

int		asm_label_count = 0;

//
//	Functions declarations
//
static	__inline	void	generate_init_array( const record_t *arr, int control_flags );
static	__inline	void	generate_init_struct( const record_t *arr, int control_flags );
static	__inline	void	generate_init_union( const record_t *arr, int control_flags );
static	__inline	void	generate_init_scalar( const record_t *arr, int control_flags );

static	__inline	void	generate_mov( const char *src1_opnd, const char *src2_opnd, const char *size_spec )
{
	int		rv1, rv2;
	char	dest1_opnd[ 256 ], dest2_opnd[ 256 ];
	int		reg1_num, reg2_num;
	int		mov_count;
	int		i;
	int		sz;

	//	Handle dereference.
	if ( src1_opnd[ 0 ] == '#' )
	{
		++src1_opnd;
		rv1 = sscanf( src1_opnd, "__temp_int%d", &reg1_num );

		if ( rv1 == 1 && reg1_num >= 0 && reg1_num < 6 )
		{
			sprintf( dest1_opnd, "[%s]", int_reg_name[ reg1_num ] );
			generate_mov( dest1_opnd, src2_opnd, size_spec );
			return;
		}
		else
		{
			fprintf( dest, "\tPUSH\tEAX\n" );
			fprintf( dest, "\tMOV\tEAX, DWORD PTR [%s]\n", src1_opnd + 1 );
			generate_mov( "[EAX]", src2_opnd, size_spec );
			fprintf( dest, "\tPOP\tEAX\n" );
			return;
		}
	}
	if ( src2_opnd[ 0 ] == '#' )
	{
		++src2_opnd;
		rv2 = sscanf( src2_opnd, "__temp_int%d", &reg2_num );
		if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
		{
			if ( strcmp( src1_opnd, "[EAX]" ) == 0 && strcmp( src2_opnd, "__temp_int0" ) == 0 )
			{
				fprintf( dest, "\tPUSH\tEBX\n" );
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [ESP+4]\n" );
				generate_mov( src1_opnd, "[EBX]", size_spec );
				fprintf( dest, "\tPOP\tEBX\n" );
				return;
			}
			else
			{
				sprintf( dest2_opnd, "[%s]", int_reg_name[ reg2_num ] );
				generate_mov( src1_opnd, dest2_opnd, size_spec );
				return;
			}
		}
		else
		{
			fprintf( dest, "\tPUSH\tEBX\n" );
			if ( strcmp( src1_opnd, "[EAX]" ) == 0 && strcmp( src2_opnd, "[EAX]" ) == 0 )
			{
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [ESP+4]\n" );
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [EBX]\n" );
			}
			else
			{
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [%s]\n", src2_opnd + 1 );
			}
			generate_mov( src1_opnd, "[EBX]", size_spec );
			fprintf( dest, "\tPOP\tEBX\n" );
			return;
		}
	}

	//
	//	If src1_opnd or src2_opnd is __ret_val*, handle it with __ret_buf. In BPC
	//	both cannot be __ret_val*
	//
	if ( strncmp( src1_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strncmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_int", sizeof( "int" ) - 1 ) == 0 )
			src1_opnd = "__temp_int0";
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp" ) == 0 )
			src1_opnd = "__temp_fp0";
 		else
 		{
			// Wrong
 			src2_opnd = "[EBP+8]";
 		}
	}

	if ( strncmp( src2_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strncmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_int", sizeof( "int" ) - 1 ) == 0 )
 		{
 			fprintf( dest, "\tMOV\tDWORD PTR __ret_buf, EAX\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp4" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tDWORD PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp8" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tQWORD PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp10" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tTBYTE PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else
 		{
 			src2_opnd = "[ESP]";
 		}
 	}

	rv1 = sscanf( src1_opnd, "__temp_int%d", &reg1_num );
	rv2 = sscanf( src2_opnd, "__temp_int%d", &reg2_num );

	if ( sscanf( size_spec, "UINT%d", &sz ) != 1 )
		if ( sscanf( size_spec, "INT%d", &sz ) != 1 )
			sscanf( size_spec, "FP%d", &sz );

	if (  rv1 == 1 && reg1_num >= 0 && reg1_num < 6 )
	{
		strcpy( dest1_opnd, int_reg_name[ reg1_num ] );
		if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
		{
			strcpy( dest2_opnd, int_reg_name[ reg2_num ] );
			fprintf( dest, "\tMOV\t%s, %s\n", dest1_opnd, dest2_opnd );
		}
		else if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '\'' )
		{
			fprintf( dest, "\tMOV\t%s, %s\n", dest1_opnd, src2_opnd );
		}
		else
		{
			if ( sz == 4 )
			{
				fprintf( dest, "\tMOV\t%s, DWORD PTR [%s]\n", dest1_opnd, src2_opnd );
			}
			else if ( sz == 2 )
			{
				fprintf( dest, "\t%s\t%s, WORD PTR [%s]\n", size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
					dest1_opnd, src2_opnd );
			}
			else if ( sz == 1 )
			{
				fprintf( dest, "\t%s\t%s, BYTE PTR [%s]\n", size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
					dest1_opnd, src2_opnd );
			}
		}
	}
	else
	{
		strcpy( dest1_opnd, src1_opnd );
		if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
		{
			strcpy( dest2_opnd, int_reg_name[ reg2_num ] );
			if ( sz == 4 )
				fprintf( dest, "\tMOV\tDWORD PTR [%s], %s\n", dest1_opnd, dest2_opnd );
			else if ( sz == 2 )
				fprintf( dest, "\tMOV\tWORD PTR [%s], %s\n", dest1_opnd, short_reg_name[ reg2_num ] );
			else if ( sz == 1 )
			{
				if ( reg2_num < 4 )
					fprintf( dest, "\tMOV\tBYTE PTR [%s], %s\n", dest1_opnd, byte_reg_name[ reg2_num ] );
				else
				{
					fprintf( dest, "\tPUSH\tEAX\n" );
					fprintf( dest, "\tMOV\tEAX, %s\n", dest2_opnd );
					fprintf( dest, "\tMOV\tBYTE PTR [%s], AL\n", dest1_opnd );
					fprintf( dest, "\tPOP\tEAX\n" );
				}
			}
		}
		else
		{
			strcpy( dest2_opnd, src2_opnd );
default_mov:
			if ( sz == 4 )
			{
				if ( size_spec[ 0 ] == 'F' )
					goto	fp4_opnd;

				strcpy( dest2_opnd, src2_opnd );
				if ( isnum( dest2_opnd[ 0 ] ) )
					fprintf( dest, "\tPUSH\t%s\n", dest2_opnd );
				else
					fprintf( dest, "\tPUSH\tDWORD PTR [%s]\n", dest2_opnd );
				fprintf( dest, "\tPOP\tDWORD PTR [%s]\n", dest1_opnd );
			}
			else if ( sz == 2 )
			{
				fprintf( dest, "\tPUSH\tEAX\n" );
				if ( !isnum( dest2_opnd[ 0 ] ) )
					fprintf( dest, "\tMOVZX\tEAX, WORD PTR [%s]\n", dest2_opnd );
				else
					fprintf( dest, "\tMOV\tEAX, %s\n", dest2_opnd );
				fprintf( dest, "\tMOV\tWORD PTR [%s], AX\n", dest1_opnd );
				fprintf( dest, "\tPOP\tEAX\n" );
			}
			else if ( sz == 1 )
			{
				fprintf( dest, "\tPUSH\tEAX\n" );
				if ( !isnum( dest2_opnd[ 0 ] ) )
					fprintf( dest, "\tMOVZX\tEAX, BYTE PTR [%s]\n", dest2_opnd );
				else
					fprintf( dest, "\tMOV\tEAX, %s\n", dest2_opnd );
				fprintf( dest, "\tMOV\tBYTE PTR [%s], AL\n", dest1_opnd );
				fprintf( dest, "\tPOP\tEAX\n" );
			}
			else if ( sz == 10 )
			{
				if ( isnum( dest2_opnd[ 0 ] ) )
				{
					double	tmp;

					sscanf( dest2_opnd, "%f", &tmp );

					// MSVC library bug.
					*( double* )&tmp = *( float* )&tmp;

					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDT\t%f\n", fp_const_count, tmp );
					fprintf( dest, ".CODE\n" );
					sprintf( dest2_opnd, "__fp_const%d", fp_const_count );
					++fp_const_count;
				}
				fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", dest2_opnd );
				fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", dest1_opnd );
			}
			else if ( sz == 8 )
			{
				if ( isnum( dest2_opnd[ 0 ] ) )
				{
					double	tmp;

					sscanf( dest2_opnd, "%f", &tmp );

					// MSVC library bug.
					*( double* )&tmp = *( float* )&tmp;

					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDQ\t%f\n", fp_const_count, tmp );
					fprintf( dest, ".CODE\n" );
					sprintf( dest2_opnd, "__fp_const%d", fp_const_count );
					++fp_const_count;
				}
				fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", dest2_opnd );
				fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", dest1_opnd );
			}
			else if ( sz == 4 )
			{
fp4_opnd:
				if ( isnum( dest2_opnd[ 0 ] ) )
				{
					double	tmp;

					sscanf( dest2_opnd, "%f", &tmp );

					// MSVC library bug.
					*( double* )&tmp = *( float* )&tmp;

					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDD\t%f\n", fp_const_count, tmp );
					fprintf( dest, ".CODE\n" );
					sprintf( dest2_opnd, "__fp_const%d", fp_const_count );
					++fp_const_count;
				}
				fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", dest2_opnd );
				fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", dest1_opnd );
			}
			else if ( size_spec[ 0 ] == '$' )
			{
				sscanf( size_spec + 1, "%d", &mov_count );
				fprintf( dest, "\tPUSH\tEAX\n" );

				for ( i = 0; i < mov_count & ~3; i += 4 )
				{
					fprintf( dest, "\tMOV\tEAX, DWORD PTR [%s+%d]\n", dest2_opnd, i );
					fprintf( dest, "\tMOV\tDWORD PTR [%s+%d], EAX\n", dest1_opnd, i );
				}
				if ( ( mov_count & 2 ) != 0 )
				{
					fprintf( dest, "\tMOVZX\tEAX, WORD PTR [%s+%d]\n", dest2_opnd, i );
					fprintf( dest, "\tMOV\tWORD PTR [%s+%d]\n", dest1_opnd, i );
					i += 2;
				}
				if ( ( mov_count & 1 ) != 0 )
				{
					fprintf( dest, "\tMOVZX\tEAX, BYTE PTR [%s+%d]\n", dest2_opnd, i );
					fprintf( dest, "\tBYTE PTR [%s+%d], AL\n", dest1_opnd, i );
				}

				fprintf( dest, "\tPOP\tEAX\n" );
			}
			else
			{
				goto	default_mov;
			}
		}
	}
}


static	__inline	void	generate_ali( char *dest_opnd, char *src1_opnd, char *src2_opnd, char *instr, const char *size_spec )
{
	int		rv1, rv2;
	char	dest1_opnd[ 256 ], dest2_opnd[ 256 ];
	int		reg1_num, reg2_num;


	//	Handle dereference.
	if ( src1_opnd[ 0 ] == '#' )
	{
		++src1_opnd;
		rv1 = sscanf( src1_opnd, "__temp_int%d", &reg1_num );

		if ( rv1 == 1 && reg1_num >= 0 && reg1_num < 6 )
		{
			sprintf( dest1_opnd, "[%s]", int_reg_name[ reg1_num ] );
			generate_ali( dest_opnd, dest1_opnd, src2_opnd, instr, size_spec );
			return;
		}
		else
		{
			fprintf( dest, "\tPUSH\tEAX\n" );
			fprintf( dest, "\tMOV\tEAX, DWORD PTR [%s]\n", src1_opnd );
			generate_ali( dest_opnd, "[EAX]", src2_opnd, instr, size_spec );
			fprintf( dest, "\tPOP\tEAX\n" );
			return;
		}
	}
	if ( src2_opnd[ 0 ] == '#' )
	{
		++src2_opnd;
		rv2 = sscanf( src2_opnd, "__temp_int%d", &reg2_num );
		if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
		{
			if ( strcmp( src1_opnd, "[EAX]" ) == 0 && strcmp( src2_opnd, "__temp_int0" ) == 0 )
			{
				fprintf( dest, "\tPUSH\tEBX\n" );
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [ESP+4]\n" );
				generate_ali( dest_opnd, src1_opnd, "[EBX]", instr, size_spec );
				fprintf( dest, "\tPOP\tEBX\n" );
				return;
			}
			else
			{
				sprintf( dest2_opnd, "[%s]", int_reg_name[ reg2_num ] );
				generate_ali( dest_opnd, src1_opnd, dest2_opnd, instr, size_spec );
				return;
			}
		}
		else
		{
			fprintf( dest, "\tPUSH\tEBX\n" );
			if ( strcmp( src1_opnd, "[EAX]" ) == 0 && strcmp( src2_opnd, "[EAX]" ) == 0 )
			{
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [ESP+4]\n" );
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [EBX]\n" );
			}
			else
			{
				fprintf( dest, "\tMOV\tEBX, DWORD PTR [%s]\n", src2_opnd );
			}
			generate_ali( dest_opnd, src1_opnd, "[EBX]", instr, size_spec );
			fprintf( dest, "\tPOP\tEBX\n" );
			return;
		}
	}

	if ( strncmp( dest_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strncmp( dest_opnd + sizeof( "__ret_val" ) - 1, "_int", sizeof( "int" ) - 1 ) == 0 )
			dest_opnd = "__temp_int0";
 		else if ( strcmp( dest_opnd + sizeof( "__ret_val" ) - 1, "_fp" ) == 0 )
			dest_opnd = "__temp_fp0";
 		else
 		{
			// Wrong
 			dest_opnd = "[EBP+8]";
 		}
	}

	if ( strncmp( src1_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strncmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_int", sizeof( "int" ) - 1 ) == 0 )
 		{
 			fprintf( dest, "\tMOV\tDWORD PTR __ret_buf, EAX\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp4" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tDWORD PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp8" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tQWORD PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp10" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tTBYTE PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else
 		{
 			src1_opnd = "[ESP]";
 		}
 	}

	if ( strncmp( src2_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strncmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_int", sizeof( "int" ) - 1 ) == 0 )
 		{
 			fprintf( dest, "\tMOV\tDWORD PTR __ret_buf, EAX\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp4" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tDWORD PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp8" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tQWORD PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src2_opnd + sizeof( "__ret_val" ) - 1, "_fp10" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tTBYTE PTR __ret_buf\n" );
 			src2_opnd = "__ret_buf";
 		}
 		else
 		{
 			src2_opnd = "[ESP]";
 		}
 	}


	//
	//	For FP only "ADD", "SUB" and "CMP" can arrive, other operations fail at parsing
	//
	if ( strncmp( size_spec, "FP", sizeof( "FP" ) - 1 ) == 0 )
	{
		int	fp_size;

		sscanf( size_spec, "FP%d", &fp_size );

		if ( isnum( src1_opnd[ 0 ] ) )
		{
			fprintf( dest, ".CONST\n" );
			fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
				fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
				src1_opnd );
			fprintf( dest, ".CODE\n" );
			sprintf( src1_opnd, "__fp_const%d", fp_const_count );
			++fp_const_count;
		}
		if ( isnum( src2_opnd[ 0 ] ) )
		{
			fprintf( dest, ".CONST\n" );
			fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
				fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
				src2_opnd );
			fprintf( dest, ".CODE\n" );
			sprintf( src2_opnd, "__fp_const%d", fp_const_count );
			++fp_const_count;
		}

		fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
			src1_opnd );
		if ( strcmp( instr, "CMP" ) != 0 )
		{
			fprintf( dest, "\tF%s\t%s [%s]\n", instr, fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
				src2_opnd );
		}
		else
		{
			fprintf( dest, "\tFCOM\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
				src2_opnd );
			fprintf( dest, "\tFSTSW\tAX\n" );
			fprintf( dest, "\tSAHF\n" );
		}
		fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
			dest_opnd );

		return;
	}

	//
	//	If src1_opnd or src2_opnd is __ret_val*, handle it with __ret_buf. In BPC
	//	both cannot be __ret_val*
	//
	if ( strncmp( src1_opnd, "__ret_val", sizeof( "__ret_val" ) - 1 ) == 0 )
	{
 		if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_int" ) == 0 )
 		{
 			fprintf( dest, "\tMOV\tDWORD PTR __ret_buf, EAX\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp4" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tDWORD PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp8" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tQWORD PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else if ( strcmp( src1_opnd + sizeof( "__ret_val" ) - 1, "_fp10" ) == 0 )
 		{
 			fprintf( dest, "\tFSTP\tTBYTE PTR __ret_buf\n" );
 			src1_opnd = "__ret_buf";
 		}
 		else
 		{
 			src1_opnd = "[ESP]";
 		}
 	}

	rv1 = sscanf( src1_opnd, "__temp_int%d", &reg1_num );
	rv2 = sscanf( src2_opnd, "__temp_int%d", &reg2_num );

	if (  strcmp( dest_opnd, src1_opnd ) == 0 )
	{
		// first operand is register: regN = regN @ opnd
		if ( rv1 == 1 && reg1_num >= 0 && reg1_num < 6 )
		{
			if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
			{
				fprintf( dest, "\t%s\t%s, %s\n", instr, int_reg_name[ reg1_num ], int_reg_name[ reg2_num ] );
			}
			else if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '\'' )
			{
				fprintf( dest, "\t%s\t%s, %s\n", instr, int_reg_name[ reg1_num ], src2_opnd );
			}
			else if ( strcmp( size_spec, "INT4" ) == 0 || strcmp( size_spec, "UINT4" ) == 0 )
			{
				fprintf( dest, "\t%s\t%s, DWORD PTR [%s]\n", instr, int_reg_name[ reg1_num ], src2_opnd );
			}
			else if ( strcmp( size_spec, "INT2" ) == 0 || strcmp( size_spec, "UINT2" ) == 0 )
			{
				if ( reg1_num > 0 )
					fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, WORD PTR [%s]\n",  size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX", src2_opnd );
				if ( reg1_num > 0 )
				{
					fprintf( dest, "\t%s\t%s, EAX\n", instr, int_reg_name[ reg1_num ] );
					fprintf( dest, "\tPOP\tEAX\n" );
				}
			}
			else
			{
				if ( reg1_num > 0 )
					fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, BYTE PTR [%s]\n",  size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX", src2_opnd );
				if ( reg1_num > 0 )
				{
					fprintf( dest, "\t%s\t%s, EAX\n", instr, int_reg_name[ reg1_num ] );
					fprintf( dest, "\tPOP\tEAX\n" );
				}
			}
		}
		else
		{
			// first operand is memory location: [mem] = [mem] @ opnd

			int	sz_spec;

			if ( sscanf( size_spec, "INT%d", &sz_spec ) != 1 )
				sscanf( size_spec, "UINT%d", &sz_spec );

			if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '\'' )
			{
				fprintf( dest, "\t%s\t%s PTR [%s], %s\n",
					instr,
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src1_opnd, 	src2_opnd );
			}
			else
			{
				fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
					sz_spec == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src2_opnd );
				fprintf( dest, "\t%s\t%s PTR %s, %s\n", 
					instr,
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src1_opnd,
					sz_spec == 4 ? "EAX" : sz_spec == 2 ? "AX" : "AL" );
				fprintf( dest, "\tPOP\tEAX\n" );
			}
		}

		return;
	}
	else if ( strcmp( dest_opnd, src2_opnd ) == 0 )
	{
		// Arithmetic-logic operations are symmetric: A + B <-> B + A
		if ( rv2 == 1 && reg2_num >= 0 && reg2_num < 6 )
		{
			if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '\'' )
			{
				fprintf( dest, "\t%s\t%s, %s\n", instr, int_reg_name[ reg2_num ], src1_opnd );
			}
			else if ( strcmp( size_spec, "INT4" ) == 0 || strcmp( size_spec, "UINT4" ) == 0 )
			{
				fprintf( dest, "\t%s\t%s, DWORD PTR [%s]\n", instr, int_reg_name[ reg2_num ], src1_opnd );
			}
			else if ( strcmp( size_spec, "INT2" ) == 0 || strcmp( size_spec, "UINT2" ) == 0 )
			{
				if ( reg2_num > 0 )
					fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, WORD PTR [%s]\n",  size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX", src1_opnd );
				if ( reg2_num > 0 )
				{
					fprintf( dest, "\t%s\t%s, EAX\n", instr, int_reg_name[ reg2_num ] );
					fprintf( dest, "\tPOP\tEAX\n" );
				}
			}
			else
			{
				if ( reg2_num > 0 )
					fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, BYTE PTR [%s]\n",  size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX", src1_opnd );
				if ( reg2_num > 0 )
				{
					fprintf( dest, "\t%s\t%s, EAX\n", instr, int_reg_name[ reg2_num ] );
					fprintf( dest, "\tPOP\tEAX\n" );
				}
			}
		}
		else
		{
			int	sz_spec;

			if ( sscanf( size_spec, "INT%d", &sz_spec ) != 1 )
				sscanf( size_spec, "UINT%d", &sz_spec );

			if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '\'' )
			{
				fprintf( dest, "\t%s\t%s PTR [%s], %s\n",
					instr,
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src2_opnd, 	src1_opnd );
			}
			else
			{

				fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
					sz_spec == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src1_opnd );
				fprintf( dest, "\t%s\t%s PTR %s, %s\n", 
					instr,
					sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
					src2_opnd,
					sz_spec == 4 ? "EAX" : sz_spec == 2 ? "AX" : "AL" );
				fprintf( dest, "\tPOP\tEAX\n" );
			}
		}
	}
	else
	{
		generate_mov( dest_opnd, src1_opnd, size_spec );
		generate_ali( dest_opnd, dest_opnd, src2_opnd, instr, size_spec );
	}
}


#if 0
static	__inline	void	generate_shift( const char *dest_opnd, const char *src1_opnd, const char *src2_opnd, char *instr )
{
	int		rv1, rv2;
	char	dest1_opnd[ 256 ], dest2_opnd[ 256 ];
	int		reg1_num, reg2_num;

	rv1 = sscanf( src1_opnd, "__temp_int%d", &reg1_num );
	rv2 = sscanf( src2_opnd, "__temp_int%d", &reg2_num );
}
#endif


static	__inline	void	generate_conversion( const char *src_line )
{
	char dest_op[ 256 ], src_op[ 256 ], t_dest[ 10 ], t_src[ 10 ];
	int	sz_dest, sz_src;
	int	n;

	sscanf( src_line, " %s <- %s ( %s %d <= %s %d )", dest_op, src_op, t_dest, &sz_dest, t_src, &sz_src );

	switch( t_src[ 0 ] )
	{
	case 'F':
		switch ( t_dest[ 0 ] )
		{
		case 'F':
			// Float to float conversion
			switch ( sz_dest )
			{
			case 4:
				if ( sz_src == 8 )
				{
					fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", dest_op );
				}
				else if ( sz_src == 4 )
				{
					fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", dest_op );
				}
				else	// 10
				{
					fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", dest_op );
				}
				break;
			case 8:
				if ( sz_src == 4 )
				{
					fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", dest_op );
				}
				else if ( sz_src == 8 )
				{
					fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", dest_op );
				}
				else	// 10
				{
					fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", dest_op );
				}
				break;
			case 10:
				if ( sz_src == 4 )
				{
					fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", dest_op );
				}
				else if ( sz_src == 10 )
				{
					fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", dest_op );
				}
				else	// 8
				{
					fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", src_op );
					fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", dest_op );
				}
				break;
			}
			break;

		case 'U':
		case 'I':
			// Float to unsigned integer conversion.
			switch ( sz_src )
			{
			case 4:
				fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", src_op );
				break;
			case 8:
				fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", src_op );
				break;
			case 10:
				fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", src_op );
				break;
			}

			if ( sscanf( dest_op, "__temp_int%d\n", &n ) == 1 && n >= 0 && n < 6 )
			{
				fprintf( dest, "\tSUB\tESP, 12\n" );
				fprintf( dest, "\tFISTP\tDWORD PTR [ESP]\n" );
				fprintf( dest, "\tMOV\t%s, [ESP]\n", int_reg_name[ n ] );
				fprintf( dest, "\tADD\tESP, 12\n" );
				break;
			}

			switch ( sz_dest )
			{
			case 1:
				fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\tSUB\tESP, 12\n" );
				fprintf( dest, "\tFISTP\tDWORD PTR [ESP]\n" );
				fprintf( dest, "\tMOV\tEAX, [ESP]\n", int_reg_name[ n ] );
				fprintf( dest, "\tADD\tESP, 12\n" );
				fprintf( dest, "\tMOV\t BYTE PTR [%s], AL\n", dest_op );
				fprintf( dest, "\tPOP\tEAX\n" );
				break;

			case 2:
				fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\tSUB\tESP, 12\n" );
				fprintf( dest, "\tFISTP\tDWORD PTR [ESP]\n" );
				fprintf( dest, "\tMOV\tEAX, [ESP]\n", int_reg_name[ n ] );
				fprintf( dest, "\tADD\tESP, 12\n" );
				fprintf( dest, "\tMOV\t WORD PTR [%s], AX\n", dest_op );
				fprintf( dest, "\tPOP\tEAX\n" );
				break;

			case 4:
				fprintf( dest, "\tPUSH\tEAX\n" );
				fprintf( dest, "\tSUB\tESP, 12\n" );
				fprintf( dest, "\tFISTP\tDWORD PTR [ESP]\n" );
				fprintf( dest, "\tMOV\tEAX, [ESP]\n", int_reg_name[ n ] );
				fprintf( dest, "\tADD\tESP, 12\n" );
				fprintf( dest, "\tMOV\t DWORD PTR [%s], EAX\n", dest_op );
				fprintf( dest, "\tPOP\tEAX\n" );
				break;
			}
			break;
		}
		break;

	case 'U':
	case 'I':
		switch ( t_dest[ 0 ] )
		{
		case 'F':
			// Convert int to float: int to 64-bit int (signed or unsigned), then FILD
			fprintf( dest, "\tPUSH\tEAX\n" );
			fprintf( dest, "\tPUSH\t0\n" );
			if ( sscanf( src_op, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
			{
				if ( n > 0 )
					fprintf( dest, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
			}
			else
			{
				fprintf( dest, "\t%s\tEAX, %s [%s]\n", sz_src == 4 ? "MOV" : ( t_src[ 0 ] == 'U' ? "MOVZX" : "MOVSX" ),
					( sz_src == 1 ? "BYTE PTR" : ( sz_src == 2 ? "WORD PTR" : "DWORD PTR" ) ),
					src_op );
			}

			if ( src_op[ 0 ] == 'I' )
			{
				fprintf( dest, "\tPUSH\tEDX\n" );
				fprintf( dest, "\tCDQ\n" );
				fprintf( dest, "\tMOV\t[ESP+4], EAX\n" );
				fprintf( dest, "\tPOP\tEDX\n" );
			}
			fprintf( dest, "\tPUSH\tEAX\n" );
			fprintf( dest, "\tFILD\tQWORD PTR [ESP]\n" );
			fprintf( dest, "\tADD\tESP, 8\n" );
			fprintf( dest, "\tPOP\tEAX\n" );

			fprintf( dest, "\tFSTP\t%s [%s]\n",
				( sz_dest == 4 ? "DWORD PTR" : ( sz_dest == 8 ? "QWORD PTR" : "TBYTE PTR" ) ),
				dest_op );

			break;
		case 'U':
		case 'I':
			if ( strncmp( src_op, "__temp_int", sizeof( "__temp_int" ) - 1 ) == 0 && 
				strncmp( src_op, "__temp_int", sizeof( "__temp_int" ) -1 ) == 0 )
			{
				sscanf( dest_op, "__temp_int%d\n", &n );

				if ( sz_dest > sz_src )
				{
					if ( t_src[ 0 ] == 'I' )
					{
						if ( strcmp( dest_op, "__temp_int0" ) != 0 )
						{
							fprintf( dest, "\tPUSH\tEAX\n" );
							fprintf( dest, "\tMOV\tEAX, %s\n", n >= 0 && n < 6 ? int_reg_name[ n ] : dest_op );
						}
						if ( sz_src == 1 )
							fprintf( dest, "\tCBW\n" );
						fprintf( dest, "\tCWDE\n" );

						if ( strcmp( dest_op, "__temp_int0" ) != 0 )
						{
							fprintf( dest, "\tMOV\t%s, EAX\n", n >= 0 && n < 6 ? int_reg_name[ n ] : dest_op );
							fprintf( dest, "\tPOP\tEAX\n" );
						}
					}
					else
					{
						generate_mov( dest_op, src_op, "INT4" );
						fprintf( dest, "\tAND\t%s, %d\n", n >= 0 && n < 6 ? int_reg_name[ n ] : dest_op, 
							sz_src == 2 ? 0xFFFF : 0xFF );
					}
				}
				else
				{
					generate_mov( dest_op, src_op, "INT4" );
				}

				break;
			}

			if ( strcmp( dest_op, "__temp_int0" ) != 0 )
				fprintf( dest, "\tPUSH\tEAX\n" );
			if ( sscanf( src_op, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
				fprintf( dest, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
			else
				fprintf( dest, "\t%s\tEAX, %s [%s]\n", ( sz_src == 4 ? "MOV" : ( t_src[ 0 ] == 'U' ? "MOVZX" : "MOVSX" ) ),
					( sz_src == 4 ? "DWORD PTR" : ( sz_src == 2 ? "WORD PTR" : "BYTE PTR" ) ), src_op );
			if ( sscanf( dest_op, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
			{
				if ( n > 0 )
					fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
			}
			else
			{
				fprintf( dest, "\tMOV\t %s [%s], %s\n", 
					( sz_dest == 4 ? "DWORD PTR" : ( sz_dest == 2 ? "WORD PTR" : "BYTE PTR" ) ),
					dest_op, ( sz_dest == 4 ? "EAX" : ( sz_dest == 2 ? "AX" : "AL" ) ), src_op );
			}
			if ( strcmp( dest_op, "__temp_int0" ) != 0 )
				fprintf( dest, "\tPOP\tEAX\n" );
			break;
		}
		break;

	}
}


static	__inline	void	generate_init_scalar( const record_t *rec, int control_flags )
{
	int	is_static;
	int	loc_static;
	char	var_name[ 256 ];

	is_static = ( control_flags & PARSE_GLOBAL ) || ( sh->type.spec & SPEC_STATIC );
	loc_static = ( control_flags & PARSE_LOCAL ) && ( sh->type.spec & SPEC_STATIC );

	if ( control_flags & PARSE_MEMBER )
   		var_name[ 0 ] = '\0';
   	else
   		sprintf( var_name, "%s", sh->type.name );

   	if ( loc_static )
   		fprintf( dest, ".DATA\n" );

	switch( rec->type.type_ref.id )
	{
	case T_CHAR:
		if ( is_static )
			fprintf( dest, "\t%s\tDB\t%d\n", var_name, rec->value_valid ? ( int )*( signed char* )rec->value : 0 );
		else
			if ( rec->value_valid )
				fprintf( dest, "\tMOV\t BYTE PTR [%s], %d\n", var_name, ( int )*( signed char* )rec->value );
			break;
	case T_UCHAR:
		if ( is_static )
			fprintf( dest, "\t%s\tDB\t%d\n", var_name, rec->value_valid ? ( int )*( unsigned char* )rec->value : 0 );
		else
			if ( rec->value_valid )
				fprintf( dest, "\tMOV\t BYTE PTR [%s], %d\n", var_name, ( int )*( unsigned char* )rec->value );
		break;
	case T_SHORT:
		if ( is_static )
			fprintf( dest, "\t%s\tDW\t%d\n", var_name, rec->value_valid ? ( int )*( short* )rec->value : 0 );
		else
	   		if ( rec->value_valid )
				fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, ( int )*( short* )rec->value );
   			break;
   	case T_USHORT:
   		if ( is_static )
   			fprintf( dest, "\t%s\tDW\t%d\n", var_name, rec->value_valid ? ( int )*( unsigned short* )rec->value : 0 );
   		else
   			if ( rec->value_valid )
   				fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, ( int )*( unsigned short* )rec->value );
   	case T_INT:
   		if ( is_static )
   			fprintf( dest, "\t%s\tDD\t%d\n", var_name, rec->value_valid ? *( int* )rec->value : 0 );
   		else
   			if ( rec->value_valid )
   				fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, *( int* )rec->value );
   		break;
print_uint:
	case T_UINT: 
		if ( is_static )
			fprintf( dest, "\t%s\tDD\t%u\n", var_name, rec->value_valid ? *( unsigned int* )rec->value : 0 );
		else
			if ( rec->value_valid )
				fprintf( dest, "\tMOV\t WORD PTR [%s], %u\n", var_name, *( unsigned int* )rec->value );
		break;
	case T_LONG:
		if ( is_static )
			fprintf( dest, "\t%s\tDD\t%ld\n", var_name, rec->value_valid ? *( long* )rec->value : 0 );
		else
			if ( rec->value_valid )
				fprintf( dest, "\tMOV\t WORD PTR [%s], %ld\n", var_name, *( long* )rec->value );
		break;
	case T_ULONG:
		if ( is_static )
			fprintf( dest, "\t%s\tDD\t%lu\n", var_name, rec->value_valid ? *( unsigned long* )rec->value : 0 );
		else
			if ( rec->value_valid )
				fprintf( dest, "\tMOV\t WORD PTR [%s], %lu\n", var_name, *( unsigned long* )rec->value );
		break;
	case T_FLOAT:
		if ( is_static )
			fprintf( dest, "\t%s\tDD\t%f\n", var_name, rec->value_valid ? ( double )*( float* )rec->value : 0.0 );
		else if ( rec->value_valid )
		{
			fprintf( dest, ".CONST\n" );
			fprintf( dest, "\t__fp_const%d\tDW\t%f\n", fp_const_count, ( double )*( float* )rec->value );
			fprintf( dest, ".CODE\n" );
			fprintf( dest, "\tFLD\tDWORD PTR [__fp_const%d]\n", fp_const_count );
			fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", var_name );
			++fp_const_count;
		}
		break;
	case T_DOUBLE:
		if ( is_static )
			fprintf( dest, "\t%s\tDQ\t%f\n", var_name, rec->value_valid ? ( double )*( float* )rec->value : 0.0 );
		else if ( rec->value_valid )
		{
			fprintf( dest, ".CONST\n" );
			fprintf( dest, "\t__fp_const%d\tDQ\t%f\n", fp_const_count, ( double )*( float* )rec->value );
			fprintf( dest, ".CODE\n" );
			fprintf( dest, "\tFLD\tQWORD PTR [__fp_const%d]\n", fp_const_count );
			fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", var_name );
			++fp_const_count;
		}
		break;
	case T_LONGDOUBLE:
		if ( is_static )
			fprintf( dest, "\t%s\tDT\t%Lf\n", var_name, rec->value_valid ? ( long double )*( float* )rec->value : 0.0 );
		else if ( rec->value_valid )
		{
			fprintf( dest, ".CONST\n" );
			fprintf( dest, "\t__fp_const%d\tDT\t%Lf\n", fp_const_count, ( long double )*( float* )rec->value );
			fprintf( dest, ".CODE\n" );
			fprintf( dest, "\tFLD\tTBYTE PTR [__fp_const%d]\n", fp_const_count );
			fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", var_name );
			++fp_const_count;
		}
		break;
	}
finish:
   	if ( loc_static )
   		fprintf( dest, ".CODE\n" );
}


static	__inline	void	generate_init_array( const record_t *arr, int control_flags )
{
	int	i;
	record_t	elem;

	if ( control_flags & PARSE_GLOBAL && !( control_flags & PARSE_MEMBER ) )
	{
		fprintf( dest, "\t%s\tLABEL BYTE\n", arr->name );
	}
	for ( i = 0; i < arr->dim[ 0 ]; ++i )
	{
		elem = *arr;
		--elem.dim_num;
		if ( elem.dim_num )
		{
			elem.dim = alloc( sizeof( *elem.dim ) * elem.dim_num );
			memmove( elem.dim, arr->dim + 1, sizeof( *elem.dim ) * elem.dim_num );
			generate_init_array( &elem, control_flags | PARSE_MEMBER );
		}
		else if ( elem.type.ptr_list != NULL )
		{
			generate_init_scalar( &elem, control_flags | PARSE_MEMBER );
		}
		else if ( elem.type.type_id == TID_STRUCT )
		{
			generate_init_struct( &elem, control_flags | PARSE_MEMBER );
		}
		else if ( elem.type.type_id == TID_UNION )
		{
			generate_init_union( &elem, control_flags | PARSE_MEMBER );
		}
		else
		{
			generate_init_scalar( &elem, control_flags | PARSE_MEMBER );
		}
	}
}


static	__inline	void	generate_init_struct( const record_t *str, int control_flags )
{
	struct_type_t	*s;
	sym_hash_t		*m;

	if ( control_flags & PARSE_GLOBAL && !( control_flags & PARSE_MEMBER ) )
	{
		fprintf( dest, "\t%s\tLABEL BYTE\n", str->name );
	}

	s = ( struct_type_t* )str->type.type_ref.type;
	for ( m = s->sym_head; m != NULL; m = m->next )
	{

		if ( m->type.dim_num != 0 )
		{
			generate_init_array( &m->type, control_flags | PARSE_MEMBER );
			continue;
		}

		if ( m->type.type.ptr_list != NULL )
		{
			generate_init_scalar( &m->type, control_flags | PARSE_MEMBER );
			continue;
		}
			//goto	print_uint;

		if ( m->type.type.type_id == TID_STRUCT )
		{
			//	Structure
			generate_init_struct( &m->type, control_flags | PARSE_MEMBER );
			continue;
		}

		if ( m->type.type.type_id == TID_UNION )
		{
			//	Union
			generate_init_union( &m->type, control_flags | PARSE_MEMBER );
			continue;
		}

		generate_init_scalar( &m->type, control_flags | PARSE_MEMBER );
	}
}


static	__inline	void	generate_init_union( const record_t *str, int control_flags )
{
	struct_type_t	*s;
	sym_hash_t		*m, *max = NULL;

	if ( control_flags & PARSE_GLOBAL && !( control_flags & PARSE_MEMBER ) )
	{
		fprintf( dest, "\t%s\tLABEL BYTE\n", str->name );
	}

	s = ( struct_type_t* )str->type.type_ref.type;
	for ( m = s->sym_head; m != NULL; m = m->next )
	{
		if ( NULL == max || do_sizeof( &m->type ) > do_sizeof( &max->type ) )
			max = m;
	}

	m = max;

   	if ( m->type.dim_num != 0 )
   	{
   		generate_init_array( &m->type, control_flags | PARSE_MEMBER );
   	}
   	else if ( m->type.type.ptr_list != NULL )
   	{
   		generate_init_scalar( &m->type, control_flags | PARSE_MEMBER );
   	}
   		//goto	print_uint;
   	else if ( m->type.type.type_id == TID_STRUCT )
   	{
   		//	Structure
   		generate_init_struct( &m->type, control_flags | PARSE_MEMBER );
   	}
   	else if ( m->type.type.type_id == TID_UNION )
   	{
   		//	Union
   		generate_init_union( &m->type, control_flags | PARSE_MEMBER );
   	}
	else
	{
	   	generate_init_scalar( &m->type, control_flags | PARSE_MEMBER );
	}
}


void	generate_init( sym_hash_t **sym_hash, int control_flags )
{
	int	i;

	int	is_static;
	int	loc_static;
	char	var_name[ 256 ];
	record_t	*rec;

	//
	//	Global symbols
	//
	for ( i = 0; i < SYM_HASH_SIZE; ++i )
	{
		for ( sh = sym_hash[ i ]; sh != NULL; sh = sh->next )
		{
			//	Global / extern specification
			if ( control_flags & PARSE_GLOBAL )
			{
				if ( ( sh->type.spec & SPEC_STATIC ) == 0 )
				{
					if ( ( sh->type.spec & SPEC_EXTERN ) != 0 )
					{
						fprintf( dest, "EXTRN\t_%s: BYTE\n", sh->type.name );
					}
					else
					{
						fprintf( dest, "PUBLIC\t%s\n", sh->type.name );
					}
				}
			}

			//is_static = ( control_flags & PARSE_GLOBAL ) || ( sh->type.spec & SPEC_STATIC );
			//loc_static = ( control_flags & PARSE_LOCAL ) && ( sh->type.spec & SPEC_STATIC );

			//if ( control_flags & PARSE_MEMBER )
			//	var_name[ 0 ] = '\0';
			//else
			//	sprintf( var_name, "%s", sh->type.name );

			//rec = lookup_symbol( var_name, SYM_HASH_SIZE, sym_hash );
			//strcpy(	var_name, rec->name );

			//if ( loc_static )
			//	fprintf( dest, ".DATA\n" );

			//
			// Array aggregate type. Generate initialization recursively for each element.
			//
			if ( sh->type.dim_num != 0 )
			{
				generate_init_array( &sh->type, control_flags );
				continue;
			}

			if ( sh->type.type.ptr_list != NULL )
			{
				generate_init_scalar( &sh->type, control_flags );
				continue;
			}
				//goto	print_uint;

			if ( sh->type.type.type_id == TID_STRUCT )
			{
				//	Structure
				generate_init_struct( &sh->type, control_flags );
				continue;
			}

			if ( sh->type.type.type_id == TID_UNION )
			{
				//	Union
				generate_init_union( &sh->type, control_flags );
				continue;
			}

			generate_init_scalar( &sh->type, control_flags );

#if 0
			switch( sh->type.type.type_ref.id )
			{
			case T_CHAR:
				if ( is_static )
					fprintf( dest, "\t%s\tDB\t%d\n", var_name, sh->type.value_valid ? ( int )*( signed char* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t BYTE PTR [%s], %d\n", var_name, ( int )*( signed char* )sh->type.value );
				break;
			case T_UCHAR:
				if ( is_static )
					fprintf( dest, "\t%s\tDB\t%d\n", var_name, sh->type.value_valid ? ( int )*( unsigned char* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t BYTE PTR [%s], %d\n", var_name, ( int )*( unsigned char* )sh->type.value );
				break;
			case T_SHORT:
				if ( is_static )
					fprintf( dest, "\t%s\tDW\t%d\n", var_name, sh->type.value_valid ? ( int )*( short* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, ( int )*( short* )sh->type.value );
				break;
			case T_USHORT:
				if ( is_static )
					fprintf( dest, "\t%s\tDW\t%d\n", var_name, sh->type.value_valid ? ( int )*( unsigned short* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, ( int )*( unsigned short* )sh->type.value );
			case T_INT:
				if ( is_static )
					fprintf( dest, "\t%s\tDD\t%d\n", var_name, sh->type.value_valid ? *( int* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %d\n", var_name, *( int* )sh->type.value );
				break;
print_uint:
			case T_UINT: 
				if ( is_static )
					fprintf( dest, "\t%s\tDD\t%u\n", var_name, sh->type.value_valid ? *( unsigned int* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %u\n", var_name, *( unsigned int* )sh->type.value );
				break;
			case T_LONG:
				if ( is_static )
					fprintf( dest, "\t%s\tDD\t%ld\n", var_name, sh->type.value_valid ? *( long* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %ld\n", var_name, *( long* )sh->type.value );
				break;
			case T_ULONG:
				if ( is_static )
					fprintf( dest, "\t%s\tDD\t%lu\n", var_name, sh->type.value_valid ? *( unsigned long* )sh->type.value : 0 );
				else
					if ( sh->type.value_valid )
						fprintf( dest, "\tMOV\t WORD PTR [%s], %lu\n", var_name, *( unsigned long* )sh->type.value );
				break;
			case T_FLOAT:
				if ( is_static )
					fprintf( dest, "\t%s\tDD\t%f\n", var_name, sh->type.value_valid ? ( double )*( float* )sh->type.value : 0.0 );
				else if ( sh->type.value_valid )
				{
					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDW\t%f\n", fp_const_count, ( double )*( float* )sh->type.value );
					fprintf( dest, ".CODE\n" );
					fprintf( dest, "\tFLD\tDWORD PTR [__fp_const%d]\n", fp_const_count );
					fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", var_name );
					++fp_const_count;
				}
				break;
			case T_DOUBLE:
				if ( is_static )
					fprintf( dest, "\t%s\tDQ\t%f\n", var_name, sh->type.value_valid ? ( double )*( float* )sh->type.value : 0.0 );
				else if ( sh->type.value_valid )
				{
					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDQ\t%f\n", fp_const_count, ( double )*( float* )sh->type.value );
					fprintf( dest, ".CODE\n" );
					fprintf( dest, "\tFLD\tQWORD PTR [__fp_const%d]\n", fp_const_count );
					fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", var_name );
					++fp_const_count;
				}
				break;
			case T_LONGDOUBLE:
				if ( is_static )
					fprintf( dest, "\t%s\tDT\t%Lf\n", var_name, sh->type.value_valid ? ( long double )*( float* )sh->type.value : 0.0 );
				else if ( sh->type.value_valid )
				{
					fprintf( dest, ".CONST\n" );
					fprintf( dest, "\t__fp_const%d\tDT\t%Lf\n", fp_const_count, ( long double )*( float* )sh->type.value );
					fprintf( dest, ".CODE\n" );
					fprintf( dest, "\tFLD\tTBYTE PTR [__fp_const%d]\n", fp_const_count );
					fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", var_name );
					++fp_const_count;
				}
				break;
			}
finish:
			if ( loc_static )
				fprintf( dest, ".CODE\n" );
#endif
		}
	}
}

 
void	generate_asm( char *src_file, char *dest_file )
{
	int		i, j;
	int		len;
	int		cnt;
	int		rv1, rv2;
	char	dest1_opnd[ 256 ], dest2_opnd[ 256 ];
	int		reg1_num, reg2_num;
	char	size_spec[ 256 ];
	int		adjust_esp;
	char	t_dest[ 256 ], t_src[ 256 ];
	unsigned	sz_dest, sz_src;


	src = fopen( src_file, "rt" );
	dest = fopen( dest_file, "wt" );

	//
	//	Output standard ASM header
	//
	fprintf( dest,	";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
					";;\n"
					";; %s\n"
					";;\n"
					";; This file was generated by the BPC compiler.\n"
					";;\n"
					";; Must be compiled with MASM 6.1x\n"
					";;\n"
					";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n", dest_file );

	//
	//	Model, CPU definitions
	//
	fprintf( dest, ".386p\n"
					".MODEL\tFLAT\n"
					".DATA\n" );

	//
	//	Reserved symbols
	//

	// Temp address for calculating structure member addresses.
	fprintf( dest, "\t__temp_addr\tDD\t0\n" );
	// Temp buf for holding non-aggregate return values.
	fprintf( dest, "\t__ret_buf\tDT\t?\n" );

	// __temp_intNN for NN >= 6 and __temp_fpNN
	for ( i = 6; i <= max_temp_level_int; ++i )
		fprintf( dest, "\t__temp_int%d\tDD\t?\n", i );
	for ( i = 0; i <= max_temp_level_fp; ++i )
		fprintf( dest, "\t__temp_fp%d\tDD\t?\n", i );


	generate_init( sym_hash, PARSE_GLOBAL );

	//
	//	String literals
	//
	fprintf( dest, ".CONST\n" );
	for ( i = 0; i < LIT_HASH_SIZE; ++i )
	{
		char	buf[ 256 ];
		int	k;
		unsigned	value;

		for ( sh = lit_hash[ i ]; sh != NULL; sh = sh->next )
		{
			fprintf( dest, "\t__lit%d\tDB\t", *( unsigned* )sh->type.value );
			for ( j = 0, len = strlen( sh->type.name ); j < len; ++j )
			{
				if ( sh->type.name[ j ] == '\\' )
				{
					++j;
					if ( isoct( sh->type.name[ j ] ) )
					{
						for ( k = 0; isoct( sh->type.name[ j ] ); ++j, ++k )
							buf[ k ] = sh->type.name[ j ];
						buf[ k ] = '\0';
						value = sscanf( buf, "%o", &value );
						fprintf( dest, "0%02XH, ", value );
						--j;
					}
					else if ( sh->type.name[ j ] == 'x' )
					{
						++j;
						for ( k = 0; ishex( sh->type.name[ j ] ); ++j, ++k )
							buf[ k ] = sh->type.name[ j ];
						buf[ k ] = '\0';
						value = sscanf( buf, "%x", &value );
						fprintf( dest, "0%02XH, ", value );
						--j;
					}
					else /*if ( sh->type.name[ j ] == '\'' || sh->type.name[ j ] == '\?' || 
						sh->type.name[ j ] == '\"' || sh->type.name[ j ] == 'a' ||
						sh->type.name[ j ] == 'b' || sh->type.name[ j ] == 'f' || 
						sh->type.name[ j ] == 'n' || sh->type.name[ j ] == 'r' || 
						sh->type.name[ j ] == 't' || sh->type.name[ j ] == 'v' )*/
					{
						switch ( sh->type.name[ j ] )
						{
						case '\\':
							fprintf( dest, "0%02XH, ", '\\' );
							break;
						case '?':
							fprintf( dest, "0%02XH, ", '\?' );
							break;
						case '"':
							fprintf( dest, "0%02XH, ", '\"' );
							break;
						case 'a':
							fprintf( dest, "0%02XH, ", '\a' );
							break;
						case 'b':
							fprintf( dest, "0%02XH, ", '\b' );
							break;
						case 'f':
							fprintf( dest, "0%02XH, ", '\f' );
							break;
						case 'n':
							fprintf( dest, "0%02XH, ", '\n' );
							break;
						case 'r':
							fprintf( dest, "0%02XH, ", '\r' );
							break;
						case 't':
							fprintf( dest, "0%02XH, ", '\t' );
							break;
						case 'v':
							fprintf( dest, "0%02XH, ", '\v' );
							break;
						}
						//++j;
					}
				}
				else
				{
					fprintf( dest, "0%02XH, ", sh->type.name[ j ] );
				}
			}
			fprintf( dest, "00H\n" );
		}
	}

	//
	//	Not defined functions.
	//
	for ( i = 0; i < FUNC_HASH_SIZE; ++i )
	{
		for ( sh = func_hash[ i ]; sh != NULL; sh = sh->next )
		{
			if ( ( ( function_type_t* )sh->type.type.type_ref.type )->defined == 0 )
			{
				//if ( sh->type.name[ 0 ] != '_' )
					fprintf( dest, "EXTRN\t_%s : NEAR\n", sh->type.name );
				//else
				//	fprintf( dest, "EXTRN\t%s : NEAR\n", sh->type.name );
			}
			else
			{
				if ( ( sh->type.spec & SPEC_STATIC ) == 0 )
					fprintf( dest, "PUBLIC\t_%s\n", sh->type.name );
			}
		}
	}

	//
	//	Code
	//
	fprintf( dest, ".CODE\n" );

	while ( !feof( src ) )
	{
		char	dest_opnd[ 256 ], src1_opnd[ 256 ], src2_opnd[ 256 ], opr[ 256 ]; 
		int		n;

		if ( fgets( line, 1023, src ) != line )
			break;

		// Will change if __ret_val* is used
		adjust_esp = 0;

		if ( strchr( line, ':' ) )
		{
			// Label
			fputs( line, dest );
			continue;
		}
		if ( strncmp( line, "__call", sizeof( "__call" ) - 1 ) == 0 )
		{
			// Label
			if ( sscanf( "__call * %s", src1_opnd ) == 1 )
			{
				fprintf( dest, "\tCALL\tDWORD PTR [%s]\n", src1_opnd );
			}
			else
			{
				sscanf( line, "__call %s", src1_opnd );
				fprintf( dest, "\tCALL\t%s\n", src1_opnd );
			}
			continue;
		}
		else if ( strncmp( line, "__return", sizeof( "__return" ) - 1 ) == 0 )
		{
			fprintf( dest, "\tRET\n" );
			continue;
		}
		else if ( strncmp( line, "__asm", sizeof( "__asm" ) - 1 ) == 0 )
		{
			do
			{
				if ( fgets( line, 1023, src ) != line )
					break;
				if ( strncmp( line, "__endasm", sizeof( "__endasm" ) - 1 ) == 0 )
					break;
				// fgets() keeps the newline.
				fprintf( dest, "%s", line );
			} while ( 1 );
		}
		else if ( strncmp( line, "__push_arg", sizeof( "__push_arg" ) - 1 ) == 0 )
		{
			int	reg_num;

			if ( sscanf( line, "__push_arg __offset %s %d", src1_opnd, &n ) == 2 )
			{
				fprintf( dest, "\tPUSH\tOFFSET %s\n", src1_opnd );
			}
			else
			{
				sscanf( line, "__push_arg %s %d", src1_opnd, &n );

				if ( src1_opnd[ 0 ] == '#' )
				{
					if ( sscanf( src1_opnd + 1, "__temp_int%d", &reg_num ) == 1 && reg_num >= 0 && reg_num < 6 )
					{
						sprintf( src1_opnd, "%s", int_reg_name[ reg_num ] );
						goto	push_operand;
					}
					else
					{
						fprintf( dest, "\tMOV\tDWORD PTR [__ret_buf], EAX\n" );
						fprintf( dest, "\tMOV\tEAX, DWORD PTR [%s]\n", src1_opnd );
						if ( n == 4 )
							fprintf( dest, "\tPUSH\tDWORD PTR [EAX]\n" );
						else
						{
							i = ( n & 3 ) == 0 ? n : ( n & ~3 ) + 4;

							for ( i = i - 4; i >= 0; i -= 4 )
								fprintf( dest, "\tPUSH\tDWORD PTR [EAX+%d]\n", i );
						}
						fprintf( dest, "\tMOV\tEAX, DWORD PTR [__ret_buf]\n" );
					}
				}
				else if ( strncmp( src1_opnd, "__temp_int", sizeof( "__temp_int" ) - 1 ) == 0 )
				{
					if ( sscanf( src1_opnd, "__temp_int%d", &reg_num ) == 1 && reg_num >= 0 && reg_num < 6 )
						fprintf( dest, "\tPUSH\t%s\n", int_reg_name[ reg_num ] );
					else
						fprintf( dest, "\tPUSH\tDWORD PTR [%s]\n", src1_opnd );
				}
				else
				{
push_operand:
					if ( n == 4 )
						fprintf( dest, "\tPUSH\tDWORD PTR [%s]\n", src1_opnd );
					else
					{
						i = ( n & 3 ) == 0 ? n : ( n & ~3 ) + 4;

						for ( i = i - 4; i >= 0; i -= 4 )
							fprintf( dest, "\tPUSH\tDWORD PTR [%s+%d]\n", src1_opnd, i );
					}
				}
			}
		}
		else if ( strncmp( line, "__value", sizeof( "__value" ) - 1 ) == 0 )
		{
			continue;
		}
		else if ( strncmp( line, "if", sizeof( "if" ) - 1 ) == 0 )
		{
			int	int_size;
			int	fp_size;

			if ( sscanf( line, "if ( !%s [ INT%d ] ) goto %s", src1_opnd, &int_size, src2_opnd ) == 3 )
			{
				generate_ali( src1_opnd, src1_opnd, "0", "CMP", strstr( line, "INT" ) );
				fprintf( dest, "\tJE\t%s\n", src2_opnd );
			}
			else if ( sscanf( line, "if ( %s [ INT%d ] ) goto %s", src1_opnd, &int_size, src2_opnd ) == 3 )
			{
				generate_ali( src1_opnd, src1_opnd, "0", "CMP", strstr( line, "INT" ) );
				fprintf( dest, "\tJNE\t%s\n", src2_opnd );
			}
			else if ( sscanf( line, "if ( !%s [ UINT%d ] ) goto %s", src1_opnd, &int_size, src2_opnd ) == 3 )
			{
				generate_ali( src1_opnd, src1_opnd, "0", "CMP", strstr( line, "UINT" ) );
				fprintf( dest, "\tJE\t%s\n", src2_opnd );
			}
			else if ( sscanf( line, "if ( %s [ UINT%d ] ) goto %s", src1_opnd, &int_size, src2_opnd ) == 3 )
			{
				generate_ali( src1_opnd, src1_opnd, "0", "CMP", strstr( line, "UINT" ) );
				fprintf( dest, "\tJNE\t%s\n", src2_opnd );
			}
			else if ( sscanf( line, "if ( !%s [ FP%d ] ) goto %s", src1_opnd, &fp_size, src2_opnd ) == 3 )
			{
				// FCOMI with 0.
				fprintf( dest, "\tFLDZ\n" );
				fprintf( dest, "\tFLD\t%s PTR [ %s ]\n", fp_size == 4 ? "DWORD" : fp_size == 8 ? "QWORD" : "TBYTE", src1_opnd );
				fprintf( dest, "\tFCOMP\tST( 1 )\n" );
				fprintf( dest, "\tFSTP\tST( 0 )\n" );
				fprintf( dest, "\tFSTSW\tAX\n" );
				fprintf( dest, "\tSAHF\n" );
				fprintf( dest, "\tJE\t%s\n", src2_opnd );
			}
			else if ( sscanf( line, "if ( %s [ FP%d ] ) goto %s", src1_opnd, &fp_size, src2_opnd ) == 3 )
			{
				// FCOMI with 0.
				fprintf( dest, "\tFLDZ\n" );
				fprintf( dest, "\tFLD\t%s PTR [ %s ]\n", fp_size == 4 ? "DWORD" : fp_size == 8 ? "QWORD" : "TBYTE", src1_opnd );
				fprintf( dest, "\tFCOMP\tST( 1 )\n" );
				fprintf( dest, "\tFSTP\tST( 0 )\n" );
				fprintf( dest, "\tFSTSW\tAX\n" );
				fprintf( dest, "\tSAHF\n" );
				fprintf( dest, "\tJNE\t%s\n", src2_opnd );
			}
			continue;
		}
		else if ( strncmp( line, "goto", sizeof( "goto" ) - 1 ) == 0 )
		{
			sscanf ( line, "goto %s", src1_opnd );
			fprintf( dest, "\tJMP\t%s\n", src1_opnd );
		}
		else if ( strncmp( line, "__switch", sizeof( "__switch" ) - 1 ) == 0 )
		{
			// Switch: here must come code designated by "__endswitch"
		}
		else if ( sscanf( line, " %s <- %s %s %s [ %s ]", dest_opnd, src1_opnd, opr, src2_opnd, size_spec ) == 5 )
		{
			int	rv1, rv2;
			char	dest1_opnd[ 256 ], dest2_opnd[ 256 ];

			// Binary operation
			if ( opr[ 0 ] == '=' )
			{
				if ( opr[ 1 ] != '=' )
				{
					// Assignment
					generate_mov( src1_opnd, src2_opnd, size_spec );
					generate_mov( dest_opnd, src1_opnd, size_spec );
				}
				else
				{
					// == operator
					generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
					generate_mov( dest_opnd, "1", size_spec );
					fprintf( dest, "\tJE\t__asm_label%d\n", asm_label_count );
					generate_mov( dest_opnd, "0", size_spec );
					fprintf( dest, "__asm_label%d:\n", asm_label_count );
					++asm_label_count;
				}
			}
			else
			{
				int	sz_spec;

				switch( opr[ 0 ] )
				{

				case '!':
					if ( opr[ 1 ] == '=' )
					{
						// != operator
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\tJNE\t__asm_label%d\n", asm_label_count );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					break;

				case '*':
					if ( size_spec[ 0 ] == 'F' )
						goto	fp_mul;

					//
					// dest is not EAX
					//
					if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
					{
						//
						// FP multiplication
						//
						if ( strcmp( size_spec, "FP4" ) == 0 )
						{
							fprintf( dest, "\tFLD\tDWORD PTR [%s]\n", src1_opnd );
							fprintf( dest, "\tFMUL\tDWORD PTR [%s]\n", src2_opnd );
							fprintf( dest, "\tFSTP\tDWORD PTR [%s]\n", dest_opnd );
						}
						else if ( strcmp( size_spec, "FP8" ) == 0 )
						{
							fprintf( dest, "\tFLD\tQWORD PTR [%s]\n", src1_opnd );
							fprintf( dest, "\tFMUL\tQWORD PTR [%s]\n", src2_opnd );
							fprintf( dest, "\tFSTP\tQWORD PTR [%s]\n", dest_opnd );
						}
						else if ( strcmp( size_spec, "FP10" ) == 0 )
						{
							fprintf( dest, "\tFLD\tTBYTE PTR [%s]\n", src1_opnd );
							fprintf( dest, "\tFMUL\tTBYTE PTR [%s]\n", src2_opnd );
							fprintf( dest, "\tFSTP\tTBYTE PTR [%s]\n", dest_opnd );
						}

						//
						// INT multiplication -- src1 is EAX
						//
						else if ( strcmp( src1_opnd, "__temp_int0" ) == 0 )
						{
							if ( sscanf( size_spec, "INT%d", &sz_spec ) != 1 )
								sscanf( size_spec, "UINT%d", &sz_spec );

							fprintf( dest, "\tPUSH\tEDX\n" );

							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
									src2_opnd );
							}

							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
								{
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								}
								else
								{
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
										sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
										dest_opnd,
										sz_spec == 4 ? "EAX" : sz_spec == 2 ? "AX" : "AL" );
								}
							}
							if ( strcmp( dest_opnd, "__temp_int3" ) != 0 )
								fprintf( dest, "\tPOP\tEDX\n" );
							else
								fprintf( dest, "\tADD\tESP, 4\n" );
						}

						//
						// INT multiplication -- src2 is EAX
						//
						else if ( strcmp( src2_opnd, "__temp_int0" ) == 0 )
						{
							if ( sscanf( size_spec, "INT%d", &sz_spec ) != 1 )
								sscanf( size_spec, "UINT%d", &sz_spec );

							fprintf( dest, "\tPUSH\tEDX\n" );

							if ( isnum( src1_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src1_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
									src1_opnd );
							}
							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
								{
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								}
								else
								{
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
										sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
										dest_opnd,
										sz_spec == 4 ? "EAX" : sz_spec == 2 ? "AX" : "AL" );
								}
							}
							if ( strcmp( dest_opnd, "__temp_int3" ) != 0 )
								fprintf( dest, "\tPOP\tEDX\n" );
							else
								fprintf( dest, "\tADD\tESP, 4\n" );
						}

						//
						// INT multiplication -- neither dest, nor src1 not src2 is EAX
						//
						else
						{
							if ( sscanf( size_spec, "INT%d", &sz_spec ) != 1 )
								sscanf( size_spec, "UINT%d", &sz_spec );

							fprintf( dest, "\tPUSH\tEAX\n" );
							fprintf( dest, "\tPUSH\tEDX\n" );

							if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
							}
							else
							{
								fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
									sz_spec == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
									src1_opnd );
							}

							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
									sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
									src2_opnd );
							}

							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
								{
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								}
								else
								{
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
										sz_spec == 4 ? "DWORD" : sz_spec == 2 ? "WORD" : "BYTE",
										dest_opnd,
										sz_spec == 4 ? "EAX" : sz_spec == 2 ? "AX" : "AL" );
								}
							}
							if ( strcmp( dest_opnd, "__temp_int3" ) != 0 )
								fprintf( dest, "\tPOP\tEDX\n" );
							else
								fprintf( dest, "\tADD\tESP, 4\n" );
							fprintf( dest, "\tPOP\tEAX\n" );
						}
					}

					//
					// MUL - integer, dest is EAX.
					//
					else if ( strncmp( size_spec, "FP", sizeof( "FP" ) - 1 ) != 0 )
					{
						int	int_size;

						if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
							sscanf( size_spec, "UINT%d", &int_size );

						fprintf( dest, "\tPUSH\tEAX\n" );
						fprintf( dest, "\tPUSH\tEDX\n" );

						fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
							int_size == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
							int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src1_opnd );

						if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
						{
							fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
							fprintf( dest, "\t%s %s PTR [ESP]\n",
								size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ) );
							fprintf( dest, "\tADD\tESP, 4\n" );
						}
						else
						{
							fprintf( dest, "\t%s %s PTR [%s]\n",
								size_spec[ 0 ] == 'I' ? "IMUL" : "MUL",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd );
						}

						fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
							int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd,
							int_size == 4 ? "EAX" : ( int_size == 2 ? "AX" : "AL" ) );

						fprintf( dest, "\tPOP\tEDX\n" );
						fprintf( dest, "\tPOP\tEAX\n" );
					}

					//
					// MUL - FP, dest is EAX (is at all possible???)
					//
					else
					{
						// FP
						int	fp_size;

fp_mul:
						sscanf( size_spec, "FP%d", &fp_size );

						if ( isnum( src1_opnd[ 0 ] ) )
						{
							fprintf( dest, ".CONST\n" );
							fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
								fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
								src1_opnd );
							fprintf( dest, ".CODE\n" );
							sprintf( src1_opnd, "__fp_const%d", fp_const_count );
							++fp_const_count;
						}
						if ( isnum( src2_opnd[ 0 ] ) )
						{
							fprintf( dest, ".CONST\n" );
							fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
								fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
								src2_opnd );
							fprintf( dest, ".CODE\n" );
							sprintf( src2_opnd, "__fp_const%d", fp_const_count );
							++fp_const_count;
						}

						fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src1_opnd );
						fprintf( dest, "\tFMUL\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src2_opnd );
						fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), dest_opnd );
					}
					break;

				case '/':
					if ( size_spec[ 0 ] == 'F' )
						goto	fp_div;

					//
					// dest is not EAX
					//
					if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
					{
						int	int_size;
						int	fp_size;

						//
						// DIV -- src1 is EAX
						//
						if ( strcmp( src1_opnd, "__temp_int0" ) == 0 )
						{

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEDX\n" );
							fprintf( dest, "\tSUB\tEDX, EDX\n" );

							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
									src2_opnd );
							}
							fprintf( dest, "\tPOP\tEDX\n" );
						}
						else if ( strcmp( src2_opnd, "__temp_int0" ) == 0 )
						{
							int	int_size;

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEDX\n" );
							fprintf( dest, "\tSUB\tEDX, EDX\n" );
							fprintf( dest, "\tPUSH\tEAX\n" );
							if ( sscanf( src1_opnd, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
							{
								fprintf( dest, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
							}
							else
							{
								if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
								{
									fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
								}
								else
								{
									fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n",
										int_size == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										src1_opnd );
								}
							}
							fprintf( dest, "\t%s\tDWORD PTR [ESP]\n", size_spec[ 0 ] == 'I' ? "IDIV" : "DIV" );
							fprintf( dest, "\tADD\tESP, 4\n" );
							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
								{
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								}
								else
								{
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n", 
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										dest_opnd,
										int_size == 4 ? "EAX" : int_size == 2 ? "AX" : "AL" );
								}
							}
							fprintf( dest, "\tPOP\tEDX\n" );
						}

						else
						{
							int	int_size;

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEAX\n" );
							fprintf( dest, "\tPUSH\tEDX\n" );

							fprintf( dest, "\tSUB\tEDX, EDX\n" );

							if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
							}
							else
							{
								fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
									int_size == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
									int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src1_opnd );
							}
							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE", 
									src2_opnd );
							}
							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								else
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n", 
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										dest_opnd,
										int_size == 4 ? "EAX" : int_size == 2 ? "AX" : "AL" );
							}
							fprintf( dest, "\tPOP\tEDX\n" );
							fprintf( dest, "\tPOP\tEAX\n" );
						}
					}
					else if ( strncmp( size_spec, "FP", sizeof( "FP" ) - 1 ) != 0 )
					{
						int	int_size;

						if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
							sscanf( size_spec, "UINT%d", &int_size );

						fprintf( dest, "\tPUSH\tEAX\n" );
						fprintf( dest, "\tPUSH\tEDX\n" );

						if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
						{
							fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
						}
						else
						{
							fprintf( dest, "\t%s\tEAX, %s [%s]\n", int_size == 4 ? "MOV" : "MOVZX",
								int_size == 4 ? "DWORD PTR" : ( int_size == 2 ? "WORD PTR" : "BYTE PTR" ), src1_opnd );
						}

						if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
						{
							fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
							fprintf( dest, "\t%s %s PTR [ESP]\n",
								size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ) );
							fprintf( dest, "\tADD\tESP, 4\n" );
						}
						else
						{
							fprintf( dest, "\t%s %s PTR [%s]\n", 
								size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd );
						}
						fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
							int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd,
							int_size == 4 ? "EAX" : ( int_size == 2 ? "AX" : "AL" ) );

						fprintf( dest, "\tPOP\tEDX\n" );
						fprintf( dest, "\tPOP\tEAX\n" );
					}
					else
					{
						// FP
						int	fp_size;

fp_div:
						sscanf( size_spec, "FP%d", &fp_size );

						if ( isnum( src1_opnd[ 0 ] ) )
						{
							fprintf( dest, ".CONST\n" );
							fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
								fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
								src1_opnd );
							fprintf( dest, ".CODE\n" );
							sprintf( src1_opnd, "__fp_const%d", fp_const_count );
							++fp_const_count;
						}
						if ( isnum( src2_opnd[ 0 ] ) )
						{
							fprintf( dest, ".CONST\n" );
							fprintf( dest, "\t__fp_const%d\t%s\t%s\n",
								fp_const_count, fp_size == 4 ? "DD" : fp_size == 8 ? "DQ" : "DT",
								src2_opnd );
							fprintf( dest, ".CODE\n" );
							sprintf( src2_opnd, "__fp_const%d", fp_const_count );
							++fp_const_count;
						}

						fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src1_opnd );
						fprintf( dest, "\tFDIV\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src2_opnd );
						fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), dest_opnd );
					}
					break;

				case '%':
					//
					// dest is not EAX
					//
					if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
					{
						int	int_size;
						int	fp_size;

						//
						// DIV -- src1 is EAX
						//
						if ( strcmp( src1_opnd, "__temp_int0" ) == 0 )
						{

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEDX\n" );
							fprintf( dest, "\tSUB\tEDX, EDX\n" );

							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
									src2_opnd );
							}
							fprintf( dest, "\tPOP\tEDX\n" );
						}
						else if ( strcmp( src2_opnd, "__temp_int0" ) == 0 )
						{
							int	int_size;

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEDX\n" );
							fprintf( dest, "\tSUB\tEDX, EDX\n" );
							fprintf( dest, "\tPUSH\tEAX\n" );
							if ( sscanf( src1_opnd, "__temp_int%d", &n ) == 1 && n >= 0 && n < 6 )
							{
								fprintf( dest, "\tMOV\tEAX, %s\n", int_reg_name[ n ] );
							}
							else
							{
								if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
								{
									fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
								}
								else
								{
									fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n",
										int_size == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										src1_opnd );
								}
							}
							fprintf( dest, "\t%s\tDWORD PTR [ESP]\n", size_spec[ 0 ] == 'I' ? "IDIV" : "DIV" );
							fprintf( dest, "\tADD\tESP, 4\n" );
							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
								{
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								}
								else
								{
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n", 
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										dest_opnd,
										int_size == 4 ? "EDX" : int_size == 2 ? "DX" : "AH" );
								}
							}
							fprintf( dest, "\tPOP\tEDX\n" );
						}

						else
						{
							int	int_size;

							if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
								sscanf( size_spec, "UINT%d", &int_size );

							fprintf( dest, "\tPUSH\tEAX\n" );
							fprintf( dest, "\tPUSH\tEDX\n" );

							fprintf( dest, "\tSUB\tEDX, EDX\n" );

							if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
							}
							else
							{
								fprintf( dest, "\t%s\tEAX, %s PTR [%s]\n", 
									int_size == 4 ? "MOV" : size_spec[ 0 ] == 'I' ? "MOVSX" : "MOVZX",
									int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src1_opnd );
							}
							if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
							{
								fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
								fprintf( dest, "\t%s\t%s PTR [ESP]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE" );
								fprintf( dest, "\tADD\tESP, 4\n" );
							}
							else
							{
								fprintf( dest, "\t%s\t%s PTR [%s]\n",
									size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
									int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE", 
									src2_opnd );
							}
							if ( strcmp( dest_opnd, "__temp_int0" ) != 0 )
							{
								if ( sscanf( dest_opnd, "__temp_int%d", &n ) == 1 && n > 0 && n < 6 )
									fprintf( dest, "\tMOV\t%s, EAX\n", int_reg_name[ n ] );
								else
									fprintf( dest, "\tMOV\t%s PTR [%s], %s\n", 
										int_size == 4 ? "DWORD" : int_size == 2 ? "WORD" : "BYTE",
										dest_opnd,
										int_size == 4 ? "EDX" : int_size == 2 ? "DX" : "AH" );
							}
							fprintf( dest, "\tPOP\tEDX\n" );
							fprintf( dest, "\tPOP\tEAX\n" );
						}
					}
					else if ( strncmp( size_spec, "FP", sizeof( "FP" ) - 1 ) != 0 )
					{
						int	int_size;

						if ( sscanf( size_spec, "INT%d", &int_size ) != 1 )
							sscanf( size_spec, "UINT%d", &int_size );

						fprintf( dest, "\tPUSH\tEAX\n" );
						fprintf( dest, "\tPUSH\tEDX\n" );

						if ( isnum( src1_opnd[ 0 ] ) || src1_opnd[ 0 ] == '-' )
						{
							fprintf( dest, "\tMOV\tEAX, %s\n", src1_opnd );
						}
						else
						{
							fprintf( dest, "\t%s\tEAX, %s [%s]\n", int_size == 4 ? "MOV" : "MOVZX",
								int_size == 4 ? "DWORD PTR" : ( int_size == 2 ? "WORD PTR" : "BYTE PTR" ), src1_opnd );
						}

						if ( isnum( src2_opnd[ 0 ] ) || src2_opnd[ 0 ] == '-' )
						{
							fprintf( dest, "\tPUSH\t%s\n", src2_opnd );
							fprintf( dest, "\t%s %s PTR [ESP]\n",
								size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ) );
							fprintf( dest, "\tADD\tESP, 4\n" );
						}
						else
						{
							fprintf( dest, "\t%s %s PTR [%s]\n", 
								size_spec[ 0 ] == 'I' ? "IDIV" : "DIV",
								int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd );
						}
						fprintf( dest, "\tMOV\t%s PTR [%s], %s\n",
							int_size == 4 ? "DWORD" : ( int_size == 2 ? "WORD" : "BYTE" ), src2_opnd,
							int_size == 4 ? "EDX" : ( int_size == 2 ? "DX" : "AH" ) );

						fprintf( dest, "\tPOP\tEDX\n" );
						fprintf( dest, "\tPOP\tEAX\n" );
					}
					else
					{
						// FP
						int	fp_size;

						sscanf( size_spec, "FP%d", &fp_size );
						fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src1_opnd );
						fprintf( dest, "\tFDIV\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), src2_opnd );
						fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" :
							( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ), dest_opnd );
					}
					break;

				case '+':
					generate_ali( dest_opnd, src1_opnd, src2_opnd, "ADD", size_spec );
					break;

				case '-':
					generate_ali( dest_opnd, src1_opnd, src2_opnd, "SUB", size_spec );
					break;

				case '<':
					if ( opr[ 1 ] == '<' )
					{
						// << operator
						if ( sscanf( src2_opnd, "%d", &cnt ) == 1 )
						{
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHL\t%s, %d\n", dest_opnd, cnt );
						}
						else if ( strcmp( src2_opnd, "__temp_int3" ) != 0 )
						{
							fprintf( dest, "\tPUSH\tECX\n" );
							fprintf( dest, "\tMOV\tECX, %s\n", src2_opnd );
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHL\t%s, ECX\n", dest_opnd );
							fprintf( dest, "\tPOP\tECX\n" );
						}
						else
						{
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHL\t%s, ECX\n", dest_opnd );
						}
					}
					else if ( opr[ 1 ] == '=' )
					{
						// <= operator
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\t%s\t__asm_label%d\n", 
							size_spec[ 0 ] == 'I' ? "JLE" : "JBE", 
							asm_label_count );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					else
					{
						// < operator
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\t%s\t__asm_label%d\n", 
							size_spec[ 0 ] == 'I' ? "JL" : "JB", 
							asm_label_count );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					break;

				case '>':
					if ( opr[ 1 ] == '>' )
					{
						// >> operator
						if ( sscanf( src2_opnd, "%d", &cnt ) )
						{
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHR\t%s, %d\n", dest_opnd, cnt );
						}
						else if ( strcmp( src2_opnd, "__temp_int3" ) != 0 )
						{
							fprintf( dest, "\tPUSH\tECX\n" );
							fprintf( dest, "\tMOV\tECX, %s\n", src2_opnd );
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHR\t%s, ECX\n", dest_opnd );
							fprintf( dest, "\tPOP\tECX\n" );
						}
						else
						{
							fprintf( dest, "\tMOV\t%s, %s\n", dest_opnd, src1_opnd );
							fprintf( dest, "\tSHR\t%s, ECX\n", dest_opnd );
						}
					}
					else if ( opr[ 1 ] == '=' )
					{
						// >= operator
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\t%s\t__asm_label%d\n", 
							size_spec[ 0 ] == 'I' ? "JGE" : "JAE", 
							asm_label_count );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					else
					{
						// > operator
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "CMP", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\t%s\t__asm_label%d\n", 
							size_spec[ 0 ] == 'I' ? "JG" : "JA", 
							asm_label_count );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					break;

				case '&':
					if ( opr[ 1 ] != '&' )
					{
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "AND", size_spec );
					}
					else
					{
						// && operator
						generate_ali( dest_opnd, src1_opnd, src1_opnd, "TEST", size_spec );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "\tJZ\t__asm_label%d\n", asm_label_count );

						generate_ali( dest_opnd, src2_opnd, src2_opnd, "TEST", size_spec );
						generate_mov( dest_opnd, "0", size_spec );
						fprintf( dest, "\tJZ\t__asm_label%d\n", asm_label_count );

						generate_mov( dest_opnd, "1", size_spec );

						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					break;

				case '^':
					generate_ali( dest_opnd, src1_opnd, src2_opnd, "XOR", size_spec );
					break;
				case '|':
					if ( opr[ 1 ] != '|' )
					{
						generate_ali( dest_opnd, src1_opnd, src2_opnd, "OR", size_spec );
					}
					else
					{
						// || operator
						generate_ali( dest_opnd, src1_opnd, src1_opnd, "TEST", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\tJNZ\t__asm_label%d\n", asm_label_count );

						generate_ali( dest_opnd, src2_opnd, src2_opnd, "TEST", size_spec );
						generate_mov( dest_opnd, "1", size_spec );
						fprintf( dest, "\tJNZ\t__asm_label%d\n", asm_label_count );

						generate_mov( dest_opnd, "0", size_spec );

						fprintf( dest, "__asm_label%d:\n", asm_label_count );
						++asm_label_count;
					}
					break;
				}
			}
		}
		else if ( sscanf( line, " %s <- %s %s [ %s ] ", dest_opnd, opr, src1_opnd, size_spec ) == 4 )
		{
			// Unary operation
			if ( strcmp( opr, "*" ) == 0 )
			{
#if 1
				if ( sscanf( src1_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
				{
					if ( sscanf( dest_opnd, "__temp_int%d", &reg2_num ) == 1 && reg2_num >= 0 && reg2_num < 6 )
					{
						fprintf( dest, "\tMOV\t%s, [%s]\n", int_reg_name[ reg2_num ], int_reg_name[ reg1_num ] );
					}
					else
					{
						fprintf( dest, "\tPUSH\tDWORD PTR [%s]\n"
										"\tPOP\tDWORD PTR [%s]\n", int_reg_name[ reg1_num ], dest_opnd );
					}
				}
				else
				{
					if ( sscanf( dest_opnd, "__temp_int%d", &reg2_num ) == 1 && reg2_num >= 0 && reg2_num < 6 )
					{
						fprintf( dest, "\tMOV\t%s, [%s]\n"
										"\tMOV\t%s, [%s]\n", 
							int_reg_name[ reg2_num ], src1_opnd, int_reg_name[ reg2_num ], int_reg_name[ reg2_num ] );
					}
					else
					{
						fprintf( dest, "\tPUSH\tEAX\n"
										"\tMOV\tEAX, %s\n"
										"\tMOV\tEAX, [EAX]\n"
										"\tMOV\t%s, EAX\n"
										"\tPOP\tEAX\n", src1_opnd, dest_opnd );
					}
				}
#else
				generate_mov( dest_opnd, src1_opnd, size_spec );
#endif
			}
			else if ( strcmp( opr, "++" ) == 0 )
			{
				if ( strncmp( size_spec, "INT", sizeof( "INT" ) - 1 ) || strncmp( size_spec, "UINT", sizeof( "UINT" ) - 1 ) )
				{
					int	sz;

					if ( sscanf( size_spec, "INT%d", &sz ) )
						sscanf( size_spec, "UINT%d", &sz );

					if ( sscanf( src1_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
						fprintf( dest, "\tINC\t%s\n", int_reg_name[ reg1_num ] );
					else
						fprintf( dest, "\tINC\t%s PTR [%s]\n",
							sz == 4 ? "DWORD" : sz == 2 ? "WORD" : "BYTE", src1_opnd );
				}
				else
				{
					int	fp_size;

					sscanf( size_spec, "FP%d", &fp_size );
					fprintf( dest, "\tFLD1\n" );
					fprintf( dest, "\tFADD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
				}

				generate_mov( dest_opnd, src1_opnd, size_spec );
			}
			else if ( strcmp( opr, "--" ) == 0 )
			{
				if ( strncmp( size_spec, "INT", sizeof( "INT" ) - 1 ) || strncmp( size_spec, "UINT", sizeof( "UINT" ) - 1 ) )
				{
					int	sz;

					if ( sscanf( size_spec, "INT%d", &sz ) )
						sscanf( size_spec, "UINT%d", &sz );

					if ( sscanf( src1_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
						fprintf( dest, "\tDEC\t%s\n", int_reg_name[ reg1_num ] );
					else
						fprintf( dest, "\tDEC\t%s PTR [%s]\n",
							sz == 4 ? "DWORD" : sz == 2 ? "WORD" : "BYTE", src1_opnd );
				}
				else
				{
					int	fp_size;

					sscanf( size_spec, "FP%d", &fp_size );
					fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFLD1\n" );
					fprintf( dest, "\tFSUB\tST(1)\n" );
					fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFSTP\tST(0)\n" );
				}

				generate_mov( dest_opnd, src1_opnd, size_spec );
			}
			else if ( strcmp( opr, "-" ) == 0 )
			{
				generate_mov( dest_opnd, src1_opnd, size_spec );

				if ( sscanf( dest_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
					fprintf( dest, "\tNEG\t%s\n", int_reg_name[ reg1_num ] );
				else
					fprintf( dest, "\tNEG\t DWORD PTR %s\n", dest_opnd );
			}
			else if ( strcmp( opr, "&" ) == 0 )
			{
				if ( sscanf( dest_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
					fprintf( dest, "\tLEA\t%s, [%s]\n", int_reg_name[ reg1_num ], src1_opnd );
				else
				{
					fprintf( dest, "\tPUSH\tEAX\n"
									"\tLEA\tEAX, [%s]\n"
									"\tMOV\t%s, EAX\n"
									"\tPOP\tEAX\n", src1_opnd, dest_opnd );
				}
			}
			else if ( strcmp( opr, "~" ) == 0 )
			{
				generate_mov( dest_opnd, src1_opnd, size_spec );

				if ( sscanf( dest_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
					fprintf( dest, "\tNOT\t%s\n", int_reg_name[ reg1_num ] );
				else
					fprintf( dest, "\tNOT\t DWORD PTR %s\n", dest_opnd );
			}
			else if ( strcmp( opr, "!" ) == 0 )
			{
				generate_mov( dest_opnd, src1_opnd, size_spec );

				if ( sscanf( dest_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
					fprintf( dest, "\tNEG\t%s\n", int_reg_name[ reg1_num ] );
				else
					fprintf( dest, "\tNEG\t DWORD PTR %s\n", dest_opnd );

				if ( strcmp( dest_opnd, "__temp_int0" ) == 0 )
				{
					fprintf( dest, "\tTEST\tEAX, EAX\n"
							"\tSETNZ\tAL\n"
							"\tCBW\n"
							"\tCWDE\n" );
				}
				else
				{
					fprintf( dest, "\tPUSH\tEAX\n"
							"\tMOV\tEAX, %s\n"
							"\tTEST\tEAX, EAX\n"
							"\tSETNZ\tAL\n"
							"\tCBW\n"
							"\tCWDE\n"
							"\tMOV\t%s, EAX\n"
							"\tPOP\tEAX\n", dest_opnd, dest_opnd );
				}
			}
		}
		else if ( sscanf( line, " %s <- %s [ %s ]", dest_opnd, src1_opnd, size_spec ) == 3 )
		{
			generate_mov( dest_opnd, src1_opnd, size_spec );
			// Assignment (+conversion)
		}
		else if ( sscanf( line, " %s <- %s ( %s %d <= %s %d )", dest_opnd, src1_opnd, t_dest, &sz_dest, t_src, &sz_src ) == 6 )
		{
			generate_conversion( line );
		}
		else if ( sscanf( line, " %s %s [ %s ] ", opr, src1_opnd, size_spec ) == 3 )
		{
			if ( strcmp( opr, "++" ) == 0 )
			{
				if ( strncmp( size_spec, "INT", sizeof( "INT" ) - 1 ) || strncmp( size_spec, "UINT", sizeof( "UINT" ) - 1 ) )
				{
					int	sz;

					if ( sscanf( size_spec, "INT%d", &sz ) != 1 )
						sscanf( size_spec, "UINT%d", &sz );

					if ( sscanf( src1_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
						fprintf( dest, "\tINC\t%s\n", int_reg_name[ reg1_num ] );
					else
						fprintf( dest, "\tINC\t%s PTR [%s]\n", 
							sz == 4 ? "DWORD" : sz == 2 ? "WORD" : "BYTE", src1_opnd );
				}
				else
				{
					int	fp_size;

					sscanf( size_spec, "FP%d", &fp_size );
					fprintf( dest, "\tFLD1\n" );
					fprintf( dest, "\tFADD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
				}
			}
			else if ( strcmp( opr, "--" ) == 0 )
			{
				if ( strncmp( size_spec, "INT", sizeof( "INT" ) - 1 ) )
				{
					int	sz;

					if ( sscanf( size_spec, "INT%d", &sz ) != 1 )
						sscanf( size_spec, "UINT%d", &sz );

					if ( sscanf( src1_opnd, "__temp_int%d", &reg1_num ) == 1 && reg1_num >= 0 && reg1_num < 6 )
						fprintf( dest, "\tDEC\t%s\n", int_reg_name[ reg1_num ] );
					else
						fprintf( dest, "\tDEC\t%s PTR [%s]\n", 
							sz == 4 ? "DWORD" : sz == 2 ? "WORD" : "BYTE", src1_opnd );
				}
				else
				{
					int	fp_size;

					sscanf( size_spec, "FP%d", &fp_size );
					fprintf( dest, "\tFLD\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFLD1\n" );
					fprintf( dest, "\tFSUB\tST(1)\n" );
					fprintf( dest, "\tFSTP\t%s [%s]\n", fp_size == 4 ? "DWORD PTR" : ( fp_size == 8 ? "QWORD PTR" : "TBYTE PTR" ),
						src1_opnd );
					fprintf( dest, "\tFSTP\tST(0)\n" );
				}
			}
		}
	}

	fclose( src );

	fprintf( dest, "END\n" );
	fclose( dest );
}