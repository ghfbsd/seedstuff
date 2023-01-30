#include <stdio.h>
#include <string.h>

FILE *fpOutFile,
     *fpInFile;

int StripChar( int* );

#define NOT_OUT	(-100)

int main( int argc, char **argv )
{
int cChar;

	if( argc == 1 )
	{
		fpOutFile = stdout;
		fpInFile  = stdin;
	}
	else if( argc == 2 )
	{
		if( (fpInFile = fopen( argv[1], "r" )) == NULL )
		{
			fprintf( stderr, "\nCan 't open file %s", argv[1] );
			exit(1);
		}
		fpOutFile = stdout;
	}
	else if( argc == 3 )
	{
		if( (fpInFile = fopen( argv[1], "r" )) == NULL )
		{
			fprintf( stderr, "\nCan 't open file %s", argv[1] );
			exit(1);
		}
		if( (fpOutFile = fopen( argv[2], "w" )) == NULL )
		{
			fprintf( stderr, "\nCan 't close file %s", argv[1] );
			exit(1);
		}
	}
	else
	{
		fprintf( stderr, "\nusage : StripEmptyLine <InFile> [OutFile]\n" );
		exit(1);
	}

	while( cChar = fgetc(fpInFile) )
	{
		if( StripChar( &cChar ) != NOT_OUT )
			fputc( cChar, fpOutFile );
	}
	fclose( fpOutFile );
	fclose( fpInFile );
}

#define TRUE	(1)
#define FALSE	(0)

int StripChar( int *cChar )
{
static int iIsNL = FALSE;

	switch( *cChar )
	{
		case '\n' :
			if( iIsNL )
				*cChar = NOT_OUT;
			else
				iIsNL = TRUE;
			break;
		default :
			iIsNL = FALSE;
	}
	return( *cChar );
}
			
