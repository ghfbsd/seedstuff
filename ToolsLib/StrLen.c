#include <stdio.h>
#include <string.h>

int main( int argc, char** argv )
{
int 	iCount, iStrLen = 1024, iEmpty;
char	caStr[iStrLen];
#define FALSE	(0)
#define TRUE	(1)

	if( argc != 2 )
	{
		fprintf( stderr, "\nusage : strlen -<e|l> " );
		fprintf( stderr, "\n");
		fprintf( stderr, "\n -e  don't echo an empty string" );
		fprintf( stderr, "\n -l  print string length\n" );
		exit(1);
	}
	else
	{
		while( fgets( caStr, iStrLen, stdin ) )
		{
			if( argv[1][1] != 'l' )
			{
				iEmpty = TRUE;
				for(iCount = 0; caStr[iCount] != '\0'; iCount++ )
					if( caStr[iCount] != ' ' &&
					    caStr[iCount] != '\n'  )
					{
						iEmpty = FALSE;
						break;
					}
				if( iEmpty == FALSE )
					fputs( caStr, stdout );
			}
			else
				fprintf( stdout, "\n%d", strlen(caStr) );
		}
	}
	exit(0);
} 
   
