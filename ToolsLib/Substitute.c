/****************************************************************************
BM         
M* Module      : Substitute.c
 * 
D* Description : Substitutes a given string with a source wildcard string
D*               in a target match string.    
 * 
I* Import      :
 *
E* Export      : SubstituteStr
 *
P* Author      : Wilhelm Ruesing
 *
U* Updates     :
EM 
 ****************************************************************************/

#if defined(STDC_HEADERS)
#include <stdlib.h>
#endif
#include <stdio.h>
#include <string.h>
#include <ctype.h>			


char *SetDelimSourceStr   ( char*, char* );
char *SetDelimTargetStr   ( char* );
char *MergeSourceTargetStr( char*, char* );
void  BuildSourceDelimStr ( char*, char* );
char *GetToken            ( char* ); 
char *IsTokenChar         ( char*, char* );
char *MergeString         ( char*, char*, int );
int   TestIsNeed          ( char*, char*, char* );
int   WildChars           ( char* );

static char *cpTokenStr;
#define FALSE		(0)
#define	TRUE		(1)
#define WILDCARD 	"*"
#define DELIM 		'|'
#define DELIMSTR	"|"
#define TOKEN_SIZE	(257)

/****************************************************************************
BF
F * Function    : SubstituteStr
  *
D * Description : A given source string which is matched by a pattern string
D *               will be converted to target string which matches a target
D *               pattern.
D *               Only the meta wildcard "*" is suported.
D *
D *               Example : The string "astringtoconvert" with the source
D *                         pattern "astring*" and the target pattern
D *                         "*totarget" will be converted to "astringtotarget". 
D *
D *
D *               The mechanism to convert:
D * 
D *               a) extract the wildcard pattern * and save is as ~.
D *               b) change the subtext around ~ to delimeter |.
D *               c) change wildcard pattern in the target string to
D *                  delimeter |.
D *               e) merge these two strings.
D *                  - if first char of source string is |
D *                    		start with target string
D *                  - otherwise start with source string
D *                  - replace | with substring of the other pattern string 
D *
D *               ~ extracted substring for wildcard
D *               - given substring
D *               | replacement delimeter for subtext
D * 
D *               Source String  -*-  ->  |~|  \                   
D *               Source Pattern -~-            \      
D *                                              -> -~-  Substituted String
D *                                             / 
D *               Target Pattern -*-  ->  -|-  /          
  * 
C * Call        : SubstituteStr( SourceStr, SourcePattern, TargetPattern )
  *
P * Parameter   : char *SourceStr
P *               char *SourcePattern
P *               char *TargetPattern 
  *
E * Error Code  : 
  *
R * Return      : char* - NULL substitution failed
R *                       Substituted string
EF
 ****************************************************************************/
/* ========================================================================== */
char *SubstituteStr( char *cpSourceStr, 
                     char *cpSourcePattern,
                     char *cpTargetPattern )
/* ========================================================================== */
{
char *cpDelimSourceStr,
     *cpDelimTargetStr,
     *cpTargetStr;

	if( TestIsNeed(cpSourceStr, cpSourcePattern, cpTargetPattern) )
	{ 
		if( !(cpTokenStr = (char*)calloc(TOKEN_SIZE,0)) )
			return( NULL );

		if( !(cpDelimSourceStr = SetDelimSourceStr(cpSourcePattern,
                                                   cpSourceStr)) )
			return( NULL );

		if( !(cpDelimTargetStr = SetDelimTargetStr(cpTargetPattern)) )
			return( NULL );
		if( !(cpTargetStr = MergeSourceTargetStr(cpDelimSourceStr,
                                                 cpDelimTargetStr)) )
			return( NULL );
		return( cpTargetStr );
	}
	else
		return( NULL );
  
	   
}

/* ========================================================================== */
char *SetDelimSourceStr( char *cpSourcePattern, char *cpSourceStr )
/* ========================================================================== */
{
char *cpTokenItem,
     *cpHelpStr = strdup( cpSourceStr );

	strcpy( cpTokenStr, cpSourcePattern ); 
	while( cpTokenItem = GetToken( WILDCARD ) )
	{
		BuildSourceDelimStr( cpHelpStr,cpTokenItem ); 
	}
	return( cpHelpStr );
}

/* ========================================================================== */
void BuildSourceDelimStr( char *cpSourceStr,
                          char *cpTokenItem )
/* ========================================================================== */
{
char cpSubStr1[TOKEN_SIZE],
     cpSubStr2[TOKEN_SIZE],
     *cpDelimChar;

	 
	if( (cpDelimChar = strstr(cpSourceStr,cpTokenItem))&&
	                   strlen( cpTokenItem ) > 0 )
	{
		strcpy( cpSubStr2, cpDelimChar+strlen(cpTokenItem) );
		*cpDelimChar = '\0';
		strcpy( cpSubStr1, cpSourceStr );		
		sprintf( cpSourceStr, "%s|%s",cpSubStr1,cpSubStr2 );
	}
	return; 
}

/* ========================================================================== */
char *SetDelimTargetStr( char *cpTargetPattern )
/* ========================================================================== */
{
int iIndex = 0;
char *cpDupStr = strdup( cpTargetPattern ); 

	while( *(cpDupStr+iIndex) != '\0' )
	{
		if( *(cpDupStr+iIndex) == '*' )
			*(cpDupStr+iIndex) = DELIM;
		iIndex++;
	}
	return( cpDupStr );
}

/* ========================================================================== */
char *GetToken( char *cpDelimeter )
/* ========================================================================== */
{
static char cpTokenRtn[TOKEN_SIZE];
char *cpTokenChar;

	cpTokenRtn[0] = '\0';
	if( cpTokenStr         == NULL ||
            strlen(cpTokenStr) == 0    ||
            *cpTokenStr        == '\0'   )
		return( NULL );

	if( (cpTokenChar = IsTokenChar(cpTokenStr, cpDelimeter)) == NULL )
	{
		cpTokenStr++;
		cpTokenChar = IsTokenChar(cpTokenStr, cpDelimeter);
	}

	while( cpTokenChar )
	{
		strcat( cpTokenRtn, cpTokenChar );
		cpTokenStr++;
                cpTokenChar = IsTokenChar(cpTokenStr, cpDelimeter); 		
	} 	
	if( strlen(cpTokenRtn) ) 
		return( cpTokenRtn );
	else
		return( (char*)NULL );
}

/* ========================================================================== */
char *IsTokenChar( char *cpChar, char *cpDelimeter )
/* ========================================================================== */
{
int iIndex;
char caChar[2];

	if( *cpChar == '\0' || !strlen(cpChar) )
		return( NULL );

	for( iIndex = 0; iIndex < strlen(cpDelimeter) ; iIndex++ )
		if( *cpChar == *(cpDelimeter+iIndex) )
			return( NULL );

	caChar[0] = *cpChar;
	caChar[1] = '\0';	
	return( caChar );
}

/* ========================================================================== */
char *MergeSourceTargetStr( char *cpDelimSourceStr,
                            char *cpDelimTargetStr )
/* ========================================================================== */
{
int  iStartNo;

	if( *cpDelimSourceStr == DELIM && 
            *cpDelimTargetStr == DELIM )
		return( NULL );
	else if( *cpDelimSourceStr != DELIM && 
                 *cpDelimTargetStr != DELIM )
		iStartNo = 1;
	else if( *cpDelimSourceStr == DELIM )
	{
		cpDelimSourceStr++;
		iStartNo = 1;
	}
	else if( *cpDelimSourceStr != DELIM )
		iStartNo = 0;

 	if( *cpDelimTargetStr == DELIM )
		cpDelimTargetStr++;

	return( MergeString( cpDelimSourceStr,
                             cpDelimTargetStr, 
                             iStartNo) );

}
/* ========================================================================== */
char *MergeString(  char *cpDelimSourceStr,
                    char *cpDelimTargetStr, 
                    int  iStartNo )
/* ========================================================================== */
{
char *cpRtnStr,
     *cpDoubleStr = (char*)calloc( TOKEN_SIZE,0 ),
     *cpaMergeStr[2];
 
	cpaMergeStr[0] = cpDelimSourceStr;
        cpaMergeStr[1] = cpDelimTargetStr;
	cpRtnStr = cpDoubleStr;
	while( !((*cpaMergeStr[0] == '\0')&&(*cpaMergeStr[1] == '\0')) )
	{
		while( *(cpaMergeStr[iStartNo]) != DELIM &&
		       *(cpaMergeStr[iStartNo]) != '\0' )
		{
			*cpDoubleStr = *(cpaMergeStr[iStartNo])++;
			cpDoubleStr++;
		}
		if( *(cpaMergeStr[iStartNo]) != '\0' )
			 (cpaMergeStr[iStartNo])++;
		iStartNo = -1*iStartNo+1;
	}
	*cpDoubleStr = '\0';

	return( cpRtnStr );
}

/* ========================================================================== */
int WildChars( char *cpWildStr )
/* ========================================================================== */
{
char *cpCompStr = cpWildStr;
int   iCountBack = 0;
	while( *cpCompStr != '\0' )
	{
		if( *cpCompStr == '*' )
			iCountBack++;
		cpCompStr++;
	}
	return( iCountBack );
}

/* ========================================================================== */
int TestIsNeed( char *cpSourceStr, 
                char *cpSourcePattern, 
                char *cpTargetPattern ) 
/* ========================================================================== */
{
	if( strlen(cpSourceStr)     < 1 &&
            strlen(cpSourcePattern) < 1 &&
            strlen(cpTargetPattern) < 1 )
		return( FALSE );

	if( WildChars(cpSourcePattern) != WildChars(cpTargetPattern) )
		return( FALSE );
	if( !strcmp(cpSourcePattern, cpTargetPattern) )
		return( FALSE );
 
	return( TRUE ); 
}

/* ************************************************************************** */

#ifdef STANDALONE

/* ========================================================================== */
int main( int argc, char **argv )
/* ========================================================================== */
{
FILE *fpOutput,
     *fpInput;
#define SIZE	(257)
char *cpGetString = (char*)malloc( SIZE ),
     *cpSourcePat = (char*)malloc( SIZE ),
     *cpTargetPat = (char*)malloc( SIZE ),
     *cpBackStr   = (char*)malloc( SIZE );

	if( argc >= 3)
	{
		fpOutput = stdout;
                fpInput  = stdin;
	}
	else if( argc < 3 )
	{
		fprintf( stderr, "\nusage : substitute <SourceString> <SourcePattern> <TargetPattern> \n");
		fprintf( stderr, "Example :substitute Testfile.sub Test*.sub Finished*.now \n\n" );
		exit(1);
	}

	if( argc >= 3 )
	{
		strcpy( cpGetString, argv[1] );
		strcpy( cpSourcePat, argv[2] );
		strcpy( cpTargetPat, argv[3] );
		fprintf( stdout, "\n" );
		if( cpBackStr = SubstituteStr( cpGetString,
                                               cpSourcePat,
                                               cpTargetPat ) )
		{
			fprintf( stdout, "\n" );
			fprintf( fpOutput,"%s\n", cpBackStr);
			exit(0);
		}
		else
			exit(1);

	}
	else
	{
		strcpy( cpSourcePat, argv[1] );
		strcpy( cpTargetPat, argv[2] );
		while( fgets( cpGetString, SIZE, fpInput ) )
		{
			*(cpGetString+strlen(cpGetString)-1) = '\0';
			if( cpBackStr = SubstituteStr( cpGetString,
                                                   cpSourcePat,
                                                   cpTargetPat ) )
			{
				fprintf( fpOutput,"%s\n", cpBackStr);
			}
			else
				exit(1);
		}
		fclose( fpOutput );
		exit(0);
	}
}

#endif
