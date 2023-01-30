#if defined(STDC_HEADERS)
#include <stdlib.h>
#include <ctype.h>
#endif
#include <stdio.h>
#include <string.h>

extern int IsDelimChar( char, char* );

/****************************************************************************
BF
F * Function    : GetSubStr
F *               GetSunStrDelim
  *
D * Description : a)
d *               Breaks down an alphanumeric string in it's peaces, where
D *               the delimiter is any other character.
D *               The first call is with the string to break down. by all 
D *               following calls the parameter for the string is NULL.
D *               A newline or NULL byte ends the seperation of the string
D *               and NULL will be returned.
D *
D *               b)
D *               The delimeter string tells the routine which char's should
D *               also be matched as valid char's.
  * 
C * Call        : GetNextSubStr( BreakDownString [, DelimeterString] )
  *
P * Parameter   : char *BreakDownString
  *
E * Error Code  : none 
  *
R * Return      : char* - The next Token which was extracted 
EF
 ****************************************************************************/
/* ======================================================================== */
char *GetSubStr( char *cpStr )
/* ======================================================================== */
{
static char caBuf[513];
char caRetBuf[513];
static int iBufCount = 0;
int iRetCount = 0;

	if( cpStr != NULL )
	{
		memset( caBuf, '\0', 512 );
		strcpy( caBuf, cpStr );
		iBufCount = 0;
	}
	else
	{
		while( !isalnum(caBuf[iBufCount]) &&  caBuf[iBufCount] != '_' )
			if( caBuf[iBufCount] == '\0' ||
			    caBuf[iBufCount] == '\n' )
				return( NULL );
			else
				iBufCount++;
	}
	while( isalnum(caBuf[iBufCount]) || caBuf[iBufCount] == '_' )
	{
		caRetBuf[iRetCount++] = caBuf[iBufCount++];
	}
	if( iRetCount == 0 )
		return( NULL );
	else
	{
		caRetBuf[iRetCount] = '\0';
		return( caRetBuf );
	}
}

/* ======================================================================== */
char *GetSubStrDelim( char *cpStr, char *cpDelimStr )
/* ======================================================================== */
{
static char caBuf[513];
char caRetBuf[513];
static int iBufCount = 0;
int iRetCount = 0;

	if( cpStr != NULL )
	{
		memset( caBuf, '\0', 512 );
		strcpy( caBuf, cpStr );
		iBufCount = 0;
	}
	else
	{
		while( !isalnum(caBuf[iBufCount]) &&  
                       caBuf[iBufCount] != '_'    &&
                       !IsDelimChar((char)caBuf[iBufCount], cpDelimStr) )
			if( caBuf[iBufCount] == '\0' ||
			    caBuf[iBufCount] == '\n' )
				return( NULL );
			else
				iBufCount++;
	}
	while( isalnum(caBuf[iBufCount]) || 
               caBuf[iBufCount] == '_'   ||
               IsDelimChar((char)caBuf[iBufCount], cpDelimStr) )
	{
		caRetBuf[iRetCount++] = caBuf[iBufCount++];
	}
	if( iRetCount == 0 )
		return( NULL );
	else
	{
		caRetBuf[iRetCount] = '\0';
		return( caRetBuf );
	}
}

/* ======================================================================== */
int IsDelimChar( char caChar, char *cpDelimStr )
/* ======================================================================== */
{
int iCount = 0;

	while( *(cpDelimStr+iCount) != '\0' )
	{
		if( *(cpDelimStr+iCount) == caChar )
			return(1);
		iCount++;
	}
	return(0);
}
