#include <stdio.h>
#include <string.h>
#include <ctype.h>

/****************************************************************************
BF
F * Function    : StrToLower
  *
D * Description : Converts uppercase characters in a string to lowercase
  * 
C * Call        : ConvertedString = StrToLower( StringToConvert )
  *
P * Parameter   : char *StringToConvert
  *
E * Error Code  : none
  *
R * Return      : char *ConvertedString
EF
 ****************************************************************************/
/* ======================================================================== */
char *StrToLower( char *cpToLowerStr )
/* ======================================================================== */
{
char *cpStrCopy;
int  iCount = 0;

	if( !(cpStrCopy = strdup(cpToLowerStr)) )
		return( NULL );
	else
	{
		while( *cpToLowerStr )
		{
			if( isupper(*cpToLowerStr) )
				*(cpStrCopy+iCount) = tolower( *cpToLowerStr );
			cpToLowerStr++;
			iCount++;
		}
	}
	return( cpStrCopy ); 
}
