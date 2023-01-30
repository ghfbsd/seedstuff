#include <stdio.h>
#include <string.h>
#include <ctype.h>

/****************************************************************************
BF
F * Function    : StrToUpper
  *
D * Description : Converts lowercase characters in a string to uppercase
  * 
C * Call        : ConvertedString = StrToUpper( StringToConvert )
  *
P * Parameter   : char *StringToConvert
  *
E * Error Code  : none
  *
R * Return      : char *ConvertedString
EF
 ****************************************************************************/
/* ======================================================================== */
char *StrToUpper( char *cpToUpperStr )
/* ======================================================================== */
{
char *cpStrCopy;
int  iCount = 0;

	if( !(cpStrCopy = strdup(cpToUpperStr)) )
		return( NULL );
	else
	{
		while( *cpToUpperStr )
		{
			if( islower(*cpToUpperStr) )
				*(cpStrCopy+iCount) = toupper( *cpToUpperStr );
			cpToUpperStr++;
			iCount++;
		}
	}
	return( cpStrCopy ); 
}
