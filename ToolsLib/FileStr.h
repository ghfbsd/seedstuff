/****************************************************************************
BM         
M* Module      : FileFind.c 
 * 
D* Description : File find utilitie 
 * 
I* Import      :
 *
E* Export      : int	FileFind( StartPath, ReturnedFile, Mode )
E*             
E*               char 	*StartPath 
E*               char	*ReturnedFile
E*               char	*Mode
 *
P* Author      : W. Ruesing
 *
U* Updates     :
EM 
 ****************************************************************************/
/* ======================================================================== */
int	FileFind( char*, char*, char* );
/* ======================================================================== */

/****************************************************************************
BM         
M * Module      : StrMatch.c 
  *
D * Description :
D * 
D *	Test string for match using pattern.  The pattern may
D *	contain the normal shell metacharacters for pattern
D *	matching.  The '*' character matches any string,
D *	including the null string.  The '?' character matches
D *	any single character.  A list of characters enclosed
D *	in '[' and ']' matches any character in the list.
D *	If the first character following the beginning '['
D *	is a '!' then any character not in the list is matched.
  * 
C * Export      : int	StrMatch( String, Pattern )
  *               
P *               register char	*String
P *               register char	*Pattern
  *
E * Error Code  : none
  *             
EM 
 ****************************************************************************/
/* ======================================================================== */
int	StrMatch( char*, char* );
/* ======================================================================== */

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
char *GetSubStr( char* );
char *GetSubStrDelim( char*, char* );
/* ======================================================================== */

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
char *StrToUpper( char* );
/* ======================================================================== */

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
char *StrToLower( char* );
/* ======================================================================== */

