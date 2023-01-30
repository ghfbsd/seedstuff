#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#ifdef  RUESING
#   include "[Ruesing.Ctools.Include]avl.h"
#else
#   include "avl.h"
#endif

HEADER  *AvlReplace( HEADER *root,
                     HEADER *key,
                     int (*cmp)(),
                     HEADER *replDat,
                     int size )
{
static  int relation;
    
    if( !root )
        return( NULL );

    relation = (*cmp)( key, root+1 );

    if( relation == 0 )
	{ 
		memcpy( root+1, replDat, size-1 );
        return( root+1);
	}
    else if( relation < 0 )
            return( AvlReplace( (HEADER*)root->left, 
                                (HEADER*)key, cmp, 
                                (HEADER*)replDat,
                                size ) );
    else
            return( AvlReplace( (HEADER*)root->right,
                                (HEADER*)key, cmp,
                                (HEADER*)replDat,
                                size ) );
}
