#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#include "avl.h"

HEADER  *AvlFind( HEADER *root, HEADER *key, int (*cmp)() )
{
static  int relation;
    
    if( !root )
        return( NULL );

    relation = (*cmp)( key, root+1 );

    if( relation == 0 ) 
        return( root+1);
    else if( relation < 0 )
            return( AvlFind( (HEADER*)root->left, (HEADER*)key, cmp ) );
    else
            return( AvlFind( (HEADER*)root->right, (HEADER*)key, cmp ) );
}
