#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#ifdef  RUESING
#   include "[Ruesing.Ctools.Include]avl.h"
#else
#   include "avl.h"
#endif

static  void    Fa( HEADER *root )
{
    if( root )
    {
        Fa( root->left );
        Fa( root->right );
        free( root );
    }
}

void    AvlFreeAll( HEADER** root )
{
    Fa( *root );
    *root = NULL;
}  
