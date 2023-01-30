#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>


#include "avl.h"

HEADER  *AvlMax( HEADER *root )
{
    
    if( !(root->right) )
        return( root+1);
    else
        return( AvlMax( (HEADER*)root->right ) );
}

HEADER  *AvlMin( HEADER *root )
{
    
    if( !(root->left) )
        return( root+1);
    else
        return( AvlMin( (HEADER*)root->left ) );
}
