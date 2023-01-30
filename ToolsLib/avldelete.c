#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#ifdef  RUESING
#   include "[Ruesing.Ctools.Include]avl.h"
#else
#   include "avl.h"
#endif

int Balance_L( HEADER** );
int Balance_R( HEADER** );
int Descend  ( HEADER**, HEADER** );
int Del      ( HEADER** );

static  HEADER  *Key;
static  int     (*Cmp)();
static  int     Notfound;

int Balance_L( HEADER** pp )
{
register    HEADER  *p, *p1, *p2;
int         b1, b2;
int         got_smaller = 1;

    p = *pp;

    switch( p->bal )
    {
        case L: p->bal = B;                     break;
        case B: p->bal = R; got_smaller = 0;    break;
        case R:
                p1 = p->right;
                b1 = p1->bal;

                if( b1 >= B )
                {
                    p->right = p1->left;
                    p1->left = p;
                    if( b1 != B )
                        p->bal = p1->bal = B;
                    else
                    {
                        p->bal  = R;
                        p1->bal = L;
                        got_smaller = 0;
                    }
                    p = p1;
                }
                else
                {
                    p2        = p1->left;
                    b2        = p2->bal;
                    p1->left  = p2->right;
                    p2->right = p1;
                    p->right  = p2->left;
                    p2->left  = p;
                    p->bal    = (b2 == R) ? L : B;
                    p1->bal   = (b2 == L) ? R : B;
                    p         = p2;
                    p2->bal   = B;
                }
    }
    *pp = p;
    return( got_smaller );
}                   

int Balance_R( HEADER** pp )
{
register    HEADER  *p, *p1, *p2;
int         b1, b2;
int         got_smaller = 1;

    p = *pp;

    switch( p->bal )
    {
        case R: p->bal = B;                     break;
        case B: p->bal = L; got_smaller = 0;    break;
        case L:
                p1 = p->left;
                b1 = p1->bal;

                if( b1 <= B )
                {
                    p->left   = p1->right;
                    p1->right = p;
                    if( b1 != B )
                        p->bal = p1->bal = B;
                    else
                    {
                        p->bal      = L;
                        p1->bal     = R;
                        got_smaller = 0;
                    }
                    p = p1;
                }
                else
                {
                    p2         = p1->right;
                    b2         = p2->bal;
                    p1->right  = p2->left;
                    p2->left   = p1;
                    p->left    = p2->right;
                    p2->right  = p;
                    p->bal    = (b2 == L) ? R : B;
                    p1->bal   = (b2 == R) ? L : B;
                    p         = p2;
                    p2->bal   = B;
                }
    }
    *pp = p;
    return( got_smaller );
}                   

int Descend( HEADER** rootp, HEADER**  dpp )
{
    if( (*rootp)->right )
        return( Descend( &(*rootp)->right, dpp ) ) ? Balance_R( rootp ) : 0;
    else
    {
        memcpy( *dpp+1, *rootp+1, (*rootp)->size );
        *dpp   = (*rootp);
        *rootp = (*rootp)->left;
        return( 1 );
    }
}

int Del( HEADER** rootp )
{
HEADER  *dp;
int     got_smaller = 0;
static  int relation;

    if( !*rootp )
        Notfound = 1;
    else
    {
        relation = (*Cmp)( Key, *rootp+1 );
        if( relation < 0 )
        {
            if( Del( &(*rootp)->left ) )
                got_smaller = Balance_L( rootp );
        }
        else if( relation > 0 )
        {
            if( Del( &(*rootp)->right) )
                got_smaller = Balance_R( rootp );
        }
        else
        {
            dp = *rootp;

            if( dp->right == NULL )
            {
                *rootp      = dp->left;
                got_smaller = 1;
            }
            else if( dp->left == NULL )
            {
                *rootp = dp->right;
                got_smaller = 1;
            }
            else if( Descend( &(*rootp)->left, &dp ) )
            {
                got_smaller = Balance_L( rootp );
            }
            free( dp );
        }
    }
    return( got_smaller );
}

int AvlDelete( HEADER** rootp, HEADER* key, int (*cmp)() )
{
    Cmp      = cmp;
    Key      = key;
    Notfound = 0;
	
    Del( rootp );
    
    return( Notfound );
}
