#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#ifdef  RUESING
#   include "[Ruesing.Ctools.Include]avl.h"
#else
#   include "avl.h"
#endif


static  int     (*Cmp)();
static  HEADER  *Newnode;

static  HEADER  *Conflicting;

HEADER *AvlTreeAlloc( int size )
{
    HEADER  *alloc_p;

    if( ( alloc_p = (HEADER*)malloc( size + sizeof(HEADER) ) ) )
    {
        alloc_p->left = NULL;
        alloc_p->right = NULL;
        alloc_p->size = size;
        alloc_p->bal   = B;
        alloc_p++;
    }
    return( alloc_p );
}

void    AvlTreeFree( HEADER* p )
{
    free( --p );
}

static  void    Ins( HEADER** pp )
{
HEADER  *p;
HEADER  *p1,*p2;
int     relation;
static  int h = 0;

    if( !(p = *pp) )
    {
        p = Newnode;
        h = 1;
    }
    else if( (relation = (*Cmp)( p+1, Newnode+1 )) == 0 )
    {
        Conflicting = p+ 1;
        h = 0;
    }
    else if( relation > 0 )
    {
        Ins( &p->left );
        if( h )
        {
            switch( p->bal )
            {
                case R: p->bal = B; h = 0; break;
                case B: p->bal = L;        break;
                case L:
                        p1 = p->left;
                        if( p1->bal == L )
                        {
                            p->left   = p1->right;
                            p1->right = p;
                            p->bal    = B;
                            p         = p1;
                        }
                        else
                        {
                            p2        = p1->right;
                            p1->right = p2->left;
                            p2->left  = p1;
                            p->left   = p2->right;
                            p2->right = p;
                            p->bal    = (p2->bal == L) ? R : B;
                            p1->bal   = (p2->bal == R) ? L : B;
                            p         = p2;
                        }
                        p->bal = B;
                        h      = 0;
            }
        }
    }
    else
    {
        Ins( &p->right );
        if( h )
        {
            switch( p->bal )
            {
                case L: p->bal = B; h = 0; break;
                case B: p->bal = R;        break;
                case R:
                        p1 = p->right;
                        if( p1->bal == R )
                        {
                            p->right  = p1->left;
                            p1->left  = p;
                            p->bal    = B;
                            p         = p1;
                        }
                        else
                        {
                            p2        = p1->left;
                            p1->left  = p2->right;
                            p2->right = p1;
                            p->right  = p2->left;
                            p2->left  = p;
                            p->bal    = (p2->bal == R) ? L : B;
                            p1->bal   = (p2->bal == L) ? R : B;
                            p         = p2;
                        }
                        p->bal = B;
                        h      = 0;
            }
        }
    }
    *pp = p;
}

HEADER  *AvlInsert( HEADER **rootp, HEADER *newnode, int (*cmp)() )
{
    Cmp         = cmp;
    Newnode     = newnode - 1;
    Conflicting = NULL;

    Ins( rootp );

    return( Conflicting );
}
