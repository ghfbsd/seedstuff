#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#ifdef  RUESING
#   include "[Ruesing.Ctools.Include]avl.h"
#else
#   include "avl.h"
#endif

#define VERT    Cset[0]
#define GAMMA   Cset[1]
#define ELL     Cset[2]
#define T_LEFT  Cset[3]
#define T_UP    Cset[4]
#define T_DOWN  Cset[5]

static  char    *Norm_chars[] = { "|", "+--", "+--", "--+", "--+", "--+" };

static  int     (*Print)();
static  FILE    *Out;
static  char    **Cset;
static  char    Map[64/8];

#define TESTBIT( c )    ( Map[c >> 3] & (1 << ( c & 0x07 )) )

static  void SetBit( int c, int val )
{
    if( val )
        Map[c >> 3] |= 1 << (c & 0x07);
    else
        Map[c >> 3] &= ~( 1 << (c & 0x07) );
}

static  void Trav( HEADER* root, int amleft )
{
static  int depth = -1;
static  int i;

    if( root )
    {
        ++depth;
        if( root->right )
            Trav( root->right, 0 );
        else
            SetBit( depth+1, 1 );

        for( i = 1; i <= depth; i++ )
        {
            (*Print)( Out, 0 );
            if( i == depth )
                fprintf( Out, "  %s", amleft ? ELL : GAMMA );
            else if( TESTBIT( i ) )
                     fprintf( Out, "  %s  ", VERT );
            else
                     fprintf( Out, "     " );
        }
        (*Print)( Out, root + 1 );
        fprintf( Out, "%s\n",(root->left) ? ( root->right ? T_LEFT : T_DOWN )
                                          : ( root->right ? T_UP   : ""     ) );
    
        SetBit( depth, amleft ? 0 : 1 );
        if( root->left )
            Trav( root->left, 1 );
        else
            SetBit( depth+1, 0 );

        --depth;
    }
}       

void    AvlTreePrint( HEADER *root, int (*print)(), FILE *stream )
{
    Out   = stream;
    Print = print;
    Cset  = Norm_chars;
    Trav( root, 0 );
}
