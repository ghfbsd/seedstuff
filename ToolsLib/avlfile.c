#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>

#include "avl.h"

static  int    (*Writeavl)();
static  int    (*Readavl)();
static  int    (*Icmp)();

static  void FilePrint( HEADER* root, FILE* stream )
{

    if( root )
    {
        FilePrint( root->left, stream );
        FilePrint( root->right, stream );
        Writeavl( stream, root+1 );
    }
}       

int AvlWriteFile( HEADER *root, char* filename, int (*writeavl)() )
{
FILE *stream;

    Writeavl = writeavl;

    if( !(stream = fopen(filename, "w+")) )
    {
        fprintf( stderr, "\nFile (write)[%s] does no exist !\n", filename );
        return( 0 );
    }
    else
        FilePrint( root, stream );

    fclose( stream );
    return( 1 );
}


int AvlReadFile( HEADER **root, 
                 char *filename,
                 int size, 
                 int (*readavl)(),
                 int (*icmp)()    )
{
FILE *stream;
HEADER *p;
static int end;

    Readavl = readavl;
    Icmp = icmp;

    if( !(stream = fopen(filename, "r+")) )
    {
        fprintf( stderr, "\nFile (read)[%s] does not exist !\n", filename );
        return( 0 );
    }
    else    
    {
        end = 0;
        do
        {
            if( !(p = AvlTreeAlloc( size )) )
            {
                fprintf( stderr, "Zu wenig Speicher !" );
                return( 0 );
            }
            else
            {
                if( (*Readavl)(stream, p) != EOF )
                {
                    if( AvlInsert( root, p, Icmp) )  
                    {
                        fprintf( stderr, "\nDouble node in AVL read !" );
                        AvlTreeFree( p );
                    }
                }
                else
                {
                    AvlTreeFree( p );
                    end = 1;
                }
            }
        }
        while( !end );
    }
    fclose( stream );
    return( 1 );
}
