#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>


typedef struct
        {
			int		id;
        	char	datum[9];
        	char	zeit[9];
        }LEAF;

#include "avltree.h"

/* -------------------------------------------------------------------------- */
/*  Ausgaberoutine der Baumdarstellung                                        */
/* -------------------------------------------------------------------------- */

int    Prnt( FILE* stream, LEAF* p )
{

/* Format des Ausgabepuffers muss mit dem der Leerausgabe uebereinstimmen */

	if( p )
    	fprintf( stream,"%5d" , (char*)p->id ); 
	else
    	fprintf( stream, "     " );  
}

/* -------------------------------------------------------------------------- */
/*  Lesefunktion der Datendatei                                               */
/*                                                                            */
/*  Format zum Lesen und Schreiben des In- und Out Streams sollten ueberein-  */
/*  stimmen                                                                   */
/* -------------------------------------------------------------------------- */

int	Readavl( FILE* stream, LEAF* p )
{
char temp_id[6];

    	if( fscanf( stream, "%5[^]%8[^]%8[^]", temp_id,
                                               (char*)p->datum,
                                               (char*)p->zeit )   == 3 )
		{
        	p->id = atoi( temp_id );
			return( 1 );
		}
		else
    		return( EOF );
}

/* -------------------------------------------------------------------------- */
/*  Schreibefunktion der Datendatei                                           */
/*                                                                            */
/*  Format zum Lesen und Schreiben des In- und Out Streams sollten ueberein-  */
/*  stimmen.                                                                  */
/* -------------------------------------------------------------------------- */

int   Writeavl( FILE* stream, LEAF* p )
{
char	string[22];

    	sprintf( string, "%-5d%-8s%-8s" ,(int)p->id,
                                         (char*)p->datum,
                                         (char*)p->zeit  );
		fprintf( stream, "%s", string );
}

/* -------------------------------------------------------------------------- */
/*  Vergleichfunktion beim Einfuegen der Baumschluessel samt Inhalt.          */
/* -------------------------------------------------------------------------- */

int Icmp( LEAF* n1, LEAF* n2 )
{
    return( n1->id - n2->id );
}

/* -------------------------------------------------------------------------- */
/*  Vergleichsfunktion zum Loeschen eines Schluessels samt Inhalt.            */
/* -------------------------------------------------------------------------- */

int Dcmp( int id, LEAF* n2 )
{
    return( id - n2->id );
}





/* -------------------------------------------------------------------------- */
/*                              DEMOPROGRAMM                                  */
/* -------------------------------------------------------------------------- */

void	menu( void )
{
    printf( " Kommandos i<Str>   - Knoten einfuegen \n" );
    printf( "           d<Str>   - Knoten loeschen \n" );
    printf( "           r<Str>   - Knoten ersetzen \n" );
    printf( "           f<Str>   - Knoten suchen \n" );
    printf( "           p<Str>   - Baum ausgeben \n" );
    printf( "           l<Datei> - Datei lesen \n" );
    printf( "           s<Datei> - Datei schreiben \n" );
    printf( "           a        - Baum loeschen \n" ); 
    printf( "           g        - Maximum im Baum \n" ); 
    printf( "           k        - Minimum im Baum \n" ); 
    printf( "           q        - Beenden \n" ); 
    printf( "           ?        - Befehlsuebersicht \n" ); 
}

void    DoCmd( int cmd, char* n )
{
static  TREE    *root = NULL;
LEAF            *p, *p2;

    switch( cmd )
    {
        case 'r':
                if( !(p = AvlTreeAlloc( sizeof( LEAF ))) )
                    fprintf( stderr, "Zu wenig Speicher!\n" );
                else
				{
				char	temp_id[6];

                    p->id    = atoi( strncpy( temp_id, n, 5 ) );
				    strncpy( p->datum, n+5, 8 );
				    strncpy( p->zeit, n+13, 8 );
                    fprintf(stderr, "\n Knoten %d wird ersetzt.\n", p->id );
                    fprintf(stderr, " durch Datum  %s\n", p->datum );
                    fprintf(stderr, " durch Zeit   %s\n\n", p->zeit );
                	if( !(AvlReplace( (TREE*)root, (LEAF*)p->id, Dcmp,
                                  (LEAF*)p, sizeof(LEAF) ) ) )
                    	fprintf( stderr, " Knoten nicht im Baum.\n" );
                    AvlTreeFree( p );
				}
                break;
        case 'd':
                if( !(AvlDelete( &root, (LEAF*)atoi( n ), Dcmp )) )
                    fprintf( stderr, " Knoten nicht im Baum.\n" );
                break;
        case 'f':
                if( p = AvlFind( (TREE*)root, (LEAF*)atoi( n ), Dcmp ) )
				{
                    fprintf(stderr, "\n Knoten %d gefunden.\n", p->id );
                    fprintf(stderr, " Datum  %s\n", p->datum );
                    fprintf(stderr, " Zeit   %s\n\n", p->zeit );
				}
                else
                    fprintf(stderr, " Knoten nicht gefunden.\n" );
                break;
        case 'i':
                if( !(p = AvlTreeAlloc( sizeof( LEAF ))) )
                    fprintf( stderr, "Zu wenig Speicher!\n" );
                else
                {
				char	temp_id[6];

                    p->id    = atoi( strncpy( temp_id, n, 5 ) );
					strncpy( p->datum, n+5, 8 );
					strncpy( p->zeit, n+13, 8 );
                    if( p2 = AvlInsert( &root, p, Icmp ) )
                    {
                        fprintf( stderr, "Knoten %d bereits im Baum.\n", p2->id );
                        AvlTreeFree( p );
                    }
                }               
                break;
        case 's':
				AvlWriteFile( root, (char*)n, Writeavl);
                break;
        case 'l':
				AvlReadFile( &root,(char*)n, sizeof( LEAF ), Readavl, Icmp );
                break;
        case 'a':
                AvlFreeAll( &root );
                break;
		case 'p':
    			printf( "\n\n" );
    			AvlTreePrint( root, Prnt, stdout );
    			printf( "\n" );
				break;
        case 'g':
				p = AvlMax( (TREE*)root );
                fprintf( stderr, "%d ist Maximum im Baum.\n", p->id );
				break;
        case 'k':
				p = AvlMin( (TREE*)root );
                fprintf( stderr, "%d ist Minimum im Baum.\n", p->id );
				break;
        case 'q':
                exit( 0 );
				break;
        case '?':
				menu();
				break;
    }
}


int main( int argc, char** argv )
{
char	buf[128];

    for( ++argv; --argc > 0; ++argv )
        DoCmd( **argv, *argv+1 );

	menu();
    printf("\ni/d/r/f/a/p/l/s/q/g/k/?:<Command> \n");
	printf("C| id|| date || time |\n");
    for( ;gets( buf ); )
	{
        DoCmd( *buf, buf+1 );
        printf("\ni/d/r/f/a/p/l/s/q/g/k/?:<Command>\n");
	    printf("C| id|| date || time |\n");
	}
}
