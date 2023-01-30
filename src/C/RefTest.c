#include <stdio.h>
#include <stdlib.h>

extern int   c_topen__    ( ); 
extern int   c_tread__    ( ); 
extern int   c_tclose__   ( ); 
extern int   c_doffset__  ( );
extern int   c_refread__  ( );

int main( int argc, char* argv ) {
int i,
    blksize = 2048,
    lend = 9,
    skip = 4*2048;
char buff[4096];

	c_topen__("/dev/sdb", &lend );
	for( i=1 ; i<10 ; i++ ) {
		if( i == 3 || i == 6)
			skip = - 4000;
		else
			skip = i*8192; 
		c_doffset__(&skip);
		c_refread__( &buff[0], &blksize );
	}
	c_tclose__();
}
