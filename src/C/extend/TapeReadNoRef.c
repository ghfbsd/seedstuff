#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <signal.h>
#include <string.h>

#define FAILURE		(-1)
#define SUCCESS		(0)
#define EOF_MARK	(1)
#define EOT_MARK	(2)
#define READ_ERR	(4)
#define FALSE		(0)
#define TRUE		(1)

#if defined(LINUX)
#define c_trewind  c_trewind__ 
#define c_topen    c_topen__
#define c_tread    c_tread__
#define c_tclose   c_tclose__
#define c_tskipf   c_tskipf__
#define c_write    c_write__	
#else
#define c_trewind  c_trewind_
#define c_topen    c_topen_
#define c_tread    c_tread_
#define c_tclose   c_tclose_
#define c_tskipf   c_tskipf_
#define c_write    c_write_
#endif

extern int   c_trewind  ( ); 
extern int   c_topen    ( ); 
extern int   c_tread    ( ); 
extern int   c_tclose  	( ); 
extern int   c_tskipf   ( ); 
extern int   c_write    ( );

static int   NextRecord ( );
static int   NextEofMark( );

struct mtop  sMaTapeCom;
struct mtget sMaTapeStat;

static int  fdTape;
static int  iGBlocks  = 0L,
            iPBlocks  = 0L;
static int  iNewFile  = TRUE;
static int  iEofCount = 0L; 


/* ==========================================================================
   Rewind the tape
   ========================================================================== */

int c_trewind( )
{
int iAnswer;

	sMaTapeCom.mt_op    = MTREW;
	sMaTapeCom.mt_count = 1;
#ifdef DEBUG
	fprintf( stderr, "\n#<c_trewind> Tape\n" );
#endif
	iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
	if( iAnswer == 0 )
	{
		iEofCount = 0L;
		return( (int)SUCCESS );
	}
	else
	{
		fprintf( stderr, "\n#<c_trewind> Can't rewind Tape\n" );
		return( (int)FAILURE );
	}
}

/* ==========================================================================
   Close the tape
   ========================================================================== */
int  c_tclose( )
{
#ifdef DEBUG
	fprintf( stderr, "\n#<c_tclose__> device\n" );
#endif
	iEofCount = 0L;
	close( fdTape );
	return( (int)0 );
}

/* ==========================================================================
   Open the tape
   ========================================================================== */
int c_topen( cpDeviceName, iNaLength )
char *cpDeviceName;
int *iNaLength;
{
int  iAnswer;
char caDevName[32],
     caTmpName[32];

#ifdef DEBUG
	fprintf( stderr, "\n#DBG: <c_topen__> device :%s\n", cpDeviceName );
#endif
	strcpy( caTmpName, cpDeviceName );
	caTmpName[*iNaLength] = '\0';
	sprintf( caDevName, "/dev/%s", caTmpName );  
	if( (fdTape = open( caDevName , O_RDONLY )) > 0 )
	{
		iEofCount = 0L;
		iNewFile = TRUE;
		return( (int)SUCCESS );
	}
	else
		fprintf( stderr, "\n#<c_topen__> open failed !\n");

	return( (int)FAILURE );
}

/* ==========================================================================
   Read next block on tape
   ========================================================================== */
int c_tread( cpBuffer, iSizeOfBuf )
char *cpBuffer;
int *iSizeOfBuf;
{
int iAnswer;


	if( iNewFile == FALSE )
	{
#ifdef DEBUG
  		fprintf( stderr, "\n#<c_tread__> read blocks (%d + %d)\n",iGBlocks
							   ,iPBlocks );
		iPBlocks = 0;
		iGBlocks = 0;
#endif
/*
		if( NextEofMark() == FAILURE )
			return( (int)READ_ERR );
*/
		iNewFile = TRUE;
	}

	iAnswer = NextRecord( cpBuffer, *iSizeOfBuf );

	if( iAnswer == 0 )
	{
		iEofCount++;
		iNewFile = FALSE;
		if( iEofCount == 1 )
			return( (int)EOF_MARK );
		else
			return( (int)EOT_MARK );
	}
	else if( iAnswer == *iSizeOfBuf )
	{
#ifdef DEBUG
	iGBlocks++;
#endif
		iEofCount = 0L;		
		return( (int)SUCCESS );		
	}
	else if( iAnswer > 0 && iAnswer != *iSizeOfBuf )
	{
#ifdef DEBUG
	iPBlocks++;
#endif		
		iEofCount++;
		iNewFile = FALSE;
		return( (int)EOF_MARK );
	}
	else
	{
#ifdef DEBUG
  		fprintf( stderr, "\n#READ_ERR <c_tread__> read blocks (%d + %d)\n",iGBlocks
							   ,iPBlocks );
#endif
		iNewFile = FALSE;
		return( (int)READ_ERR );
	}
}

/* ==========================================================================
   Read the block via read() on tape
   ========================================================================== */
static int NextRecord( cpBuffer, iSizeOfBuf )
char *cpBuffer;
int  iSizeOfBuf;
{
int  iAnswer;

	return( iAnswer = read(fdTape, cpBuffer, iSizeOfBuf) );
#ifdef DEBUG
	fprintf( stderr, "\n#read returned (%d)", iAnswer);
#endif
}

/* ==========================================================================
   write buffer vi write() on tape
   ========================================================================== */
int c_twrite( cpBuffer, iSizeOfBuf )
char *cpBuffer;
int  iSizeOfBuf;
{
int  iAnswer;
 
        return( iAnswer = write(fdTape, cpBuffer, iSizeOfBuf) );
}


/* ==========================================================================
   Skip over EOF mark to continue reading of the file 
   ========================================================================== */
static int NextEofMark( )
{
int iAnswer;

	sMaTapeCom.mt_op    = MTFSF;
	sMaTapeCom.mt_count = 1;

	iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
	ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);	

	if( iAnswer < 0 )
	{
		perror( "\n#ioctl ERROR" );
		return( FAILURE );
	}
	else if( iAnswer == 0 )
	{
#ifdef DEBUG
	fprintf( stderr, "\n#<ioctl> Skipped over EOF mark\n" );
#endif
		return( SUCCESS );
	} 
	else
	{
		fprintf( stderr, "\n#ioctl SUCCESS number [%d]\n", iAnswer );
	} 
}

/* ==========================================================================
   Skip over n EOF marks 
   ========================================================================== */
int c_tskipf( iEofMarks )
int *iEofMarks;
{
int iAnswer;

	sMaTapeCom.mt_op    = MTFSF;
	sMaTapeCom.mt_count = *iEofMarks;

	iAnswer = ioctl( fdTape, MTIOCTOP, &sMaTapeCom );
	ioctl( fdTape, MTIOCGET, (char*)&sMaTapeStat);	

	if( iAnswer < 0 )
	{
		perror( "\n#ioctl ERROR" );
		return( FAILURE );
	}
	else if( iAnswer == 0 )
	{
#ifdef DEBUG
	fprintf( stderr, "\n#Skipped over %d EOF marks\n", *iEofMarks );
#endif
		return( SUCCESS );
	} 
	else
	{
		fprintf( stderr, "\n#ioctl SUCCESS number [%d]\n", iAnswer );
		return( FAILURE );
	} 
}
