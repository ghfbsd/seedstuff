#if !defined(FSYMSFX) || (FSYMSFX==1)
#define c_trewind  c_trewind_ 
#define c_topen    c_topen_
#define c_tread    c_tread_
#define c_tclose   c_tclose_
#define c_tskipf   c_tskipf_
#define c_write    c_write_	
#define c_doffset  c_doffset_
#define c_refread  c_refread_
#endif
#if defined(FSYMSFX) && (FSYMSFX==2)
#define c_trewind  c_trewind__
#define c_topen    c_topen__
#define c_tread    c_tread__
#define c_tclose   c_tclose__
#define c_tskipf   c_tskipf__
#define c_write    c_write__
#define c_doffset  c_doffset__
#define c_refread  c_refread__
#endif

/* null routines if no mag tape io */

#define FAILURE		(-1)
#define SUCCESS		(0)
#define EOF_MARK	(1)
#define EOT_MARK	(2)
#define READ_ERR	(4)
#define FALSE		(0)
#define TRUE		(1)

int
c_refread( char *cpBuffer, int *iSizeOfBuf ) {
   return FAILURE;
};

#if defined(REFTEK) && defined(LINUX)
int
c_doffset(long *offset) {
   return FAILURE;
}
#endif

int
c_topen( char *cpDeviceName, int *iNaLength ) {
   return FAILURE;
}

int
c_trewind(void){
   return FAILURE;
}

int
c_tclose(void){
   return FAILURE;
}

int
c_tread( char *cpBuffer, int *iSizeOfBuf ) {
   return FAILURE;
}

int
c_twrite( char *cpBuffer, int *iSizeOfBuf ) {
   return FAILURE;
}

int
c_tskipf( int *iEofMarks ) {
   return FAILURE;
}
