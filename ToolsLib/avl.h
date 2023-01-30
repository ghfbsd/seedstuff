typedef struct  _leaf
{
    struct  _leaf   *left    ;
    struct  _leaf   *right   ;
    unsigned        size : 14;
    unsigned        bal  :  2;
}HEADER;

#define L   (0)
#define B   (1)
#define R   (2)
     
extern int     AvlDelete    ( HEADER**, HEADER*, int(*)() );
extern HEADER  *AvlInsert   ( HEADER**, HEADER*, int(*)() );
extern HEADER  *AvlFind     ( HEADER*, HEADER*, int(*)() );
extern HEADER  *AvlReplace  ( HEADER*, HEADER*, int(*)(), HEADER*, int );
extern void    AvlTreePrint ( HEADER*, int(*)(), FILE* );
extern HEADER  *AvlTreeAlloc( int );
extern void    AvlTreeFree  ( HEADER* );
extern int     AvlWriteFile ( HEADER*, char*, int (*)() );
extern int     AvlReadFile  ( HEADER**, char*, int, int (*)(), int (*)() );
extern HEADER	*AvlMax      ( HEADER * );
extern HEADER	*AvlMin      ( HEADER * );
