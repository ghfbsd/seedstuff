/*******************************************************************************
O*          MODULE  AVLTREE.H
 *  
J*   PROJECT:               
 *  
S*   SUBPROJECT:                         
 *  
G*   PROGRAMMER:    
 *  
T*   DATE:           
 *  
V*   VERSION:        
 *  
M*   MODIFICATION:   
 *  
 *******************************************************************************
 *  
D*   DESCRIPTION:   Allgemeine AVL Routinen, fuer die Grundoperationen auf AVL
D*                  Baeumen.
 *  
A*   ARCHITECTURE:  vms
 *  
X*   EXECUTABLE:    
 *  
B*   LIBRARY:       AVL.OLB
 *  
L*   USED MODULES:  AvlDelete.c
L*                  AvlInsert.c
L*                  AvlFind.c
L*                  AvlPrint.c 
L*                  AvlFile.c
L*                  Avl.h
 *  
I*   IMPORT:        -
 *  
E*   EXPORT:        -
Z*  
 ******************************************************************************/

typedef int *TREE;                            /* Dummy typedef fuer eine Baum */

/*******************************************************************************
U*          PROCEDURE   AvlDelete
 *  
D*   DESCRIPTION:       Loeschen eines Eintrages im AVL Baum.
D*                      Zum Loeschen eines Eintrages muss die entsprechende
D*                      Vergleichsfunktion auf den zu vergleichenden Schluessel
D*                      uebergeben werden.
D*
D*                      Die Vergleichsfunktion hat die Form:
D*                      
D*                      int Dcmp( <keytype> n1, LEAF* n2 ){
D*                         return( strcmp( ..., ... )
D*                      }
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlDelete    ( &Wurzel, 
C*                                             (LEAF*)Blatt, 
C*                                             Schluesselvergleich );
 *  
P*   PARAMETER:         TREE** Wurzel               - Wurzel des AVL Baumes
P*                      LEAF*  Blatt                - Daten des Blattes
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
 *  
R*   RETURN:            NULL    - Knoten geloescht
R*                      <> NULL - Knoten nicht im Baum
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern int     AvlDelete    ( TREE**, LEAF*, int(*)() );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlInsert
 *  
D*   DESCRIPTION:       Eintrag eines Knotens in den AVL Baum.
D*                      Zum Eintrag muss die entsprechende Vergleichsfunktion 
D*                      auf den zu vergleichenden Schluessel uebergeben werden.
D*                      Die Vergleichsfunktion hat die Form:
D*                      
D*                      int Icmp( LEAF* n1, LEAF* n2 ){
D*                         return( strcmp( ..., ... )
D*                      }
D*                      
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlInsert    ( &Wurzel, 
C*                                             (LEAF*)Blatt, 
C*                                             Schluesselvergleich );
 *  
P*   PARAMETER:         TREE** Wurzel               - Wurzelknoten des AVL
P*                                                    Baumes
P*                      LEAF*  Blatt                - Daten des Blattes
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
 *  
R*   RETURN:            NULL    - Daten eingetragen
R*                      <>NULL  - Daten im Baum
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlInsert   ( TREE**, LEAF*, int(*)() );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlReplace
 *  
D*   DESCRIPTION:       Aendern eines Knotens in den AVL Baum.
D*                      Zum Eintrag muss die entsprechende Vergleichsfunktion 
D*                      auf den zu vergleichenden Schluessel uebergeben werden.
D*                      Die Vergleichsfunktion hat die Form:
D*                      
D*                      int Icmp( LEAF* n1, LEAF* n2 ){
D*                         return( strcmp( ..., ... )
D*                      }
D*                      
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlReplace   ( &Wurzel, 
C*                                             (LEAF*)Blatt, 
C*                                             Schluesselvergleich,
C*                                             (LEAF*)Ersetzen,
C*                                             groesse              );
 *  
P*   PARAMETER:         TREE** Wurzel               - Wurzelknoten des AVL
P*                                                    Baumes
P*                      LEAF*  Blatt                - Daten des Blattes
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
P*                      LEAF*  Ersetzen             - Daten sie veraendert
P*                                                    werden sollen
P*                      int    greosse              - Groesse Datenfeld
 *  
R*   RETURN:            <>NULL    - Daten eingetragen
R*                        NULL    - Daten nicht eingetragen
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlReplace ( TREE*, LEAF*, int(*)(), LEAF*, int );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlFind
 *  
D*   DESCRIPTION:       Suchen eines Knotens im AVL Baum.
D*                      Zum Auffinden muss die entsprechende Vergleichsfunktion 
D*                      auf den zu vergleichenden Schluessel uebergeben werden.
D*                      Die Vergleichsfunktion hat die Form:
D*                      
D*                      int Fcmp( LEAF* n1, LEAF* n2 ){
D*                         return( strcmp( ..., ... )
D*                      }
D*                      
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlFind( (TREE*)Startknoten 
C*                                       (LEAF*)Blatt, 
C*                                       Schluesselvergleich );
 *  
P*   PARAMETER:         TREE*  Startknoten          - Startknoten des AVL Baumes
P*                      LEAF*  Blatt                - Daten des Blattes
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
 *  
R*   RETURN:            NULL    - Daten nicht gefunden
R*                      <> NULL - gefundene Daten
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlFind     ( TREE*, LEAF*, int(*)() );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlWriteFile
 *  
D*   DESCRIPTION:       Ausgabe des AVL Baumes auf Out-Stream.
D*                      
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlWriteFile( (TREE*)Startknoten 
C*                                            (char*)Dateiname, 
C*                                            Ausgabefunktion );
 *  
P*   PARAMETER:         TREE*  Startknoten          - Startknoten des AVL Baumes
P*                      char*  Dateiname            - Name der Ausgabedatei
P*                      (*)()  Ausgabefunktion      - Ausgabefunktion, zur 
P*                                                    Satzweisen Ausgabe. 
 *  
R*   RETURN:            <>NULL    - kein Ausgabefehler
R*                      NULL      - Fehler bei der Ausgabe
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern int     AvlWriteFile ( TREE*, char*, int (*)() );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlReadFile
 *  
D*   DESCRIPTION:       Lesen der Daten aus einer Datei und Aufbau des Avl-
D*                      Baumes.
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              int     AvlReadFile( (TREE*)Startknoten 
C*                                           (char*)Dateiname, 
C*                                           Lesefunktion
C*                                           Vergleichsfunktion );
 *  
P*   PARAMETER:         TREE*  Startknoten          - Startknoten des AVL Baumes
P*                      LEAF*  Dateiname            - Name der Eingabedatei
P*                      (*)()  Lesefunktion         - Lesefunktion, zur 
P*                                                    Satzweisen Eingabe. 
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
 *  
R*   RETURN:            <>NULL    - kein Ausgabefehler
R*                      NULL      - Fehler bei der Ausgabe
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern int     AvlReadFile  ( TREE**, char*, int, int (*)(), int (*)() );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlTreePrint
 *  
D*   DESCRIPTION:       Grafische Ausgabe der Schluesselwerte auf stream.
D*                      
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              void     AvlTreePrint( (TREE*)Startknoten 
C*                                             Vergleichsfunktion,
C*                                             (char*)stream ); 
 *  
P*   PARAMETER:         TREE*  Startknoten          - Startknoten des AVL Baumes
P*                      (*)()  Schluesselvergleich  - Vergleichsfunktion, 
P*                                                    arbeitet wie strcmp
P*                      LEAF*  stream               - Name der Ausgabedatei
 *  
R*   RETURN:            -
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern void    AvlTreePrint ( TREE*, int(*)(), FILE* );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlTreeAlloc
 *  
D*   DESCRIPTION:       Stellt Speicherplatz fuer die aufzunehmenden Daten
D*                      zur Verfuegung.
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              LEAF    *AvlTreeAlloc( int Datenpuffer );
 *  
P*   PARAMETER:         int Datenpuffer - sizeof( Puffer )
 *  
R*   RETURN:            NULL   - kein Speicher mehr zur Verfuegung
R*                      LEAF*  - Datenpuffer
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlTreeAlloc( int );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlTreeFree
 *  
D*   DESCRIPTION:       Stellt den Speicherplatz des Datenpuffers wieder
D*                      zur Verfuegung.
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              void    AvlTreeFree( (LEAF*)Puffer );
 *  
P*   PARAMETER:         LEAF *Puffer - freizugebender Datenpuffer
 *  
R*   RETURN:            -
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern void    AvlTreeFree  ( LEAF* );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlFreeAll
 *  
D*   DESCRIPTION:       Loescht den gesamten AVL Baum und stellt den Speicher-
D*                      platz wieder zur Verfuegung.
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              void    AvlTreeFree( &Wurzel );
 *  
P*   PARAMETER:         TREE **Wurzel - Wurzel des AVL Baumes
 *  
R*   RETURN:            -
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern void    AvlFreeAll   ( TREE** );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlMax
 *  
D*   DESCRIPTION:       Groesster Schlussel im AVL Baum
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              LEAF    *AvlMax( (TREE*)Wurzel );
 *  
P*   PARAMETER:         TREE *Wurzel - Startknoten des AVL Baumes
 *  
R*   RETURN:            NULL  - Baum ist leer
R                       LEAF* - Maximum im Baum
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlMax( TREE* );
/* -------------------------------------------------------------------------- */


/*******************************************************************************
U*          PROCEDURE   AvlMax
 *  
D*   DESCRIPTION:       Kleinster Schlussel im AVL Baum
 *  
S*   SIDE EFFECTS:      -
 *  
C*   CALL:              LEAF    *AvlMax( (TREE*)Wurzel );
 *  
P*   PARAMETER:         TREE *Wurzel - Startknoten des AVL Baumes
 *  
R*   RETURN:            NULL  - Baum ist leer
R                       LEAF* - Minimum im Baum
 *      
E*   ERROR CODES:       -
 *  
L*   CALLED PROCEDURES: -
Z*  
 ******************************************************************************/
/* -------------------------------------------------------------------------- */
extern LEAF    *AvlMin( TREE* );
/* -------------------------------------------------------------------------- */
