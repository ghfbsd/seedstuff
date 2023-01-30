C -----------------------------------------------------------------------------
C Open the tape device  
C Function trewin
C           - Fortran Library Routines S. 357
C -----------------------------------------------------------------------------
	SUBROUTINE rew( tlunit )
C
	EXTERNAL  c_tclose, c_trewind 
	INTEGER   tlunit, c_tclose, c_trewind
	CHARACTER r_device*(15), n_device*(15), devname*(15)
	COMMON /tape/r_device,n_device,devname
C
		n=c_trewind( )
c 	RETURN
	END

C -----------------------------------------------------------------------------
C Open the tape device
C Function trewin
C           - Fortran Library Routines S. 357
C -----------------------------------------------------------------------------
        SUBROUTINE refseek( offset )
C
	INTEGER offset
        EXTERNAL  c_doffset
		
		n=c_doffset( offset )

	RETURN
	END

C -----------------------------------------------------------------------------
C Open the tape device  
C Function topen
C           - Fortran Library Routines S. 355
C -----------------------------------------------------------------------------
	SUBROUTINE dvasn( tlunit, dev_name )
C
	CHARACTER dev_name*(*)
	INTEGER   tlunit, opencode, c_topen
	LOGICAL   islabeled
	CHARACTER r_device*(15), n_device*(15), devname*(15)
	COMMON /tape/r_device,n_device,devname
	EXTERNAL c_topen
	data opencode/0/,islabeled/.false./
C
		r_device = dev_name
c		n_device = 'n'//dev_name
		devname = dev_name
		LEN = lenc(dev_name)
C
		opencode = c_topen( r_device, LEN )
		IF( opencode.LT.0 ) THEN
			WRITE( *,* )' topen: unable to open device ',
     &	                r_device
			STOP
		ELSE	
			WRITE( *,* )' open Ok',opencode,' ',tlunit,' ',
     &	                devname   
		ENDIF
	RETURN
	END
	subroutine back()
C
	write( *, * ) ' ****************************'
	write( *, * ) ' DUMMY FUNCTION back()'
	write( *, * ) ' ****************************'
	return
	end
C -----------------------------------------------------------------------------
C skip over files   
C import function: tskipf  
C           - Fortran Library Routines S. 358
C -----------------------------------------------------------------------------	
	SUBROUTINE spfil( tape_unit, files_to_skip, skiped_files )
C
	INTEGER   tape_unit, files_to_skip, skiped_files, skip_code
	COMMON    eof_marks
	CHARACTER r_device*(15), n_device*(15), devname*(15)
	COMMON /tape/r_device,n_device,devname
	INTEGER  c_tskip
	EXTERNAL c_tskip
C
		IF( files_to_skip.LT.0 ) THEN
			WRITE( *,* )' c_tskipf : skip back ! DUMMY '
		ELSE
			skip_code = c_tskipf( files_to_skip )
			IF( skip_code.LT.0 ) THEN
				WRITE( *,* )' c_tskipf : cannot skip ! '
				STOP
			ELSE
				skiped_files = files_to_skip
			ENDIF
		ENDIF    			
	RETURN
	END
C -----------------------------------------------------------------------------
C find file  
C Function lib$findfile 1 version
C           
C -----------------------------------------------------------------------------
        function next_sfile(mode,ddir,stat,chan,name,num_tima,num_time)
	IMPLICIT INTEGER (a-z)
	REAL*8    num_tima,num_time
	CHARACTER ddir*(*),stat*(*),chan*(*),name*(*),cyr*4,cyre*4
	CHARACTER stat_cr*7
	INTEGER   returncode,mode
	EXTERNAL  get_sfile
C
	returncode = -1
	namlen = lenc(name)
	ddirlen = lenc(ddir)
c
	if(num_tima.gt.19000000000000.) then
	   numa=num_tima/10000000000.
	else
	   numa=num_tima/10000.
	endif
	if(num_time.gt.19000000000000.) then
	   nume=num_time/10000000000.
	else
	   nume=num_time/10000.
	endif
	write(cyr,'(i4.4,$)') numa
	write(cyre,'(i4.4,$)') nume
c	write(*,'(2f16.0)') num_tima,num_time
c	write(*,*) 'cnum: ',numa,nume,cyr,cyre
	if(cyr(1:3).eq.cyre(1:3)) then
	   ls=index(stat,'*')
	   if(ls.gt.0) then
	      stat_cr=stat(1:ls-1)//cyr(3:3)//'*'
	   else
	      ls=lenc(stat)
	      stat_cr=stat(1:ls)//cyr(3:3)
	   endif
	else
	   stat_cr=stat
	endif
c	write(*,*) 'name >',name(1:namlen),'<namlen >',namlen,'<ddir>',
c     &	ddir(1:ddirlen),'<ddirlen >',ddirlen,'<',statlen,chanlen,
c     &	stat_cr,chan,num_tima,num_time,mode
	call get_sfile(mode,ddir,ddirlen,stat_cr,chan,name,namlen,
     &	num_tima,num_time,returncode)
c     	write(*,*) 'exit',num_tima,num_time,returncode
	next_sfile = returncode
C
	RETURN
	END
C -----------------------------------------------------------------------------
C find file  
C Function lib$find_file 2 version
C           
C -----------------------------------------------------------------------------
        function next_grffile(mode,ddir,name,nfrec)
c	CHARACTER ddir*(*),name*(*)
c	INTEGER   ind,nfrec,ichk,namlen
c	EXTERNAL  get_file
c
	IMPLICIT INTEGER (a-z)
	REAL*8    num_tima,num_time
	CHARACTER ddir*(*),stat*5,chan*3,name*(*)
	INTEGER   returncode,mode
	EXTERNAL  get_sfile
	data stat,chan/'grf','dat'/,num_tima,num_time/0.,3153600000./
C
c	namlen = lenc(name)
c	call get_file(ind,ddir,name,namlen,nfrec,ichk)
c
	ddirlen = lenc(ddir)
	name=ddir(1:ddirlen)//'grf70010100.dat'
	namlen = lenc(name)
	returncode = -1
c	write(*,*) mode,ddir,ddirlen,name,namlen,stat,chan,num_tima,num_time
	call get_sfile(mode,ddir,ddirlen,stat,chan,name,namlen,
     &	num_tima,num_time,returncode)
	next_grffile = returncode
	nfrec=360
C
	RETURN
	END
C -----------------------------------------------------------------------------
C Read records from tape  
C Function tread
C           - Fortran Library Routines S. 356
C -----------------------------------------------------------------------------
	SUBROUTINE rd( tlunit, ibuf, no_of_ints, read_code, read_bytes )
C
	CHARACTER cbuf*65536
	INTEGER   tlunit, readcode, read_code, read_bytes,ibuf(*),
     &	no_of_ints	
	CHARACTER r_device*(15), n_device*(15), devname*(15)
	COMMON /tape/r_device,n_device,devname
	EXTERNAL  c_memcpy, c_tread
	INTEGER   c_tread 
C
	readcode = 0
	readcode = c_tread( cbuf, no_of_ints*2 )
	IF( readcode.LT.0 .or. readcode.EQ.4 ) THEN
C	-- error while reading --
C		call perror( 'read ERROR' )
		read_code = 4
	ELSE IF( readcode.EQ.1 ) THEN
C  	-- end of file --
		read_code = 1
	ELSE IF( readcode.EQ.2 ) THEN
C  	-- end of tape --
		read_code = 2
	ELSE 
C	-- read bytes --
		read_code  = 0
		read_bytes = readcode
C       -- Convert to byte count --
		call c_memcpy( ibuf, cbuf, no_of_ints*2 ) 
	ENDIF
	RETURN
	END
C -----------------------------------------------------------------------------
C Transform a string with lower letters to uppercase
C  
C Function str$upcase
C        - to perform the upcase function a C call is used from C_functions.c
C -----------------------------------------------------------------------------
	FUNCTION str_upcase( destination, source )
C
	CHARACTER source*(*),destination*(*)
	INTEGER   c_str_upcase,str_upcase
C	EXTERNAL  c_str_upcase
C
		lenght_des  = len(destination)
		lenght_sour = len(source)
		str_upcase = c_str_upcase(destination,lenght_des,source,
     &	        lenght_sour)
		return
	end
