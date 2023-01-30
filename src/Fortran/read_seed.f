	program read_seed
c
	implicit integer(a-z)
	integer ibuf(1050)
	byte buf(4200)
	character buffer*4200,seed_in*132,idir*32
	equivalence (buffer,ibuf,buf)
	parameter (idsc=1)
c
	character stat*5,chan*3,net_id*2,strng*1,
     &	opt(10)*1,opt_string(10)*80,string*132
c
	character version*18
 	include 'version.inc'
	write(*,*) 'read_seed -',version
c
	inarg=rd_options(nopt,opt,opt_string)
c	if(inarg.eq.0) call read_seed_use
	inarg=1
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	nbtin=4096
	lenr=nbtin
	long=nbtin/4
c
	open(idsc,file=seed_in,status='old',form='unformatted',
     &    access='direct',recl=lenr)
c	open(idsc,name=seed_in,status='old')
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened file: ',seed_in(1:nfl)
	nrec=0
c	read(idsc,'(a)') buffer(1:21)
c	write(*,'(8z8)') (buf(i),i=1,21)
c
100	nrec=nrec+1
	read(idsc,rec=nrec,iostat=ios) buffer(1:nbtin)
	if(ios.ne.0) goto 900
c	lenr=8
c	read(idsc,'(a)') buffer(1:lenr)
c#if defined(LINUX)
c	call swap_mseed(1,ibuf)
c	if(nrec.eq.1.and.istat.eq.1) then
c	  write(*,*) 'Byte swap option enabled'
c	endif
c#endif
c	write(*,'(8z8)') (buf(i),i=1,8)
	write(*,*) buffer(1:8)
	goto 100
c
900	stop
	end
	subroutine read_seed_use
c
	return
	end
