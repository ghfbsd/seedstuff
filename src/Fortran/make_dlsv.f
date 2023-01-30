	program make_dlsv
c
	implicit integer (a-z)
c	integer nchan(100),chan(15),rate(15)
	integer ibuf(1024)
	character ofile*132,cfgfile*132,stat_sel*5,net_sel*7,upd_fl*1,
     &	opt(10)*1,opt_string(10)*80,dummy1*80,dummy2*80,dummy3*80,
     &	ifile*132,odir*132,buffer*4096
	equivalence (ibuf,buffer)
c
	integer sera_idx(100,100,15),sere_idx(100,100,15),stat_lkup(100),
     &	no_chan(100),chan_lkup(100,15),nser(300),no_rec(300)
	real*4 rateo(100,15)
	real*8 spa_tim(100),spe_tim(100),sera_tim(100,100,15),sere_tim
     &	(100,100,15),vol_anf,vol_end,tmp_tim
        logical okeep, oexist
c
	character version*18
 	include 'head_seed.inc'
 	include 'version.inc'
	write(*,*) 'make_dlsv -',version
c
	data unit/1/,ofile/'dataless.seed'/,unit1/2/,unit2/3/,odir/'./'/
	data nspn/0/,id_short/0/
c
	ifile=ofile(1:lenc(ofile))//'.lst'
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call make_dlsv_use
	okeep=1 .eq. get_opt('k',dummy1,nopt,opt,opt_string)
	if(0 .eq. get_opt('l',label,nopt,opt,opt_string)) then
	  label='DATALESS'
        endif
	istat=get_opt('u',stat_sel,nopt,opt,opt_string)
	if(1.eq.get_opt('t',dummy1,nopt,opt,opt_string)) then
c         Trace option letters: p - parse, h - seed header generation
	  optpar=0.ne.index(dummy1,'p')
	  optshd=0.ne.index(dummy1,'h')
	endif
	upd_fl='N'
	if(istat.eq.1) then
	  upd_fl='U'
	endif
	net_sel='all'
	stat_sel='all'
	istat=get_opt('s',stat_sel,nopt,opt,opt_string)
	if(istat.eq.1) then
	  net_sel=' '
	endif
	istat=get_opt('n',net_sel,nopt,opt,opt_string)
	if(istat.eq.1) then
	  stat_sel=' '
	endif
	if(stat_sel.eq.'all'.or.net_sel.eq.'all') then
	  net_sel=' '
	  stat_sel=' '
	endif
	istat=get_opt('a',cfgfile,nopt,opt,opt_string)
	if(istat.ne.1) cfgfile=' '
c	istat=parse_seed_db(1,2,cfgfile,' ',' ')
	istat=parse_seed_db(1,2,cfgfile,stat_sel,net_sel)
c
	write(*,*) 'Network(s) selected:',no_net,' ',(net(i),i=1,no_net)
	write(*,*) 'Station(s) selected:',nst_tot,' ',
     &    (stat_net(i),i=1,nst_tot)
c       Extract relevant station information and operating time period.
        vol_anf=0
	vol_end=0
	no_stat=nst_tot
	do n=1,no_stat
	  stat_lkup(n)=n
	  stlkp=stat_lkup(n)
	  no_chan(stlkp)=nch_tot(stlkp)
	  do m=1,no_chan(stlkp)
	    chan_lkup(stlkp,m)=m
	    rateo(stlkp,m)=orate(m,stlkp)
c	write(*,*) 'make',chan_lkup(stlkp,m),stlkp,no_stag(chan_lkup(stlkp,m),stlkp)
	  enddo
	  call parse_time(stat_st_tim(n),tmp_tim)
	  if(tmp_tim .lt. vol_anf .or. n .eq. 1) vol_anf=tmp_tim
	  call parse_time(stat_end_tim(n),tmp_tim)
	  if(tmp_tim .gt. vol_end .or. n .eq. 1) vol_end=tmp_tim
	enddo
	inquire(file=ofile,exist=oexist)
	if (oexist) then
	   write(*,*) 'Output file ',ofile(1:lenc(ofile)),
     &        ' exists, remove it if you want to recreate.'
           stop
        endif
c
c	open(unit,name=ofile,status='unknown',access='direct',form=
c     &	'unformatted',recl=1024)
c	open(unit,name=ofile,status='new',access='direct',form=
c     &	'unformatted',recl=4096)
	open(unit,file=ifile,status='unknown')
c
	istat=cre_head_seed(vol_anf,vol_end,no_stat,
     &	stat_lkup,no_chan,chan_lkup,rateo,nspn,spa_tim,spe_tim,
     &	nser,sera_tim,sera_idx,sere_tim,sere_idx,id_short,upd_fl,
     &	0,unit1,unit,0,odir,odir)
	close(unit)
c
	open(unit2,file=ofile,status='unknown',access='direct',form=
     &	'unformatted',recl=4096)
	open(unit,file=ifile,status='old')
	irec=0
900	continue
	  read(unit,'(a)',end=999) ofile
	  write(*,*) 'Append ',ofile(1:lenc(ofile))
	  open(unit1,file=ofile,status='old',access='direct',
     &	     form='unformatted',recl=4096)
	  nrec=0
910	  continue
            nrec=nrec+1
c	    write(*,*) 'read rec ',nrec
	    read(unit1,rec=nrec,err=920) ibuf
	    irec=irec+1
c	    write(*,*) 'write rec ',irec
	    write(buffer(1:6),'(i6.6)') irec
	    write(unit2,rec=irec) ibuf
	  goto 910
920       continue
	  if(.not.okeep) then
	    close(unit1,status='delete')
	  else
	    close(unit1)
	  endif
        goto 900
999	continue
        if(okeep) then
	  close(unit)
        else
	  close(unit,status='delete')
        endif
	close(unit2)
c
	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	end
	subroutine make_dlsv_use
c
	write(*,*) 'Creates SEED header (dataless SEED volume) out ',
     &    'of a database file'
	write(*,*) 'Usage:'
	write(*,*) 'make_dlsv -[a,k,l,n,s,t,u]'
	write(*,*) 'A database file copy_seed.cfg has to be present ',
     &    'in $SEED_STUFF_HOME or'
	write(*,*) '-a option used'
	write(*,*) '  -a <filename> - alternate database file'
	write(*,*) '  -k - keep intermediate data files (default ',
     &    'delete)'
	write(*,*) '  -l <label> - volume label (default DATALESS)'
	write(*,*) '  -n <netcode> - select specific network only ',
     &    '(all)'
	write(*,*) '  -s <stationcode> - select specfic station only ',
     &    '(all)'
	write(*,*) '  -t [h][p] - trace header/cfg file parse (off)'
	write(*,*) '  -u  - set update flag'
c
	stop
	end
