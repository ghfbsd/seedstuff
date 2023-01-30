	program extr_file
c
	implicit integer(a-z)
c	logical lopen
	integer ibuf(4000),numw(300),nrec(300),rday(300),iopen(300)
	character stat*5,chan*3,chanw(300)*3,ans*1,sr_stat*5,sr_chan*3,
     &	net_id*2,ifile*132,ofile(300)*132,idir*132,odir*132,cnum*6,
     &	seed_in*100,stats(300)*5,chans(300)*3,chours*2,start_string*13,
     &	end_string*13,stat_sr*5,chan_sr*3,opt(10)*1,opt_string(10)*80,
     &	rec_typ*2,string*132,stat_new*5,times_file*132,stat_last*5,
     &	chan_last*3,chan_typ*1,dummy1*80,dummy2*80,dummy3*80
	real*4 rate,ratew(300),dt,dtw(300),rates(300),rate_last
	real*8 first_tim,dum_tim,new_tim(300),next_tim(300),
     &	rnd_tim,start_tim,last_tim,numta,numte,tim_last
c
	common /cor_year/cor_year,cor_rate
	common /inf_dat/iftim,iftsc,timcr,rate,nsamp,stat,chan,net_id
c
	parameter (unit=1,unit1=10,unit2=2,unit3=3,unit4=4,unit5=5)
	equivalence (ibuf,cnum)
c
	character version*18
 	include 'statnet.inc'
 	include 'version.inc'
	write(*,*) 'extr_file',version
c
	data idt/4/,iend/0/,nws/0/
	data sr_stat,sr_chan/2*' '/
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call extr_file_use
	inarg=1
	idiv=0
	i86400=86400
	start_tim=0.
	last_tim=3153600000.
	ind_out=0
	ishd=0
	idir='./'
	odir='./'
	rec_typ='QT'
	chan_typ='A'
	stat_new=' '
	irec=0
	jmp=0
c
	istat=get_opt('d',rec_typ,nopt,opt,opt_string)
	if(rec_typ.ne.'RF'.and.rec_typ.ne.'CS'.and.rec_typ.ne.'GF'.and.
     &	rec_typ.ne.'S2') rec_typ='QT'
	istat=get_opt('B',string,nopt,opt,opt_string)
	nbtin=4096
	if(istat.eq.1) read(string,*) nbtin
	istat=get_opt('O',string,nopt,opt,opt_string)
	nbtout=nbtin
	if(istat.eq.1) read(string,*) nbtout
	istat=get_opt('i',idir,nopt,opt,opt_string)
	ln=lenc(idir)
	if(idir(ln:ln).ne.'/') idir(ln+1:ln+1)='/'
	istat=get_opt('o',odir,nopt,opt,opt_string)
	ln=lenc(odir)
	if(odir(ln:ln).ne.'/') odir(ln+1:ln+1)='/'
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	isearch=0
	istar=index(seed_in,'*')
	if(istar.gt.0) then
	  isearch=1
	  if(istar.gt.1) then
	    nstat=1
	    stat_net(1)=seed_in(1:istar-1)
	  endif
	endif
	nshours=24
	istat=get_opt('s',chours,nopt,opt,opt_string)
	if(istat.eq.1) then
	  idiv=1
	  if(chours(1:1).ne.' '.and.ichar(chours(1:1)).ne.0) then
	    read(chours,*) nshours
	  endif
	endif
	istat=get_opt('b',start_string,nopt,opt,opt_string)
	isel=0
	if(istat.eq.1) then
	  istat=change_time_string(start_string,itag,mon,iyear,ih,im,is)
	  call datum(1,idy,iyear,mon,itag)
	  call abstim(1,istim,iyear,idy,ih,im,is)
	  start_tim=dble(istim)
	  isel=1
	endif
	istat=get_opt('e',end_string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  istat=change_time_string(
     &      end_string,itagl,monl,iyearl,ihl,iml,isl
     &    )
          call datum(1,idyl,iyearl,monl,itagl)
          call abstim(1,iltim,iyearl,idyl,ihl,iml,isl)
	  last_tim=dble(iltim)
	endif
	istat=get_opt('f',ans,nopt,opt,opt_string)
	if(ans.eq.'C'.or.ans.eq.'c') ind_out=1
	if(ans.eq.'A'.or.ans.eq.'a') ind_out=2
	if(rec_typ.eq.'GF') ind_out=3
	i86400=nshours*3600
	if(rec_typ.eq.'RF') then
	  istat=get_opt('S',stat_new,nopt,opt,opt_string)
	endif
	iselis=0
	istat=get_opt('T',times_file,nopt,opt,opt_string)
	if(istat.eq.1) then
	  iselis=1
	  open(unit2,file=times_file,status='old')
	endif
	if(isel.eq.1.or.iselis.eq.1) then
	  istat=get_opt('j',chan_typ,nopt,opt,opt_string)
	  if(istat.eq.1.and.rec_typ.eq.'RF') jmp=1
	  istat=get_opt('c',chan_typ,nopt,opt,opt_string)
	  if(istat.eq.1) then
	    if(chan_typ.eq.'a') chan_typ='A'
	    if(chan_typ.eq.'h') chan_typ='H'
	    if(chan_typ.eq.'e') chan_typ='E'
	    if(chan_typ.eq.'s') chan_typ='S'
	    if(chan_typ.eq.'b') chan_typ='B'
	    if(chan_typ.eq.'m') chan_typ='M'
	    if(chan_typ.eq.'l') chan_typ='L'
	    if(chan_typ.eq.'v') chan_typ='V'
	    if(chan_typ.eq.'u') chan_typ='U'
	    if(chan_typ.eq.' ') chan_typ='B'
	  endif
	endif
	istat=get_opt('l',ans,nopt,opt,opt_string)
	isngl=0
	if(istat.eq.1) isngl=1
	if(isngl.eq.1) ishd=1
	cor_year=0
	istat=get_opt('Y',string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  read(string(1:4),'(i4)') cor_year
	  write(*,*) 'Correct wrong year information to ',cor_year
	endif
	cor_rate=0
	istat=get_opt('R',string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  read(string(1:2),'(i2)') cor_rate
	  write(*,*) 'Correct wrong rate information to ',cor_rate
	endif
c
2000	if(iselis.eq.1) then
	  read(unit2,'(a,1x,a)',end=999) start_string,end_string
	  write(*,*) 'Extract event ',
     &      start_string(1:lenc(start_string)),' - ',
     &	    end_string(1:lenc(end_string))
	  istat=change_time_string(start_string,itag,mon,iyear,ih,im,is)
	  call datum(1,idy,iyear,mon,itag)
	  call abstim(1,istim,iyear,idy,ih,im,is)
	  start_tim=dble(istim)
	  istim=istim/10
	  istat=change_time_string(
     &      end_string,itagl,monl,iyearl,ihl,iml,isl
     &    )
          call datum(1,idyl,iyearl,monl,itagl)
          call abstim(1,iltim,iyearl,idyl,ihl,iml,isl)
	  last_tim=dble(iltim)
	  isel=1
	endif
c
	mode=0
	nst=0
	nch_sr=9
	istfl=2
c
1000	if(isearch.eq.1) then
	  if(istfl.eq.2) then
	    nch_sr=nch_sr+1
	    if(nch_sr.gt.nchan) then
	      nst=nst+1
	      if(nst.gt.nstat) goto 999
	      nch_sr=1
	      stat_sr=stat_net(nst)(1:lenc(stat_net(nst)))//'*'
	    endif
	    if(index(stat_sr,'*').eq.0) stat_sr=stat_sr(1:lenc
     &	    (stat_sr))//'*'
	    chan_sr=chan_net(nch_sr)
	    mode=0
	    seed_in='*'
	    numta=19700101.
	    numte=20380101.
c	    numta=700101.
c	    numte=991231.
	    if(isel.eq.1) then
c	      write(*,*) mode,idir,stat_sr,chan_sr,seed_in,numta,numte
	      istfl=next_sfile(mode,idir,stat_sr,chan_sr,seed_in,numta,
     &	      numte)
c	      write(*,*) 'next_sfile: ',istfl,seed_in
	      read(start_string(1:6),*) inumt1
	      read(start_string(8:13),*) inumt2
c	      write(*,*) inumt1,inumt2
	      if(inumt1.gt.500000) then
	         iyra=19000000
	         numte=19991231235959.
	      else
	         iyra=20000000
	         numte=20380101000000.
	      endif   
	      numta=iyra+inumt1
	      numta=numta*10**6+inumt2
	      mode=2
	      if(index(stat_sr,'*').gt.0) stat_sr=stat_sr(1:index
     &	      (stat_sr,'*')-1)
	    endif
	  endif
c	  write(*,*) mode,idir,stat_sr,chan_sr,seed_in,numta,numte
	  istfl=next_sfile(mode,idir,stat_sr,chan_sr,seed_in,numta,numte)
c	  write(*,*) 'next_sfile2: ',istfl,seed_in
	  if(istfl.eq.2) goto 1000
	  mode=1
	endif
c
	irew=1
	icp=1
	jred=0
c
	if(rec_typ.eq.'QT'.or.rec_typ.eq.'S2') then
	  lenbi=nbtin/4
	  lenbo=nbtout/4
	elseif(rec_typ.eq.'CS') then
	  nbtin=512
	  lenbi=nbtin/4
	  nbtout=4096
	  lenbo=nbtout/4
	  rec_typ='QT'
	elseif(rec_typ.eq.'RF') then
	  lenbi=256
	  nbtin=1024
	  lenbo=lenbi
	elseif(rec_typ.eq.'GF') then
	  lenbo=1024
	endif
	if(rec_typ.eq.'QT'.or.rec_typ.eq.'S2'.or.rec_typ.eq.'RF') then
	  open(unit,file=seed_in,status='old',access='direct',form=
     &	  'unformatted',recl=nbtin)
c     &	  recl=nbtin/4)		! for Sun f77 -xl
	endif
c
	chmod=0
c
200	nch=1
	if(icp.eq.1) then
	  istat=0
	elseif(icp.eq.-1) then
	endif
	next_tim(nch)=0.
	iend=0
	iftim=0
c
	if(isel.eq.1.and.rec_typ.eq.'RF') then
	  irec=find_rec(rec_typ,unit,0,lenbi,start_tim,3)
	elseif(istim.eq.0.and.rec_typ.eq.'GF') then
	  istim = first_time(unit,idir,ibuf,ndat,ichf)
	  write(*,*) 'First time found in GRF files',istim,ichf,ndat
	elseif(iselis.ne.1) then
	  irec=0
	endif
c
	if(istat.eq.0) then
	  do n=1,300
	    nrec(n)=0
	    numw(n)=0
	    iopen(n)=0
	  enddo
	  num=0
	  nchs=0
	  nshd=0
c
	  do while (istat.eq.0)
1010	    if(iend.eq.0) then
	      irec=irec+1
	      if(rec_typ.ne.'GF') then
	        istat=read_rec_fil(rec_typ,1,unit,lenbi,lenbo,ibuf,
     &            irec,num,frm,stat,chan,net_id,iftim,iftsc,timcr,rate,
     &            nsamp,iof,nframe)
	      else
	        istat=read_grf(unit,idir,mode,istim,irec,ibuf,iftim,iftsc,
     &	        timcr,stat,chan,net_id,rate,nsamp)
	      endif
c	write(*,*) istat,irec,num,stat,chan,iftim,iftsc,timcr,rate,nsamp,iend
c
	      if(istat.eq.3) then
	        if(ishd.eq.1) then
	          if(nshd.eq.0) then
	            write(*,*) 'Start extracting SEED header'
	            open(unit1,file='GFN.SHD',status='unknown',
     &	            recl=4096,access='direct',form='unformatted')
	          endif
	          nshd=nshd+1
	          write(unit1,rec=nshd) (ibuf(i),i=1,1024)
	        else
	          if(nshd.eq.0) write(*,*) 'Skip SEED header'
	          nshd=1
	        endif
	        goto 1010
	      elseif(istat.eq.0) then
	        if(ind_out.gt.0.and.chan(2:2).ne.'H') goto 1010
	        if(nchs.eq.0) write(*,*) 'Start extracting data records'
	      else
	        iend=1
	      endif
c
	    endif
c
	    ind=0
	    nch=0
	    if(isngl.eq.1) then
	      nch=1
	      nchs=1
	      isep=0
	      if(stat.ne.stats(1).or.chan.ne.chans(1)) then
	        isep=1
	        stats(1)=stat
	        chans(1)=chan
	        rates(1)=rate
	      endif
	    else
	      isep=0
	      do n=1,nchs
	        if(stat//chan.eq.stats(n)//chans(n)) then
	          ind=1
	          nch=n
	        endif
	      enddo
	      if(ind.eq.0) then
	        nchs=nchs+1
	        stats(nchs)=stat
	        chans(nchs)=chan
	        rates(nchs)=rate
	        nch=nchs
	      endif
	    endif
c
c	write(*,*) istat,irec,num,stat,chan,iftim,iftsc,timcr,rate,nsamp,iend
c	write(*,*) 'nch',nch,nchs
c	type '(8(1x,z8))',(ibuf(i),i=1,1024)
c
	    if(istat.eq.0) then
	      if(rec_typ.eq.'RF'.and.stat_new.ne.' ') then
	        istat=change_stat(stat_new,ibuf)
	        stat=stat_new
	      endif
	      iend=0
c	      new_tim(nch)=dble(iftim)
c	      new_tim(nch)=new_tim(nch)+dble(iftsc+timcr)*0.0001
	      new_tim(nch)=dble(iftim)+dble(iftsc+timcr)*0.0001
	      if(rate.gt.0.0) then
	        next_rtim=new_tim(nch)+dble(nsamp)/dble(rate)
	      else
	        next_rtim=new_tim(nch)
	      endif
c
	      if(next_rtim.lt.start_tim) goto 1010
c	      write(*,*) 'time check:',next_rtim,start_tim,chan_typ
	      if((isel.eq.1.and.chan(1:1).ne.chan_typ.and.
     &	      chan_typ.ne.'A').or.chan(2:2).ne.'H') goto 1010
	      if(iopen(nch).eq.0) then
	        iopen(nch)=1
	      endif
c	write(*,*) nch,iopen(nch),new_tim(nch),last_tim
	      if(new_tim(nch).gt.last_tim.and.iopen(nch).ne.2) then
	        iopen(nch)=2
	        iend=1
	        do n=1,nchs
	          if(iopen(n).eq.1) iend=0
	        enddo
	        if(iend.eq.1) istat=2
c	write(*,*) 'close channel',nch,iend,istat,nchs,(iopen(i),i=1,nchs)
	        goto 1010
	      else
	        if(iopen(nch).eq.2) goto 1010
	      endif
c
	      nrec(nch)=nrec(nch)+1
c
c	write(*,*) isep,nrec(nch),idiv,new_tim(nch)-next_tim(nch),1000.*dtw(nch),
c     &	new_tim(nch),next_tim(nch)
	      if((isep.eq.1.and.nrec(nch).gt.1) .or.
     &           ((idiv.eq.1.or.isngl.eq.1) .and. nrec(nch).gt.1 .and.
     &           new_tim(nch)-next_tim(nch).gt.1000.*dtw(nch) .and.
     &           dtw(nch).gt.0.0)) then
	        istat=wr_file(999,unit1+nch,ind_out,start_tim,
     &            last_tim,nch,nrec(nch),lenbo,ibuf)
	        call tfix(tim_last,ietim,ietsc)
	        call abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	        call datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) '  End of input volume:',
     &	          stat_last,chan_last,rate_last,itgf,monf,iyrf,ihf,
     &            imf,isf,ietsc,istat
9	        format(3(1x,a),f5.1,1x,2(i2.2,1h/),
     &                 i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,i4/)
	        leno=lenc(ofile(nch))
	        write(*,'(1x,i5,2a)') nrec(nch),
     &            ' records written on file ',ofile(nch)(1:leno)
	        close (unit1+nch)
	        nrec(nch)=1
	      endif
c
	      if(nrec(nch).eq.1) then
	        nfil=nfil+1
	        if(rate.gt.0.0) then
	          dt=1./rate
	        else
	          dt=0.0
	        endif
	        ldat=1024-iof
	        if(icp.lt.1) istart=1
	        call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
                call datum(0,idyf,iyrf,monf,itgf)
	        write(*,19) 'Start of input volume:',
     &	          stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
19	        format(3(1x,a),f5.1,1x,2(i2.2,1h/),
     &            i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4)
	        if(jred.eq.1.and.rate.gt.40.) then
	          ratew(nch)=rate/float(idt)
	          dtw(nch)=1./ratew(nch)
	          chanw(nch)='BH'//chan(3:3)
	          ired=1
	        else
	          ratew(nch)=rate
	          if(ratew(nch).gt.0.0) then
	            dtw(nch)=1./ratew(nch)
	          else
	            dtw(nch)=0.0
	          endif
	          chanw(nch)=chan
	          ired=0
	        endif
c
	        rday(nch)=idint(new_tim(nch))/i86400
c	        dum_tim=new_tim(nch)+(ismp_st-1)*dt
	        dum_tim=new_tim(nch)
	        dum_tim=rnd_tim(0,dum_tim,dtw(nch),tcor_st)
	        call tfix(dum_tim,iftim,iftsc)
	        call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        call datum(0,idyf,iyrf,monf,itgf)
	        if(ihf.eq.0.and.imf.eq.0.and.isf.eq.0.and.iftsc.eq.0) then
	          write(ofile(nch),'(2a,3i2.2,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),cyear(iyrf),monf,itgf,chanw(nch)
	        else
	          write(ofile(nch),'(2a,6i2.2,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),cyear(iyrf),monf,itgf,ihf,imf,isf,
     &	          chanw(nch)
	        endif
	        if(ind_out.lt.2.or.ind_out.eq.3) then
	          if(ind_out.eq.0) new_recl=lenbo*4		! input length
		  if(ind_out.eq.1.or.ind_out.eq.3) new_recl=4096	! 4K
c		  new_recl = 1024 ! only with out the -xl flag (1024*4)
	          open(unit1+nch,file=ofile(nch),status='new',
     &            recl=new_recl,
     &	          access='direct',form='unformatted',iostat=ios)
	          if(ios.ne.0) then	! ios=128 file exists
	            write(*,*) 'Open error or outfile exists - use ',
     &                'extension .ext'
	            ofile(nch)=ofile(nch)(1:lenc(ofile(nch)))//'.ext'
	            open(unit1+nch,file=ofile(nch),status='new',
     &                recl=new_recl,access='direct',form='unformatted')
	          endif
	        endif
	        if(ind_out.eq.2) then
	          write(ofile(nch),'(2a,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),chanw(nch)
	          ofile(nch)=ofile(nch)(1:lenc(ofile(nch)))//'.asc'
	          open(unit1+nch,file=ofile(nch),status='new')
	        endif
	      else
	        istart=0
	        ismp_st=1
	      endif
c
c	write(*,*) 'write',nch,ind_out,start_tim,last_tim,nrec(nch),lenbo
	      istat=wr_file(0,unit1+nch,ind_out,start_tim,last_tim,
     &          nch,nrec(nch),lenbo,ibuf)
c
	      if(nsamp.gt.0) then
	        next_tim(nch)=new_tim(nch)+nsamp*dtw(nch)
	      else
	        next_tim(nch)=new_tim(nch)
	      endif
c	write(*,*) 'next',stat,chan,nch,nrec(nch),new_tim(nch),next_tim(nch),
c     &	rday(nch),idint(next_tim(nch))/i86400
c
	      if(idiv.eq.1.and.idint(next_tim(nch))/i86400.gt.rday(nch)
     &	      .and.dtw(nch).gt.0.0) then
	        istat=wr_file(999,unit1+nch,ind_out,start_tim,last_tim,
     &            nch,nrec(nch),lenbo,ibuf)
	        call tfix(next_tim(nch),ietim,ietsc)
	        call abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	        call datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) '  End of input volume:',
     &	          stats(nch),chans(nch),rates(nch),itgf,monf,iyrf,
     &            ihf,imf,isf,ietsc,istat
	        leno=lenc(ofile(nch))
	        write(*,'(1x,i5,2a)') nrec(nch),
     &            ' records written on file ',ofile(nch)(1:leno)
	        close (unit1+nch)
	        nrec(nch)=0
	      endif
	      rday(nch)=idint(next_tim(nch))/i86400
	      stat_last=stats(nch)
	      chan_last=chans(nch)
	      rate_last=rates(nch)
	      tim_last=next_tim(nch)
c
	    endif
	    if(istat.eq.3) istat=0
	  enddo
c
c	  nrec=nrec+1
c	write(*,*) 'enddo',istat,nrec(nch),nch,ired,iend,idiv
	  do n=1,nchs
	  if(istat.ne.5.and.nrec(n).gt.0) then
c
	    istat=wr_file(999,unit1+n,ind_out,start_tim,last_tim,n,
     &	      nrec(n),lenbo,ibuf)
	    call tfix(next_tim(n),ietim,ietsc)
	    call abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	    call datum(0,idyf,iyrf,monf,itgf)
	    write(*,9) '  End of input volume:',
     &	    stats(n),chans(n),rates(n),itgf,monf,iyrf,ihf,imf,isf,
     &	    ietsc,istat
	    leno=lenc(ofile(n))
	    write(*,'(1x,i5,2a)') nrec(n),' records written on file ',
     &	    ofile(n)(1:leno)
	    write(*,*)
	    close(unit1+n)
	  endif
	  enddo
	endif
c	istat=0
c	if(idiv.eq.1.and.iend.ne.1) then
c	if(iend.ne.1) then
c	  if(icp.eq.1) goto 1000
c	endif
c
c900	if(icp.eq.0) then
c	  if(more.eq.1) goto 100
c	elseif(icp.eq.-1) then
c	  goto 100
c	endif
c
900	close(unit)
	if(isearch.eq.1) goto 1000
	if(iselis.eq.1) goto 2000
c
999	continue
	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	end
	subroutine extr_file_use
c
	write(*,*) 'Extracts and re-encodes data files in ',
     &    'Quanterra Steim1 or Steim2, Reftek or GRF formats'
	write(*,*) 'Usage:'
	write(*,*) 'extr_file <filename> -[b,B,c,d,e,f,i,j,l,o,R,s,S,T,Y]'
	write(*,*) '  <filename> - full filename or "*" or "<statcode>*"'
	write(*,*) '  -b <yymmdd_hhmmss> - start time (def=first rec)'
	write(*,*) '  -B <bsize> - input blocksize in bytes (def=4K)'
	write(*,*) '  -c <code> - channel code (def a) - for b/e and ',
     &    'T option only'
	write(*,*) '             h : VSP (HH?, 80 Hz)'
	write(*,*) '             m : MP  (MH?, 40 Hz)'
	write(*,*) '             b : VBB (BH?, 20 Hz)'
	write(*,*) '             l : LP  (LH?, 1 Hz)'
	write(*,*) '             v : VLP (VH?, 0.1 Hz)'
	write(*,*) '             u : ULP (UH?, 0.01 Hz)'
	write(*,*) '             a : all (BH?+LH?+VH?)'
	write(*,*) '  -d <fcode> - input decoding format (def=QT)'
	write(*,*) '             QT - Quanterra MiniSEED (Steim1 or ',
     &    'Steim2 compression)'
	write(*,*) '             CS - Comserv MiniSEED (Steim1 or ',
     &    'Steim2 compression)'
	write(*,*) '             S1 - Quanterra MiniSEED (Steim1)'
	write(*,*) '             S2 - Quanterra MiniSEED (Steim2)'
	write(*,*) '             RF - REFTEK compression format (CO)'
	write(*,*) '             GF - GRF compression format (K)'
	write(*,*) '  -e <yymmdd_hhmmss> - end time   (def=last rec)'
	write(*,*) '  -f <fcode> - output format code (def=input ',
     &    'format)'
	write(*,*) '             c : Steim1 compression (recl=4K)'
	write(*,*) '             a : PITSA ASCII'
	write(*,*) '  -i <idir> - indir  (def=./)'
	write(*,*) '  -j iterative jump to selected start time - for ',
     &    'RF data and b/e or T options'
	write(*,*) '  -l linear extraction (for large SEED volumes)'
	write(*,*) '  -o <odir> - outdir (def=./)'
	write(*,*) '  -O <bsize> - output blocksize in bytes (def=input)'
	write(*,*) '  -R <def_rate> - define sampling rate (for RF only)'
	write(*,*) '  -s <[n]>  - separation into n hour files, (def=24)'
	write(*,*) '  -S <new_statcode> - change station code (for RF ',
     &    'only)'
	write(*,*) '  -T <filename> - file containing start and end ',
     &    'times for extraction'
	write(*,*) '                line format: <yymmdd_hhmmss> ',
     &    '<yymmdd_hhmmss>'
	write(*,*) '  -Y <def_year> - define correct year (for RF only)'
c
	stop
	end
