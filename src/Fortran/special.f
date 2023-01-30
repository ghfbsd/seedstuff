C    Special one-off program to swap the headers of a station when byte-swapping
C    problems have disrupted the data.

      integer ibuf(1024)
      integer*2 hbuf(2048), i2c1, i2c50
      character arg*64, idir*64, odir*64, file*64
      character infile*128,oufile*128,buf*4096
      equivalence (ibuf,buf,hbuf)

      data i2c1/1/, i2c50/50/

      idir = '.'
      odir = '.'
      file = ' '

      iskip = 0
      do 5 i=1,iargc()
         if (i.le.iskip) go to 5
	 call getarg(i,arg)
	 if (arg .eq. '-i') then
	    call getarg(i+1,idir)
	    iskip = i+1
	 elseif (arg .eq. '-o') then
	    call getarg(i+1,odir)
	    iskip = i+1
	 elseif (arg(1:1) .eq. '-') then
	    write(0,*) '**Unrecognized parameter: ',arg(1:index(arg,' '))
	    stop
	 else
	    file=arg
	 endif
5     continue
      if (file.eq.' ') stop '**No input file'

      infile=idir(1:lenb(idir)) // '/' // file
      open(1,file=infile,form='unformatted',recl=4096,iostat=ios)
      if (ios.ne.0) stop '**Input file does not exist'

      oufile=odir(1:lenb(odir)) // '/' // file
      open(2,file=oufile,form='unformatted',recl=4096,iostat=ios)
      if (ios.ne.0) stop '**Output file does not exist'

      irec=0
1000  continue
         irec = irec + 1
         read(1,rec=irec,iostat=ios) ibuf
	 if (ios.ne.0) go to 2000

	 if (buf(9:13) .ne. 'PATM ') then
	    pause '**Bad station...'
	 endif
	 if (hbuf(23) .ne. 64) then
	    pause '**Bad first data...'
	 endif
	 if (hbuf(24) .ne. 48) then
	    pause '**Bad first blockette...'
	 endif
	 if (iswap(hbuf(25)) .ne. 1000) then
	    pause '**Bad blockette 1000...'
	 endif

C        Set sample rate to 50 sps, swap header to little-endian, flag.
	 hbuf(11) = iswap(hbuf(11))
	 hbuf(12) = iswap(hbuf(12))
	 hbuf(16) = iswap(hbuf(16))
	 hbuf(17) = iswap(i2c1)
	 hbuf(18) = iswap(i2c50)
	 hbuf(23) = iswap(hbuf(23))
	 hbuf(24) = iswap(hbuf(24))
	 buf(54:54) = char(0)

         write(2,rec=irec,iostat=ios) ibuf
	 if (ios.ne.0) then
	    write(0,*) '**Output file write error, record ',irec
	 endif
      go to 1000

2000  continue
      close(1)
      close(2)
      end

      function lenb(str)
      character str*(*)

      do i=len(str),1,-1
         if (str(i:i) .ne. ' ') then
	    lenb = i
	    return
	 endif
      enddo
      lenb = 1
      end

	function iswap(int)
c
	integer*2 int,jint
	byte jbyt(2),sbyte
	equivalence (jint,jbyt)
c
	jint=int
	sbyte=jbyt(1)
	jbyt(1)=jbyt(2)
	jbyt(2)=sbyte
	iswap=jint
c
	return
	end
