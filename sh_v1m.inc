// Rangecoder
typedef unsigned short word;
typedef unsigned int   uint;
typedef unsigned char  byte;
typedef unsigned long long qword;
//    ProcMode -> 0=encode, 1=decode;
template< int ProcMode >
struct Rangecoder_SH1m {

  struct DefaultIO {
    File* CodeFile;
    unsigned char get(void) { return CodeFile->getc(); }
    void put( int c ) { CodeFile->putc( c ); }
  };
  DefaultIO io;
  unsigned char get(void) { return io.get(); }
  void put( int c ) { io.put(c); }

  enum {
    SCALElog = 15,
    SCALE    = 1<<SCALElog
  };

  enum {
    NUM   = 4,
    sTOP  = 0x01000000U,
    Thres = 0xFF000000U
  };

  union {
    struct {
      uint  low;  
      uint  Carry;
    };
    qword lowc;
  };
  uint  code; 
  uint  FFNum;
  uint  Cache;
  uint  range;

  uint muldivR( uint a, uint b ) { return (qword(a)*b)/(qword(range)+1); }

  uint mulRdiv( uint a, uint c ) { return (qword(a)*(qword(range)+1))/c; }

  void rc_Process( uint cumFreq, uint freq, uint totFreq ) {
    uint tmp = mulRdiv( cumFreq, totFreq ) + 1;
    if( ProcMode ) code-=tmp; else lowc+=tmp;
    range = mulRdiv( cumFreq+freq, totFreq )-1 - tmp;
    if( ProcMode ) 
      while( range<sTOP ) range=(range<<8)+0xFF, (code<<=8)+=get();
    else 
      while( range<sTOP ) range=(range<<8)+0xFF, ShiftLow();
  }

  uint rc_GetFreq( uint totFreq ) {
    return muldivR( code, totFreq );
  }

  void BProcess( uint freq, int& bit ) { 
    int x[] = { 0, int(freq), SCALE };
    if( ProcMode ) {
      uint count = rc_GetFreq( SCALE );
      bit = (count>=freq);
    }
    rc_Process( x[bit], x[1+bit]-x[bit], SCALE );
  }

  void ShiftLow( void ) {
    if( low<Thres || Carry ) {
      if( Cache!=-1 ) put( Cache+Carry );
      for (;FFNum != 0;FFNum--) put( Carry-1 ); // (Carry-1)&255;
      Cache = low>>24;
      Carry = 0;
    } else FFNum++;
    low<<=8;
  }

  void rcInit( void ) { 
    range = 0xFFFFFFFF;
    low   = 0;
    FFNum = 0;
    Carry = 0;    
    Cache = -1;
  }
  
  void Init( /*uint _ProcMode=0*/ ) {
    rcInit();
    //ProcMode = _ProcMode;
    if( ProcMode==1 ) {
      for(int _=0; _<NUM; _++) (code<<=8)+=get(); 
    }
  }

  void Quit( void ) {
    if( ProcMode==0 ) {
      uint i, n = NUM;

      // cache .. FF x FFNum .. low
      qword llow=low;
      qword high=llow+range+1;

      if( qword(llow|0xFF) < high ) llow|=0xFF,n--;
      if( qword(llow|0xFFFF) < high ) llow|=0xFFFF,n--;
      if( qword(llow|0xFFFFFF) < high ) llow|=0xFFFFFF,n--;
      if( qword(llow|0xFFFFFFFF) < high ) llow|=0xFFFFFFFF,n--;
      low = uint(llow);
      if( low==0xFFFFFFFF ) FFNum=0; // they're also FFs

      if( Cache!=-1 ) put( Cache );
      for( i=0; i<FFNum; i++ ) put( 0xFF );
      for( i=0; i<n; i++ ) put( low>>24 ), low<<=8;

      //for( i=0; i<n; i++ ) ShiftLow(); 
    }
  }
};