// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, for and expression statements

// Written by Robert Swierczek
// + x86 JIT compiler by Dmytro Sirenko
// + win32 port by Joe Bogner
// + port to paq Kaido Orav

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <io.h>
#ifndef unix
#ifndef WINDOWS
#define WINDOWS // assume windows 
#endif
#endif

//#define VMJIT  // Comment to compile without x86 JIT
#define VMMSG  // prints error messages and x86 asm to console

#ifdef WINDOWS
#define PROT_NONE       0
#define PROT_READ       1
#define PROT_WRITE      2
#define PROT_EXEC       4

#define MAP_FILE        0
#define MAP_SHARED      1
#define MAP_PRIVATE     2
#define MAP_TYPE        0xf
#define MAP_FIXED       0x10
#define MAP_ANONYMOUS   0x20
#define MAP_ANON        MAP_ANONYMOUS
#define MAP_FAILED      ((void *)-1)

void*   mmap(void *addr, size_t len, int prot, int flags, int fildes, off_t off);
#else
#include <sys/mman.h>
#endif
/*
 int absolute(int value) {
  if (value < 0) {
    return -value;
  }
  else {
    return value;  
  }
}*/
// tokens and classes (operators last and in precedence order)
enum {  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Short, Return, For, Sizeof, While,
  Comma, Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};
// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI ,LS ,LC  ,SI ,SS ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,VTHIS,
        PRTF,SMP,SMN,APM,VMS,VMI,VMX,MXP,MXC,MXA,MXS,GCR,BUF,BUFR,MALC,MSET,MCMP,MCPY,STRE,SQUA,ILOG,H2,H3,H4,H5,READ,WRTE,EXIT};
// types (unsigned)
enum { rCHAR, sSHORT, iINT, PTR };
// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
enum { VMCOMPRESS=0,VMDETECT,VMENCODE,VMDECODE};
#ifdef VMMSG
#define kprintf printf // prints error messages
//#define dprintf printf // prints x86 asm to console
#define dprintf(...)  // prints x86 asm to console
#else
#define kprintf(...)    
#define dprintf(...)    
#endif

class VM {
private:
 
char *p, *lp, // current position in source code
     *data,*data0,   // data/bss pointer
     *jitmem, // executable memory for JIT-compiled native code
     *je;     // current position in emitted native code
int *e, *le, *text,*codestart,  // current position in emitted code
    *id,      // currently parsed indentifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line;     // current line number
    int fd, bt,   poolsz, *idmain,*idp,*idupdate;
    int *iddetect, *iddecode, *idencode;//functions detect,decode and encode
    //int *iddetect,*iddecode,*idencode; //
    int *pc, *sp,*sp0, *bp, cycle; // vm registers
    int i, *t,*pc0,tmp; // temps
     int a;
	 
    int  initvm( ) ; 
char *mod;
public:
    int fix;
    int debug;    // print executed instructions
    BlockData& x;
    int smc, apm1,  apm2,  rcm,  scm,  mcm,  cm,mc,  mx;
    StateMapContext **smC;
    APM1 **apm1C;
    APM2 **apm2C;
    RunContextMap **rcmC;
    SmallStationaryContextMap **scmC;
    //ContextMap2 **mcmC;
    ContextMap **cmC;
    //MatchContext **mcC;
    Mixer **mxC; 
    int totalc;  //total number of components
    int currentc; //current component, used in vmi
    int *mcomp;  //component list set in vmi
    int initdone; //set to 1 after main exits
    Array<char*> mem; //array of allocated memory
    int mindex;        // count fo memory allocations
    Array<int> memSize; // size in bytes of memory in mem[mindex]    
    File *inFile, *outFile; // files for decoding and encoding
    int vmMode;
VM(char* m,BlockData& bd,int mode);
~VM() ;
void next();
void  expr(int lev);
void  stmt();
int dovm(int *ttt);
#ifdef VMJIT
int  dojit();
 #endif
 int detect(int c4,int pos);
 int  decode(int info,int len);
  int  encode(int info,int len);
int block(int info1,int info2);
int doupdate(int y, int c0, int bpos,U32 c4,int pos);
void  killvm( );
void emit(int op,int val);
void decompile();
};
// alloc function in interpreted code
// keep track of pointers and sizes in bytes
// no bounds test
char* vmmalloc(VM* v,size_t i,int w){
  programChecker.alloc(U64(i*w));
  char*ptr= (char*)calloc(i*w,1);
  if (ptr==0) perror("mem error "),printf("%d ",i),quit("VM mem alloc fail");
  v->mem.resize(v->mem.size()+1);
  v->mindex =v->mem.size();
  v->mem[v->mindex-1]=ptr;
  v->memSize.resize(v->mindex);
  v->memSize[v->mindex-1]=i*w;
  return ptr;
}
void VM::killvm( ){
    if ( smc>0 )  {
       for (int i=0;i<smc;i++) delete smC[i];
       delete[]  smC; 
    }
    if ( apm1>0 ) {
        for (int i=0;i<apm1;i++) {
            delete apm1C[i];
        }
        delete[]  apm1C;
    }
    if (  apm2>0 ){
       for (int i=0;i<apm2;i++) delete apm2C[i];
       delete[]  apm2C;
    }
    if ( rcm>0 ) {
        for (int i=0;i<rcm;i++) delete rcmC[i];
        delete[]  rcmC;
    }
    if ( scm>0 ) {
        for (int i=0;i<scm;i++) delete scmC[i];
        delete[]  scmC;
    }
    // if ( mcm>0 ) delete[]  mcmC;
    if ( cm>0 ) {
        for (int i=0;i<cm;i++) delete cmC[i]; 
        delete[]  cmC;
    }
    if ( mx>0 ) {
        for (int i=0;i<mx;i++) delete mxC[i];
        delete[]  mxC;
    }
    /*if ( mc>0 ) {
        for (int i=0;i<mc;i++) delete mcC[i];
        delete[]  mcC;
    }*/
    if ( totalc>0 ) free(  mcomp);
    // free memory allocated by vmmalloc
    if (mindex){

        for (int i=0;i<mindex;i++){
            free(mem[i]);
            programChecker.free((U64)memSize[i]); // meaningless if MT enabled and thread count 1+ 
        }
    }
    smc=apm1=apm2=rcm=scm=mcm=cm=mx=mc=currentc=totalc=initdone=mindex=0;
    if (sym) free(sym),sym=0;
    if (text) free(text),text=0;
    if (data) free(data),data=0;
    if (sp) free(sp),sp=0;
  
}
//vms - set number of components
void components(VM* v,int a,int b,int c,int d,int e,int f,int g,int h,int i){
    if (v->initdone==1) printf("VM vms error: vms allowed only in main\n "),quit();
    if (v->totalc>0) printf("VM vms error: vms allready called\n "),quit();
    v->smc=a, v->apm1=b, v->apm2=c, v->rcm=d, v->scm=e, v->mcm=f, v->cm=g, v->mx=h,v->mc=i;
    v->totalc= a+d+e+f+g+i;
    //if (v->totalc>0 && h==0) quit("No mixer defined VM\n");
    if (v->totalc==0 && h>0) quit("No inputs for mixer defined VM\n");
    if (a>0 ) v->smC = new StateMapContext*[a]; //uses mixer if set
    if (b>0 ) v->apm1C = new APM1*[b];
    if (c>0 ) v->apm2C = new APM2*[c];
    
    if (v->totalc>0 ) v->mcomp=(int*)calloc(v->totalc*sizeof(int),1); //alloc memory for component list array
    //uses mixer
    if (d>0 ) v->rcmC = new RunContextMap*[d];
    if (e>0 ) v->scmC = new SmallStationaryContextMap*[e];
    if (f>0 ) quit("MCM not present.\n");//v->mcmC = new ContextMap2*[f];
    if (g>0 ) v->cmC = new ContextMap*[g];
    if (h>0 ) v->mxC = new Mixer*[h];
    if (i>0 ) quit("MC not present.\n");//v->mcC = new MatchContext*[i];
}
//vmi - init components
enum {vmSMC=1,vmAPM1,vmAPM2,vmRCM,vmSCM,vmMCM,vmCM,vmMX,vmMC};
void initcomponent(VM* v,int c,int i, int f,int d, int e){
    assert(f!=0); //mem
    assert(i>=0); //component index
    assert(d>=0); //component context
    assert(e>=0 || e==-1); //component mixer
    if (v->initdone==1) printf("VM vmi error: vmi allowed only in main\n "),quit();
    if (v->currentc>  v->totalc) printf("VM vmi error: component %d not set %d - %d\n ",c,v->currentc, v->totalc),quit();
    //  v->smc=a, v->apm1=b, v->apm2=c, v->rcm=d, v->scm=e, v->mcm=f, v->cm=g, v->mx=h;
    if (c==vmAPM2) printf("VM vmi error: APM2 not usable.\n "),quit();
    const int ii=i+1;
    switch (c) {
    case vmSMC:  if (ii>v->smc ) printf("VM vmi error: smc(%d) defined %d, max %d\n",c,ii, v->smc),quit(); 
        break; 
    case vmAPM1: if (ii>v->apm1) printf("VM vmi error: apm1(%d) defined %d, max %d\n",c,ii, v->apm1),quit();
        break; 
    case vmAPM2: if (ii>v->apm2) printf("VM vmi error: apm2(%d) defined %d, max %d\n",c,ii, v->apm2),quit();
        break; 
    case vmRCM:  if (ii>v->rcm ) printf("VM vmi error: rcm(%d) defined %d, max %d\n",c,ii, v->rcm),quit();
        break; 
    case vmSCM:  if (ii>v->scm ) printf("VM vmi error: scm(%d) defined %d, max %d\n",c,ii, v->scm),quit();
        break; 
    case vmMCM:  quit("MCM not present.\n");//if (ii>v->mcm ) printf("VM vmi error: mcm(%d) defined %d, max %d\n",c,ii, v->mcm),quit();
        break; 
    case vmCM:   if (ii>v->cm  ) printf("VM vmi error: cm(%d) defined %d, max %d\n",c,ii, v->cm),quit();
        break; 
    case vmMX:   if (ii>v->mx  ) printf("VM vmi error: mx(%d) defined %d, max %d\n",c,ii, v->mx),quit();
        break; 
    case vmMC:   quit("MC not present.\n");//if (ii>v->mc  ) printf("VM vmi error: mc(%d) defined %d, max %d\n",c,ii, v->mc),quit();
        break; 
    default: quit("VM vmi error\n");
    }
    // if e is -1 then no mixer is needed
    if ((c>  vmAPM2 && c< vmMX) || (c==vmSMC  && e!=-1)|| (c==vmMC))   v->mcomp[v->currentc++] =e+(i<<16)+(c<<8); // 0x00iiccmm index,component, mixer
    switch (c) {
    case vmSMC: v->smC[i] = new StateMapContext(f,  v->x);
        break;  
    case vmAPM1: v->apm1C[i] = new APM1(f,  v->x);
        break;
    case vmAPM2: v->apm2C[i] = new APM2(f,  v->x);
        break;
    case vmRCM: v->rcmC[i] = new RunContextMap(CMlimit(f<0?MEM()/(!f+1):MEM()*f),  v->x);
        break;
    case vmSCM: v->scmC[i] = new SmallStationaryContextMap(f );
        break;
    case vmMCM: quit("MCM not present.\n");//v->mcmC[i] = new ContextMap2(CMlimit(f<0?MEM()/(!f+1):MEM()*f), d); //??
        break;
    case vmCM: v->cmC[i] = new ContextMap(CMlimit(f<0?MEM()/(!f+1):MEM()*(U64)f),d);
        break;
    case vmMX: v->mxC[i] = new Mixer(f,d,  v->x,e);
        break;
    case vmMC: quit("MC not present.\n");//v->mcC[i] = new MatchContext(v->x,CMlimit(f<0?MEM()/(!f+1):MEM()*f),d);
        break;
    default:
        quit("VM vmi error\n");
        break;
    }
}
//set context to component
void setcomponent(VM* v,int c,int i, U32 f){
    //  if (e>  vmAPM2 && e< vmMX)   v->mcomp[v->currentc++] =e+(i<<16)+(c<<8); // component, mixer
    switch (c) {
    case vmRCM: v->rcmC[i]->set(f) ; 
        break;
    case vmSCM: v->scmC[i]->set(f);
        break;
    case vmMCM: quit("MCM not present.\n");//v->mcmC[i]->set(f);
        break;
    case vmCM: v->cmC[i]->set(f,-1);
        break;
     case vmMC:  
        break;
    default:
        quit("VM vmx error\n");
        break;
    }
}
int smn(VM* v,int state){ // get next state
    assert(state>=0 && state<256) ;
    return nex(state,v->x.y);
}
int smp(VM* v,int a,int cx,int limit){ // StateMap predict
    assert(limit>0 && limit<1024);
    return v->smC[a]->p(cx,limit);
}
int ap(VM* v,int a,int pr,int cx,int limit){ //APM1 predict
    assert(pr>=0 && pr<4096  && limit>0 && limit<32);
    return v->apm1C[a]->p(pr,cx,limit);
}
int bf(VM* v,int bufa){ // get buf
    //assert(bufa>=0) ;
    if (bufa<0){
       return v->x.buf(0);
    }
    return v->x.buf(bufa);
}
int bfr(VM* v,int bufra){ // get bufr
    //assert(bufra>=0) ;
    if (bufra<=0) bufra=0;
    return v->x.buf[bufra];
}
void mxa(VM* v,int i,int a){ // mixer add
    assert(i>=0 && i < v->mx);
    assert(a<=2047) ;
    assert(a>=-2047) ;
    v->mxC[i]->add(a);
}
void mxs(VM* v,int i,int a,int b){ // mixer set
   assert(i>=0 && i < v->mx);
   v->mxC[i]->set(a,b);
}
int readfile(VM* v,U8 *i,int size){ // 
   assert(size>0);
    assert(v->inFile!=NULL);
   return (int)v->inFile->blockread(i,(U64)size);
}
int writefile(VM* v,U8 *i,int size){ //
   assert(size>0);
   assert(v->outFile!=NULL);
   return (int)v->outFile->blockwrite(i,(U64)size);
}
//mix all mixer components  
int mxc(VM* v,int a){  
    assert(a>=0 && a < v->mx);
    for (int i=0;i< v->totalc;i++){
        int mi=v->mcomp[i] &0xff ;    // mixer  
        if (a==mi) { //if user called mixer found
            int j=v->mcomp[i]>>16;    // component index
            assert (j>=0 && j<v->totalc);
            //printf("%d %d ",(v->mcomp[i]>>8)&0xff,v->mcomp[i]);
            switch ((v->mcomp[i]>>8)&0xff) { // select component and mix
            case vmSMC: v->smC[j]->mix(*v->mxC[mi]);
                break;
            case vmRCM: v->rcmC[j]->mix(*v->mxC[mi]);
                break;
            case vmSCM: v->scmC[j]->mix(*v->mxC[mi]);
                break;
            case vmMCM:quit("MCM not present.\n");// v->mcmC[j]->mix(*v->mxC[mi]);
                break;
            case vmCM: v->cmC[j]->mix(*v->mxC[mi]);
                break;
            case vmMC: quit("MC not present.\n");// v->mcC[j]->mix(*v->mxC[mi]);
                break;
            default:
                quit("VM mxp error\n");
                break;
            }
        }
    }
    return 0;
}
//predict from mixer
int mxp(VM* v,int a){  
    assert(a>=0 && a < v->mx);
    assert(v->fix==0);
    return v->mxC[a]->p();
}
//get component result value
int gcr(VM* v,int a,int b,int c){  //this,mixer,index,component type
    assert(a>=0 && a < v->mx);
    for (int i=0;i< v->totalc;i++){
        int mi=v->mcomp[i] &0xff ;    // mixer  
        if (a==mi) {
            int j=v->mcomp[i]>>16;    // component index
            //printf("%d %d ",(v->mcomp[i]>>8)&0xff,v->mcomp[i]);
            int k= ((v->mcomp[i]>>8)&0xff);   // select component and mix
            if (j==b && k==c) {
            switch (k){
            case vmMCM:quit("MCM not present.\n");//return v->mcmC[j]->get();
                break;
            case vmCM: return v->cmC[j]->get();
                break;
            case vmMC: quit("MC not present.\n");//return v->mcC[j]->get();
                break;
            default:
                quit("VM gcr error\n");
                break;
            }
        }
        }
    }
    return 0;
}
VM::VM(char* m,BlockData& bd,int mode):x(bd),vmMode(mode),mem(0),memSize(0) {
    mod=m;
    smc=apm1=apm2=rcm=scm=mcm=cm=mx=mc=currentc=totalc=initdone=mindex=0;
    debug=0;
    fix=0;
    if (initvm()==-1) 
    exit(1);  //load cfg file, if error then exit
    initdone=1;
    totalc=currentc; //update total count to current count 
}

VM::~VM() {killvm();
}

void VM::next(){
  char *pp;
  int n;
  while (tk = *p) {
    ++p;
    if (tk == '\n') {
      ++line;
    }
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk == '0' && *(p)== 'x') { //Hexadecimal numbers
        p++;
        for (ival = 0; '\0' != (n = *p); p++) {
                if ( n >= 'a' && n <= 'f') {
                        n = n - 'a' + 10;
                } else if (n >= 'A' && n <= 'F') {
                        n = n - 'A' + 10;
                } else if (n >= '0' && n <= '9') {
                        n = n - '0';
                } else {
                        tk = Num;
                        return;
                }
                ival = ival<<4;
                ival  =ival + n;
        }
    }
    else if (tk >= '0' && tk <= '9') { //numbers
      ival = tk - '0';
      while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0';
      tk = Num;
      return;
    }
    else if (tk == '/') { //comment
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == ',') { tk = Comma; return;}
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ':') return;
  }
}
void VM::emit(int op,int val=0){
    *++e =op;
/*dprintf("%d: ",e);*/
switch (op){
case LEA: { /*dprintf(" LEA: %d\n",val);*/*++e=val; break;}
case IMM: { /*dprintf(" IMM: %d\n",val);*/*++e=val; break;}
case JMP: { /*dprintf(" JMP: %d\n",val);*/*++e=val; break;}
case JSR: { /*dprintf(" JSR: %d\n",val);*/*++e=val; break;}
case BZ : { /*dprintf(" BZ   %d\n",val);*/*++e=val; break;}
case BNZ: { /*dprintf(" BNZ: %d\n",val);*/*++e=val; break;}
case ENT: { /*dprintf(" ENT: \n");*/ break;} 
case ADJ: { /*dprintf(" ADJ: %d\n",val);*/*++e=val; break;} 
/*case LEV: { dprintf(" LEV: \n"); break;} 
case LI: { dprintf(" LI: \n"); break;} 
case LS: { dprintf(" LS: \n"); break;} 
case LC : dprintf(" LC \n"); { break;}
case SI: { dprintf(" SI: \n"); break;} 
case SS: { dprintf(" SS: \n"); break;} 
case SC : dprintf(" SC \n"); { break;}
case PSH: { dprintf(" PSH: \n"); break;} 
case OR : dprintf(" OR \n"); { break;}
case XOR: { dprintf(" XOR: \n"); break;} 
case AND: { dprintf(" AND: \n"); break;} 
case EQ : dprintf(" EQ \n"); { break;}
case NE : dprintf(" NE \n"); { break;}
case LT : dprintf(" LT \n"); { break;}
case GT : dprintf(" GT \n"); { break;}
case LE : dprintf(" LE \n"); { break;}
case GE : dprintf(" GE \n"); { break;}
case SHL: { dprintf(" SHL: \n"); break;} 
case SHR: { dprintf(" SHR: \n"); break;} 
case ADD: { dprintf(" ADD: \n"); break;} 
case SUB: { dprintf(" SUB: \n"); break;} 
case MUL: { dprintf(" MUL: \n"); break;} 
case DIV: { dprintf(" DIV: \n"); break;} 
case MOD: { dprintf(" MOD: \n"); break;} 
case VTHIS: { dprintf(" VTHIS: \n"); break;} 
case PRTF: { dprintf(" PRTF: \n"); break;} 
case SMP: { dprintf(" SMP: \n"); break;} 
case SMN: { dprintf(" SMN: \n"); break;} 
case APM: { dprintf(" APM: \n"); break;} 
case VMS: { dprintf(" VMS: \n"); break;} 
case VMI: { dprintf(" VMI: \n"); break;} 
case VMX: { dprintf(" VMX: \n"); break;} 
case MXP: { dprintf(" MXP: \n"); break;} 
case MXC: { dprintf(" MXC: \n"); break;} 
case MXA: { dprintf(" MXA: \n"); break;} 
case MXS: { dprintf(" MXS: \n"); break;} 
case GCR: { dprintf(" GCR: \n"); break;} 
case BUF: { dprintf(" BUF: \n"); break;} 
case BUFR: { dprintf(" BUFR: \n"); break;} 
case MALC: { dprintf(" MALC: \n"); break;} 
case MSET: { dprintf(" MSET: \n"); break;} 
case MCMP: { dprintf(" MCMP: \n"); break;} 
case MCPY: { dprintf(" MCPY: \n"); break;} 
case STRE: { dprintf(" STRE: \n"); break;} 
case SQUA: { dprintf(" SQUA: \n"); break;} 
case ILOG: { dprintf(" ILOG: \n"); break;} 
case H2: { dprintf(" H2: \n"); break;} 
case H3: { dprintf(" H3: \n"); break;} 
case H4: { dprintf(" H4: \n"); break;} 
case H5: { dprintf(" H5: \n"); break;} 
case EXIT: { dprintf(" EXIT: \n"); break;} */
default:
{break;}
}
 
}
void VM::expr(int lev){
  int t, *d,fc;

  if (!tk) { kprintf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { emit( IMM, ival);/**++e = IMM; *++e = ival;*/ next(); ty = iINT; }
  else if (tk == '"') {
    emit( IMM, ival); next(); 
    while (tk == '"') next();
    data = (char *)(((int)data + sizeof(int)) & -sizeof(int)); ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { kprintf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = iINT; if (tk == Int) next(); else if (tk == Char) { next(); ty = rCHAR; } else if (tk == Short) { next(); ty = sSHORT; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { kprintf("%d: close paren expected in sizeof\n", line); exit(-1); }
    emit(IMM,(ty == rCHAR) ? sizeof(char) :(ty == sSHORT) ? sizeof(short) : sizeof(int));//*++e = IMM; *++e = (ty == rCHAR) ? sizeof(char) :(ty == sSHORT) ? sizeof(short) : sizeof(int);
    ty = iINT;
  }
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {      
      if (d[Val]>=SMP &&  d[Val]<MALC || d[Val]==MALC|| d[Val]==READ|| d[Val]==WRTE){//for special functions in vm
            emit(VTHIS);//*++e = VTHIS;
            next();
            t = 1; //adjust stack
      }
      else{
          next();
          t=0;
      }
      fc=0;
    if ( d[Val]==MALC) {fc=3 ;}//args count
    else if (d[Val] == SMP  ) {fc=4 ;}
    else if (d[Val] == SMN  ) {fc=2 ;}
    else if (d[Val] == APM  ) {fc=5 ;}
    else if (d[Val] == VMS ) {fc=10 ;}
    else if (d[Val] ==VMI  ) {fc=6 ;}
    else if (d[Val] == VMX  ) {fc=4 ;}
    else if (d[Val] == MXP ) {fc=2 ;}
    else if (d[Val] == MXC  ) {fc=2 ;}
    else if (d[Val] == MXA  ) {fc=3 ;}
    else if (d[Val] == MXS  ) {fc=4 ;}
    else if (d[Val] == GCR  ) {fc=4 ;}
    else if (d[Val] == BUF  ) {fc=2 ;}
    else if (d[Val] == BUFR  ) {fc=2 ;}
    else if (d[Val] == STRE  ) {fc=1 ;}
    else if (d[Val] == SQUA  ) {fc=1 ;}
    else if (d[Val] == ILOG  ) {fc=1 ;}
    else if (d[Val] == H2  ) {fc=2 ;}
    else if (d[Val] == H3  ) {fc=3 ;}
    else if (d[Val] == H4  ) {fc=4 ;}
    else if (d[Val] == H5  ) {fc=5 ;}
    else if (d[Val] == READ || d[Val] == WRTE  ) {
         fc=3;
         if (!(vmMode==VMDECODE || vmMode==VMENCODE)) {
             printf("read/write allowed only in decode or encode stage.");
             exit(-1);
         }
    }
    //else if (d[Val] == WRTE  ) {fc=3 ;}
      while (tk != ')') { expr(Assign); emit(PSH);/**++e = PSH;*/ ++t; if (tk == Comma) next(); }
      next();
      if (d[Class] == Sys) {*++e = d[Val];
      
    if (t!=fc && fc!=0){ kprintf("%d: wrong number of arguments, (%d) expected %d\n", line,t,fc); exit(-1);}
      }
      else if (d[Class] == Fun) { emit(JSR,d[Val]);/**++e = JSR; *++e = d[Val];*/ }
      else { kprintf("%d: bad function call\n", line); exit(-1); }
      if (t) { emit(ADJ,t);/**++e = ADJ; *++e = t;*/ }
      ty = d[Type]; 
      
    }
    else if (d[Class] == Num) { emit(IMM,d[Val]);/**++e = IMM; *++e = d[Val];*/ ty = iINT; }
    else {
      if (d[Class] == Loc) { emit(LEA,loc - d[Val]);/**++e = LEA; *++e = loc - d[Val];*/ }
      else if (d[Class] == Glo) { emit(IMM,d[Val]);/**++e = IMM; *++e = d[Val];*/ }
      else { kprintf("%d: undefined variable\n", line); exit(-1); }
      emit(((ty = d[Type]) == rCHAR) ? LC : ((ty = d[Type]) == sSHORT) ? LS : LI);//*++e = ((ty = d[Type]) == rCHAR) ? LC : ((ty = d[Type]) == sSHORT) ? LS : LI;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char || tk == Short) {
      t = (tk == Int) ? iINT : (tk == Short) ? sSHORT : rCHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { kprintf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { kprintf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > iINT) ty = ty - PTR; else { kprintf("%d: bad dereference\n", line); exit(-1); }
    emit((ty == rCHAR) ? LC : (ty == sSHORT) ? LS : LI);// *++e = (ty == rCHAR) ? LC : (ty == sSHORT) ? LS : LI;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI || *e == LS) --e; else { kprintf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); emit(PSH);emit(IMM,0);emit(EQ);/* *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ;*/ ty = iINT; }
  else if (tk == '~') { next(); expr(Inc); emit(PSH);emit(IMM,-1);emit(XOR);/* *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR;*/ ty = iINT; }
  else if (tk == Add) { next(); expr(Inc); ty = iINT; }
  else if (tk == Sub) {
    next(); //*++e = IMM;
    if (tk == Num) { emit(IMM,-ival);/* *++e = -ival;*/ next(); } else { emit(IMM,-1);emit(PSH);emit(MUL);/* *++e = -1; *++e = PSH; expr(Inc); *++e = MUL;*/ }
    ty = iINT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; emit(LC);/**++e = LC;*/ }
    else if (*e == LI) { *e = PSH; emit(LI);/**++e = LI; */}
    else if (*e == LS) { *e = PSH; emit(LS);/**++e = LS;*/ }
    else { kprintf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    emit(PSH);/**++e = PSH;*/
    emit(IMM,(ty > PTR) ? sizeof(int) : (ty > iINT) ?  sizeof(short) : sizeof(char));/**++e = IMM; *++e = (ty > PTR) ? sizeof(int) : (ty > iINT) ?  sizeof(short) : sizeof(char);*/
    emit((t == Inc) ? ADD : SUB);/**++e = (t == Inc) ? ADD : SUB;*/
    emit((ty == rCHAR) ? SC : (ty == sSHORT) ? SS : SI);/**++e = (ty == rCHAR) ? SC : (ty == sSHORT) ? SS : SI;*/
  }
 
  else { kprintf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Comma) { 
      next(); expr(Assign);
    }
    else if (tk == Assign) {
      next();
      if (*e == LC || *e == LI || *e == LS) *e = PSH; else { kprintf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == rCHAR) ? SC : ((ty = t) == sSHORT) ? SS : SI;
    }
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { kprintf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = iINT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = iINT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = iINT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = iINT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = iINT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = iINT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = iINT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = iINT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = iINT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = iINT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = iINT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = iINT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = iINT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = iINT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = iINT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = iINT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = iINT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else if (*e == LS) { *e = PSH; *++e = LS; }
      else { kprintf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ?  sizeof(int) : (ty > iINT) ?  sizeof(short) : sizeof(char);;
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == rCHAR) ? SC : (ty == sSHORT) ? SS : SI;;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ?  sizeof(int) : (ty > iINT) ?  sizeof(short) : sizeof(char);;
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { kprintf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = ((ty = t - PTR) == rCHAR) ? 1 : ((ty = t - PTR) == sSHORT) ? 2 : 4; *++e = MUL;  } //fixed to int !!!
      else if (t < PTR) { kprintf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      char aa=((ty = t - PTR) == rCHAR) ? LC : ((ty = t - PTR) == sSHORT) ? LS : LI;//9 i 10 s 11 c
      *++e = aa;
    }
    else { kprintf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

void VM::stmt() {
    int *a, *b, *c,*d;
    switch (tk) {
    case If:  
        next();
        if (tk == '(') next(); else { kprintf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { kprintf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        if (tk == Else) {
            *b = (int)(e + 3); *++e = JMP; b = ++e;
            next();
            stmt();
        }
        *b = (int)(e + 1);
        break;
    case While:
        next();
        a = e + 1;
        if (tk == '(') next(); else { kprintf("%d: open paren expected\n", line); exit(-1); }
        expr(Comma);
        if (tk == ')') next(); else { kprintf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        *++e = JMP; *++e = (int)a;
        *b = (int)(e + 1);
        break;
    case For:
        next();
        if (tk == '(') next(); else { kprintf("%d: open paren expected\n", line); exit(-1); }
        if (tk != ';') expr(Comma);  //1
        next();
        a = e + 1;
        if (tk != ';') expr(Comma); //2
        *++e = BZ; b = ++e;
        next();
        if (tk != ')'){
            *++e = JMP; c=e+1;*++e = (int)0; //j1
            d=e+1;
            expr(Comma); //3
            *++e = JMP;  ;*++e = (int)a;//j2
            *c = (int)(e + 1); //patch j1
            a=d; //replace jmp to //3
        }
        if (tk == ')') next(); else { kprintf("%d: close paren expected\n", line); exit(-1); }
        stmt();
        *++e = JMP; *++e = (int)a;
        *b = (int)(e + 1);
        break; 
    case Return:
        next();
        if (tk != ';') expr(Comma);
        *++e = LEV;
        if (tk == ';') next(); else { kprintf("%d: semicolon expected\n", line); exit(-1); }
        break; 
    case '{':
        next();
        while (tk != '}') stmt();
        next();
        break; 
    case ';': 
        next();
        break; 
        default :
        expr(Comma);
        if (tk == ';') next(); else { kprintf("%d: semicolon expected\n", line); exit(-1); }
    }  
}
 
int stre(int a){ return stretch(a);}
int squa(int a){ return squash(a);}
int il(int a){ return ilog(a);}
U32 h2(U32 a, U32 b){ return hash0(a,b);}
U32 h3(U32 a, U32 b, U32 c){ return hash0(a,b,c);}
U32 h4(U32 a, U32 b, U32 c, U32 d){ return hash0(a,b,c,d);}
U32 h5(U32 a, U32 b, U32 c, U32 d, U32 e){ return hash0(a,b,c,d,e);}

#ifndef VMJIT
int VM::dovm(int *ttt){
  if (!(pc = ttt)) { kprintf("main() not defined\n"); return -1; }
  pc0=pc;
 
  cycle = 0;
  a=0;
  while (1) {
    i = *pc++; ++cycle;
 /*if (debug) {
      kprintf("%d>%x  %.4s", cycle,pc-pc0,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LS  ,LC  ,SI  ,SS  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,THIS,"
         "PRTF,SMP ,SMN ,APM ,VMS ,VMI ,VMX ,MXP ,MXC ,MXA ,MXS ,GCR ,BUF ,BUFR,MALC,MSET,MCMP,MCPY,STRE,SQUA,ILOG,H2  ,H3  ,H4  ,H5  ,READ,WRTE,EXIT"[i * 5]);
    if (i < JMP) kprintf(" %d\n",*pc); //? +1
     else if (i <= ADJ) kprintf(" %x\n",(int *)*pc-pc0+1); else kprintf("\n");
    }*/
    if      (i == LEA) a = (unsigned int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = (unsigned int)*pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(unsigned int *)a;                                     // load int
    else if (i == LS)  a = *(unsigned short *)a;                                     // load short
    else if (i == LC)  a = *(unsigned char *)a;                                    // load char
    else if (i == SI) *(unsigned int *)*sp++ = (unsigned int)a;                                 // store int
    else if (i == SC)   a = *(unsigned char *)*sp++ = (unsigned char)a;                            // store char
    else if (i == SS)   a = *(unsigned short *)*sp++ =(unsigned short) a;                            // store short
    else if (i == PSH) *--sp = a;                                         // push

    else if (i == OR)  a = (unsigned int)*sp++ |  (unsigned int)a;
    else if (i == XOR)  a = (unsigned int)*sp++ ^  (unsigned int)a;
    else if (i == AND)   a = (unsigned int)*sp++ & (unsigned int)a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)   a = *sp++ !=a;
    else if (i == LT)   a = *sp++ <  a;
    else if (i == GT)   a = *sp++ >  a;
    else if (i == LE)   a = *sp++ <= a;
    else if (i == GE)   a = *sp++ >= a;
    else if (i == SHL) a = (unsigned int)*sp++ << (unsigned int)a;
    else if (i == SHR) a = (unsigned int)*sp++ >> (unsigned int)a;
    else if (i == ADD)  a =*sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ * a;
    else if (i == DIV) a = *sp++ /   a;
    else if (i == MOD) a = *sp++ % a;

    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)vmmalloc(this,sp[1],*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == MCPY) a = (int)memcpy((char *)sp[2], (char *)sp[1], *sp);
    else if (i == SMP) a = (int)smp(this, sp[2], sp[1],*sp );
    else if (i == SMN) a =  (int)smn(this, *sp );
    else if (i == APM) a = (int)ap(this, sp[3],sp[2], sp[1],*sp );
    else if (i == VMS) a=0, components(this,sp[8],sp[7],sp[6], sp[5], sp[4], sp[3],sp[2], sp[1],*sp);
    else if (i == VMI) a=0, initcomponent(this, sp[4], sp[3],sp[2], sp[1],*sp);
    else if (i == VMX) a=0, setcomponent(this, sp[2], sp[1],*sp);
    else if (i == MXP) a = (int)mxp(this, *sp);
    else if (i == MXC) a = (int)mxc(this, *sp);
    else if (i == GCR) a = (int)gcr(this,   sp[2], sp[1],*sp);
    else if (i == STRE) a = (int)stretch( *sp);
    else if (i == SQUA) a = (int)squash(  *sp);
    else if (i == BUF) { a = (int)bf(this,  *sp);}
    else if (i == BUFR) { a = (int)bfr(this,  *sp);}
    else if (i == ILOG) { a = (int)il(  *sp);  }
    else if (i == MXA) {a=0,  mxa(this, sp[1], *sp);}
    else if (i == MXS) {a=0,  mxs(this,sp[2], sp[1],*sp);}
    else if (i == H2)  a = h2((U32)sp[1], (U32)*sp);
    else if (i == H3)  a = h3((U32)sp[2], (U32)sp[1],(U32)*sp);
    else if (i == H4)  a = h4((U32)sp[3],(U32)sp[2], (U32)sp[1],(U32)*sp);
    else if (i == H5)  a = h5((U32)sp[4],(U32)sp[3],(U32)sp[2], (U32)sp[1],(U32)*sp);
    else if (i == VTHIS)  *--sp;  //ignore
    else if (i == EXIT) { /*printf("exit(%d) cycle = %d\n", *sp, cycle);*/ return *sp; }
    else if (i == READ) a = (int)readfile(this,(U8 *)sp[1], *sp); //pointer,lenght
    else if (i == WRTE) a = (int)writefile(this,(U8 *)sp[1], *sp); //pointer,lenght
    else { kprintf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
    // if (debug) printf("a=%d ",a);
  }

}
 #endif
#ifdef VMJIT

int VM::dojit(){
  int u;
  // setup jit memory
  jitmem = (char*)mmap(0, poolsz, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (!jitmem) { dprintf("could not mmap(%d) jit executable memory\n", poolsz); return -1; }

  // first pass: emit native code
  pc = text + 1; je = jitmem; line = 0;
  while (pc <= e) {
    i = *pc;
     //if (debug) {
    //dprintf("// %x: ",pc);
    dprintf("   %.4s",  
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LS  ,LC  ,SI  ,SS  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,THIS,"
         "PRTF,SMP ,SMN ,APM ,VMS ,VMI ,VMX ,MXP ,MXC ,MXA ,MXS ,GCR ,BUF ,BUFR,MALC,MSET,MCMP,MCPY,STRE,SQUA,ILOG,H2  ,H3  ,H4  ,H5  ,READ,WRTE,EXIT"[i * 5]);
    if (i < JMP) dprintf(" 0x%x\n",*(pc+1)); //? +1
     else if (i <= ADJ) dprintf(" 0x%x\n",(int *)*pc); else dprintf("\n");
    // }
    *pc++ = ((int)je << 8) | i; // for later relocation of JMP/JSR/BZ/BNZ
 
    if (i == LEA) {
      i = 4 * *pc++; if (i < -128 || i > 127) { kprintf("jit: LEA out of bounds\n"); return -1; }
      *(int*)je = 0x458d; je = je + 2; *je++ = i;
      dprintf("\tlea eax,[ebp%s%d]\n",i>=0?"+":"",i);
    }
    else if (i == ENT) {
      //*je++ = 0xcc; 
      i = 4 * *pc++; if (i < -128 || i > 127) { kprintf("jit: ENT out of bounds\n"); return -1; }
      *(int *)je = 0xe58955; je = je + 3;
      dprintf("\tpush ebp\n\tmov ebp, esp\n",i);
      if (i > 0) { *(int *)je = 0xec83; je = je + 2; *(int*)je++ = i; dprintf("\tsub esp,BYTE %x\n",i); *(int *)je++ = 0x56;dprintf("\tpush esi\n"); }
    }
    else if (i == IMM) { 
    if (*(pc+1)==LI){
        dprintf("// %x:    LI\n",pc);
        *je++ = 0xa1; *(int *)je = i=*pc++; je = je + 4; dprintf("\tmov eax,DWORD [0x%x]\n",i);
        i = *pc;*pc++ = ((int)je << 8) | i; 
    }
    else {
    *je++ = 0xb8; *(int *)je = i=*pc++; je = je + 4; dprintf("\tmov eax,DWORD 0x%x\n",i);}
    } 
    else if (i == ADJ) { i = 4 * *pc++; *(int *)je = 0xc483; je = je + 2; *(int *)je = i; je++; } // add esp,BYTE (n * 4)
	else if (i == PSH && *(pc)==IMM && *(pc+1)==4 && *(pc+2)==MUL && *(pc+3)==ADD){	//array index*int
        *(int*)je = 0x59    ;je = je + 1; //pop ecx 
        dprintf("   IMM 0x4\n");
        dprintf("   MUL\n");
        dprintf("   ADD\n");
        dprintf("\tpop ecx\n");
        dprintf("\tlea    eax,[ecx+eax*4]\n");
        i = *pc;//IMM
        *pc++ = ((int)je << 8) | i; *pc++;
        i = *pc;//mul
        *pc++ = ((int)je << 8) | i; 
        i = *pc;//add
        *pc++ = ((int)je << 8) | i; 
        *(int *)je = 0x81048d;     je = je + 3; // lea    eax,[ecx+eax*4]
	}
    else if (i == PSH) {  *(int *)je++ = 0x50;dprintf("\tpush eax\n"); }
    else if (i == LEV) { *(int *)je++ = 0x5e;dprintf("\tpop esi\n");  *(int *)je = 0xc35dec89; je = je + 4; dprintf("\tmov esp, ebp\n\tpop ebp\n\tret\n"); }
    else if ((i == LI ||i == LC ||i == LS )&& *(pc)==PSH && *(pc+1)==IMM && 
	    (*(pc+3)==SHR || *(pc+3)==SHL || *(pc+3)==SUB|| *(pc+3)==ADD|| 
		(*(pc+3)==MUL && *(pc+4)!=ADD) ||
		*(pc+3)==OR || *(pc+3)==XOR || *(pc+3)==AND	)){
        
		if (i == LI )*(int *)je = 0x008b,     je = je + 2; //LI
		else if (i == LC )*(int *)je = 0x00b60f,     je = je + 3; //LC
		else if (i == LS )*(int *)je = 0x00B70F,     je = je + 3; //LS
        i = *pc;//PSH
        *pc++ = ((int)je << 8) | i;    
        i = *pc;//IMM
        *pc++ = ((int)je << 8) | i; 
        *je++ = 0xb9; *(int *)je = i=*pc++; je = je + 4; 
        dprintf("    PSH\n");
        dprintf("    IMM 0x%x\n",i);
        dprintf("    XXX\n");
        dprintf("\tmov eax,DWORD PTR [eax]\n");
        dprintf("\tmov ecx,DWORD 0x%x\n",i);
        i = *pc;//XXX
        *pc++ = ((int)je << 8) | i; 
		
        *(int*)je = i==SHR?0xe8d3:i==SHL?0xe0d3:i==SUB?0xc829:i==ADD?0xc801:i==MUL?0xc1af0f:i==OR?0xc809:i==XOR?0xc831:0xc821; 
		je = je + 2;dprintf("\tXXX  ;optimized\n");  
		if (i==MUL) je=je+1;
    }
    else if (i == LI)  { *(int *)je = 0x008b;     je = je + 2; dprintf("\tmov eax,DWORD PTR [eax]\n");} 
    else if (i == LC)  { *(int *)je = 0x00b60f;   je = je + 3; dprintf("\tmovzx eax,BYTE PTR [eax]\n"); } 
    else if (i == LS)  { *(int *)je = 0x00B70F;   je = je + 3; dprintf("\tmovzx eax,WORD PTR [eax]\n"); } 
    else if (i == SI)  { *(int *)je = 0x018959;   je = je + 3; dprintf("\tpop ecx\n\tmov DWORD PTR [ecx],eax    \n");}    
    else if (i == SC)  { *(int *)je = 0x018859;   je = je + 3; dprintf("\tpop ecx\n\tmov BYTE PTR [ecx],al    \n"); }
    else if (i == SS)  { *(int *)je = 0x01896659; je = je + 4; dprintf("\tpop ecx\n\tmov WORD PTR [ecx],ax    \n"); }
    else if (i == OR)  { *(int *)je = 0xc80959;   je = je + 3; dprintf("\tpop ecx\n\tor eax, ecx\n"); }
    else if (i == XOR) { *(int *)je = 0xc83159;   je = je + 3; dprintf("\tpop ecx\n\txor eax, ecx\n"); }
    else if (i == AND) { *(int *)je = 0xc82159;   je = je + 3; dprintf("\tpop ecx\n\tand eax, ecx\n"); }
    else if (EQ <= i && i <= GE) {
        *(int*)je=0x0fc13959; je+=4; *(int*)je=0x9866c094;        dprintf("\tpop ecx\n\tcmp ecx, eax");
        if      (i == NE)  { *je = 0x95; dprintf("\n\tsetne al");} // setne al
        else if (i == LT)  { *je = 0x9c; dprintf("\n\tsetb al");} // setb al
        else if (i == GT)  { *je = 0x9f; dprintf("\n\tseta al");} // seta al
        else if (i == LE)  { *je = 0x9e; dprintf("\n\tsetbe al");} // setbe al
        else if (i == GE)  { *je = 0x9d; dprintf("\n\tsetae al");} // setae al
        else dprintf("\n\tsete al");
        dprintf("\n\tcwde\n");
        je+=4; *je++=0x98;
    }
    else if (i == SHL) { *(int*)je = 0xe0d39159; je = je + 4;dprintf("\tpop ecx\n\txchg ecx, eax\n\tshl eax, cl\n"); } // pop ecx; xchg ecx, eax; shl eax, cl
    else if (i == SHR) { *(int*)je = 0xe8d39159; je = je + 4;dprintf("\tpop ecx\n\txchg ecx, eax\n\tshr eax, cl\n");  } // pop ecx; xchg ecx, eax; shr eax, cl
    else if (i == ADD) { *(int*)je = 0xc80159;   je = je + 3;dprintf("\tpop ecx\n\tadd eax, ecx\n"); } // pop ecx; add eax, ecx
    else if (i == SUB) { *(int*)je = 0xc8299159; je = je + 4;dprintf("\tpop ecx\n\txchg ecx, eax\n\tsub eax, ecx\n");  } // pop ecx; xchg ecx,eax; sub eax,ecx
    else if (i == MUL) { *(int*)je = 0xc1af0f59; je = je + 4;dprintf("\tpop ecx\n\txchg ecx, eax\n\t imul eax, ecx\n");  } // pop ecx; imul eax,ecx
    else if (i == DIV) { *je++=0x59; *(int*)je = 0xF9F79991; je = je + 4;dprintf("\txor edx,edx\tpop ecx\n\txchg ecx, eax\n\tdiv eax, ecx\n"); } // pop ecx; xchg ecx,eax; idiv eax,ecx
    else if (i == MOD) { *(int*)je = 0x999159; je += 3; *(int *)je = 0x92f9f7; je += 3; dprintf("\txor edx,edx\n\tpop ecx\n\txchg ecx,eax\n\tdiv ecx\n\txchg   edx,eax" ); }
    else if (i == JMP) { ++pc; *je       = 0xe9;     je = je + 5; dprintf("\tjmp  %x\n", *(pc-1) ); } // jmp <off32>
    else if (i == JSR) { ++pc; *je       = 0xe8;     je = je + 5; dprintf("\tcall  %x\n", *(pc-1) ); } // call <off32>
    else if (i == BZ)  { ++pc; *(int*)je = 0x840fc085; je = je + 8;dprintf("\ttest eax, eax\n\tjz  %x\n", *(pc-1) ); } // test %eax, %eax; jz <off32>
    else if (i == BNZ) { ++pc; *(int*)je = 0x850fc085; je = je + 8;dprintf("\ttest eax, eax\n\tjnz  %x\n", *(pc-1)  );  } // test %eax, %eax; jnz <off32>
    else if (i == VTHIS) { 
    *je++ = 0xb8; 
    *(int*)je =i=(unsigned int)(size_t(this));je += 4; *(int *)je++ = 0x50;dprintf("\tmov eax,DWORD %x\n\tpush eax    ;this\n",i); } //mov ecx,this b9
    else if (i >= PRTF) {
        /*  if      (i == OPEN) { tmp = (int)open; dprintf("\topen\n"); }  else if (i == READ) { tmp = (int)read;dprintf("\tread\n"); }
          else if (i == CLOS) { tmp = (int)close;dprintf("\tclose\n"); }  else 
*/ 
        if (i == PRTF) { tmp = (int)printf;  }
        else if (i == MALC) { tmp = (int)vmmalloc;  } //else if (i == MSET) { tmp = (int)memset;  }
        else if (i == SMP) { tmp = (int)smp;  } else if (i == SMN) { tmp = (int)smn;  }
        else if (i == MCMP) { tmp = (int)memcmp;  } else if (i == MCPY) { tmp = (int)memcpy;  }
        else if (i == EXIT) { tmp = (int)exit;  }else if (i == APM) { tmp = (int)ap;  }
        else if (i == VMS) { tmp = (int)components;  }else if (i == VMI) { tmp = (int)initcomponent;  }
        else if (i == VMX) { tmp = (int)setcomponent;  }else if (i == MXP) { tmp = (int)mxp;  }
        else if (i == STRE) { tmp = (int)stre;  }else if (i == SQUA) { tmp = (int)squa;  }
        else if (i == BUF) { tmp = (int)bf;  }else if (i == BUFR) { tmp = (int)bfr;  }
        else if (i == ILOG) { tmp = (int)il;  }else if (i == MXC) { tmp = (int)mxc;  }
        else if (i == MXA) { tmp = (int)mxa;  }else if (i == MXS) { tmp = (int)mxs;  }
        else if (i == GCR) { tmp = (int)gcr;  }
        else if (i == H2) { tmp = (int)h2;  }else if (i == H3) { tmp = (int)h3;  }else if (i == H4) { tmp = (int)h4;  }
        else if (i == H5) { tmp = (int)h5;  } else if (i == READ) { tmp = (int)readfile;  }
        else if (i == WRTE) { tmp = (int)writefile;  }
        
        u=i;
        if (*pc++ == ADJ) { i = *pc++; } else { kprintf("no ADJ after native proc!\n"); exit(2); }
        *je++ = 0xb9; *(int*)je = i << 2; je += 4; dprintf("\tmov ecx, 0x%x\n", i << 2 );  // movl $(4 * n), %ecx;
        *(int*)je = 0xce29e689; je += 4; dprintf("\tmov esi,esp\n\tsub esi,ecx\n"); // mov %esp, %esi; sub %ecx, %esi;  -- %esi will adjust the stack
        *(int*)je = 0x8302e9c1; je += 4; dprintf("\tshr ecx,2\n");// shr $2, %ecx; and                -- alignment of %esp for OS X
        *(int*)je = 0x895af0e6; je += 4; // $0xfffffff0, %esi; pop %edx; mov..
        *(int*)je = 0xe2fc8e54; je += 4; // ..%edx, -4(%esi,%ecx,4); loop..  -- reversing args order

        *(int*)je = 0xe8f487f9; je += 4; // ..<'pop' offset>; xchg %esi, %esp; call    -- saving old stack in %esi
        dprintf("\tand esi,0xfffffff0\n"); 
        dprintf("\tpop edx\n\tmov DWORD PTR [esi+ecx*4-0x4],edx\n"); 
        dprintf("\tloop 0x00000006\n\tcall "); 
        if (u == PRTF) {  dprintf("printf"); }
        else if (u == MALC) {  dprintf("malloc"); }// else if (u == MSET) { dprintf("memset\n"); }
        else if (u == MCMP) {  dprintf("memcmp"); } else if (u == MCPY) {  dprintf("memcpy"); }
        else if (u == EXIT) {  dprintf("exit"); }else if (u == SMP) {  dprintf("smp"); }
        else if (u == SMN) {  dprintf("smn"); }else if (u == APM) {  dprintf("apm"); }
        else if (u == VMS) {  dprintf("vms"); }else if (u == VMI) {  dprintf("vmi"); }
        else if (u == VMX) {  dprintf("vmx"); }else if (u == MXP) {  dprintf("mxp"); }
        else if (u == STRE) { dprintf("stretch");   }else if (u == SQUA) {  dprintf("squash");  }
        else if (u == BUF) {  dprintf("buf");  } else if (u == BUFR) {  dprintf("bufr");  }
        else if (u == ILOG) {  dprintf("ilog");  }
        else if (u == MXA) {  dprintf("mxa");  } else if (u == MXS) {  dprintf("mxs");  }
        else if (u == MXC) {  dprintf("mxc");  } else if (u == GCR) {  dprintf("gcr");  }
        else if (u == H2) {  dprintf("h2");  } else if (u == H3) {  dprintf("h3");  }
        else if (u == H4) {  dprintf("h4");  }
        else if (u == H5) {  dprintf("h5");  }else if (u== READ)  dprintf("read");  
        else if (u == WRTE)  dprintf("write");

        *(int*)je = tmp - (int)(je + 4); je = je + 4; // <*tmp offset>;
        dprintf(" //  %x\n", *(int*)(je-4) );  //print running memory address
        dprintf("// %x: ",pc-2); dprintf("ADJ 0x%x\n",i<<2);
        *(int*)je = 0xf487; je += 2;         // xchg %esi, %esp  -- ADJ, back to old stack without arguments
        dprintf("\txchg esp,esi\n");
    }
    else { kprintf("code generation failed for %d!\n", i); return -1; }
  }
  dprintf(" Code size %d \n",je -jitmem);

  // second pass, relocation
  pc = text + 1;
  while (pc <= e) {
    i = *pc & 0xff;
    je = (char*)(((unsigned)*pc++ >> 8) | ((unsigned)jitmem & 0xff000000)); // MSB is restored from jitmem
    if (i == JSR || i == JMP || i == BZ || i == BNZ) {
        tmp = (*(unsigned*)(*pc++) >> 8) | ((unsigned)jitmem & 0xff000000); // extract address
        if      (i == JSR || i == JMP) { je += 1; *(int*)je = tmp - (int)(je + 4); }
        else if (i == BZ  || i == BNZ) { je += 4; *(int*)je = tmp - (int)(je + 4); }
    }
    else if (i < LEV) { ++pc; }
    else if (i>EXIT) {kprintf("code generation failed. relocation error\n", i); return -1; }
  }
 return 0;
}
#endif
int  VM::decode(int info,int len){
#ifdef VMJIT
  int (*jitmain)( int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)(  int,int) >(*(unsigned*)( iddecode[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return  jitmain(len,info);
#else
  // setup stack
  data =data0;
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = info;
  *--sp = len; 
  *--sp = (int)t;
  return   dovm((int *)iddecode[Val]);
#endif
}
int  VM::encode(int info,int len){
#ifdef VMJIT
  int (*jitmain)(int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)( int,int) >(*(unsigned*)( idencode[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return  jitmain(len,info);
#else
  // setup stack
  data =data0;
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = info;
   *--sp = len; 
  *--sp = (int)t;
  return   dovm((int *)idencode[Val]);
#endif
}

int  VM::detect(int c4,int pos){
#ifdef VMJIT
  int (*jitmain)(int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)( int,int) >(*(unsigned*)( iddetect[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return  jitmain(pos,c4);
#else
  // setup stack
  data =data0;
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = c4;
  *--sp = pos; 
  *--sp = (int)t;
  return   dovm((int *)iddetect[Val]);
#endif
}
int  VM::block(int info1,int info2){
#ifdef VMJIT
  int (*jitmain)(int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)( int,int) >(*(unsigned*)( idp[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return  jitmain(info2,info1);
#else
  // setup stack
  data =data0;
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = info1;
  *--sp = info2; 
  *--sp = (int)t;
  return   dovm((int *)idp[Val]);
#endif
}

int  VM::doupdate(int y,int c0, int bpos,U32 c4,int pos){
    
if (mx>0 && totalc>0)    for (int i=0;i< mx;i++) mxC[i]->update(); //update all mixers
#ifdef VMJIT
  int (*jitmain)(int,U32,int,int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)(int,U32,int,int,int) >(*(unsigned*)( idupdate[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return  jitmain(pos,c4,bpos,c0,y);
#else
  // setup stack
  data =data0;
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = y;
  *--sp = c0;
  *--sp = bpos;
  *--sp = c4;
  *--sp = pos; 
  *--sp = (int)t;
  return   dovm((int *)idupdate[Val]);
#endif
}

int VM::initvm() { 
  poolsz = 1024*1024; // arbitrary size

  if (!(sym = (int *)malloc(poolsz))) { kprintf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(text = le = e = (int *)malloc(poolsz))) { kprintf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data =data0= (char *)malloc(poolsz))) { kprintf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp =sp0= (int *)malloc(poolsz))) { kprintf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

   p = "char else enum if int short return for sizeof while printf smp smn apm vms vmi vmx mxp mxc mxa mxs gcr buf bufr malloc memset memcmp memcpy stretch squash ilog h2 h3 h4 h5 read write exit void block update main detect decode encode";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = PRTF; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = iINT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type  
  next(); idp = id;      // keep track of block
  next(); idupdate = id; // keep track of updater
  next(); idmain = id;   // keep track of main
  next(); iddetect = id;
  next(); iddecode = id;
  next(); idencode = id;
  p=mod; //contains model
  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = iINT; // basetype
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = rCHAR; }
    else if (tk == Short) { next(); bt = sSHORT; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { kprintf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { kprintf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = iINT; id[Val] = i++;
          if (tk == Comma) next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { kprintf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { kprintf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = iINT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = rCHAR; }
          else if (tk == Short) { next(); ty = sSHORT; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { kprintf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { kprintf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == Comma) next();
        }
        next();
        if (tk != '{') { kprintf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char || tk == Short) {
          bt = (tk == Int) ? iINT : (tk == Short) ? sSHORT : rCHAR;;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { kprintf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { kprintf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == Comma) next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else if (tk == Brak) { // global array
        id[Class] = Glo;
        id[Val] =(int)data;
        *((int *)data)=(int)(data+4); //store pointer, data starts after pointer
        data = data + 4;
        int idz=id[Type];        
        id[Type]=id[Type]+ PTR;
        if (idz>iINT) { kprintf("%d: bad global declaration only char, short, int \n", line); return -1; }
        next(); i = ival; //array size
        if (tk != Num )          { kprintf("%d: bad global declaration\n", line); return -1; }
        if (i==0 )               { kprintf("%d: array to small \n", line); return -1; }
        next();if (tk != ']')    { kprintf("%d: missing closing braket\n", line); return -1; }
        next();if (tk != Assign) { kprintf("%d: missing array assingn \n", line); return -1; }
        next();if (tk != '{')    { kprintf("%d: missing array { \n", line); return -1; }
        next();
        int count=0;
        //store data after pointer
        while (tk != '}') {
          if (tk != Num) { kprintf("%d: bad glabal array value\n", line); return -1; }
          if (idz==0) *((char *)data) =(char)ival,data = data + 1;
          if (idz==1) *((short *)data) =(short)ival,data = data + 2;
          if (idz==2) *((int *)data)=(int)ival,data = data + 4;
          count++;
          if (count > i) { kprintf("%d: array out of bounds \n", line); return -1; }
          next();
          if (tk == Comma) next(); //else { kprintf("%d: comma expected \n", line); return -1; }
        }
      }
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == Comma) next();
    }
    next();
  }
       if (vmMode==VMDETECT && (iddetect[Val]==0 ||  idmain[Val]==0) ) quit("detect or main not defined");
       if (vmMode==VMENCODE && (idencode[Val]==0 ||  idmain[Val]==0) ) quit("encode or main not defined");
       if (vmMode==VMDECODE && (iddecode[Val]==0 ||  idmain[Val]==0) ) quit("decode or main not defined");
       if (vmMode==VMCOMPRESS && (idp[Val]==0 || idupdate[Val]==0 || idmain[Val]==0) ) quit("block update or main not defined");

#ifdef VMJIT
if (dojit()!=0) return -1;
 // run jitted code
  int (*jitmain)(); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)() >(*(unsigned*)(idmain[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  return jitmain();

#else
 // setup stack
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; 
  t = sp;
  *--sp = (int)t;
return dovm((int *)idmain[Val]);
#endif
}
 
#ifdef WINDOWS
#include <windows.h>

void* mmap(void *addr, size_t len, int prot, int flags, int fildes, off_t off){
    HANDLE fm, h;
    void * map = MAP_FAILED;
    const off_t maxSize = off + (off_t)len;
    h = (HANDLE)_get_osfhandle(fildes);
    fm = CreateFileMapping(h, NULL, PAGE_EXECUTE_READWRITE, 0, maxSize, NULL);
    map = MapViewOfFile(fm, FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_EXECUTE, 0, off, len);
    CloseHandle(fm);
    return map;
}
#endif        
