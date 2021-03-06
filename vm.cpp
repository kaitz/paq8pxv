// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, for and expression statements

// Written by Robert Swierczek
// + x86 JIT compiler by Dmytro Sirenko
// + win32 port by Joe Bogner
// + port to paq Kaido Orav


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
void jitReadonly(void *p, size_t size){
#ifdef WINDOWS
  DWORD oldProtect;
    VirtualProtect(p, size, PAGE_EXECUTE_READ, &oldProtect);
#else
    mprotect(p, size, PROT_READ | PROT_EXEC);
#endif
}
// tokens and classes (operators last and in precedence order)
enum {  Num = 128, Fun, Sys, Glo, Loc, Id,  Load, Enter,
  Char, Else, Enum, If, Int, Short, Return, For, Sizeof, While,
  Comma, Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};
// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI ,LS ,LC  ,SI ,SS ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,BOUND,VTHIS,
        PRTF,VMS,VMI,VMX,H2,READ,WRTE,EXIT};

// types
enum { rCHAR, sSHORT, iINT, PTR };
// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal,IDLen,UBound, Idsz };
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
 Array<char> data1;
char *p, *lp, // current position in source code
     *data,*data0,   // data/bss pointer,
     *jitmem, // executable memory for JIT-compiled native code
     *je;     // current position in emitted native code
int *e, *le, *text,*codestart,  // current position in emitted code
    *id,      // currently parsed indentifier
    *n,       // current node in abstract syntax tree
    *sym,     // symbol table (simple list of identifiers)
    *ast,
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line;     // current line number
    int fd, bt,   poolsz, *idmain,*idp,*idupdate;
    int *iddetect, *iddecode, *idencode;//functions detect,decode and encode
    int *pc, *sp,*sp0, *bp, cycle; // vm registers
    int i, *t,*pc0,tmp; // temps
    int a;
	 
    int  initvm( ) ; 
char *mod;
public:
    int debug;    // print executed instructions
    BlockData& x;
    int vmMode;
    Array<char*> mem; //array of allocated memory
    Array<int> memSize; // size in bytes of memory in mem[mindex]    
    Array<int*> membound; //array of allocated memory
    Array<int> prSize; // size in bytes of memory in mem[mindex]   
    Array<int> mcomp;  //component list set in vmi
    Array<ContextMap *>cmC;
    int smc, apm1, rcm, scm, cm, mx,st,av,ds,mm,dhs;

    MixMap1 mmA[255];
    Mixer1 mxA[255];
    StaticMap stA[255];
    DynamicSMap dsA[255];
    DynamicHSMap dhsA[255];
    APM1 apm1A[255];
    StateMapContext smA[255];
    AvgMap  avA[255];   
    SmallStationaryContextMap scmA[255];    
    RunContextMap rcmA[255];  
    int totalc;  //total number of components
    int currentc; //current component, used in vmi
    int initdone; //set to 1 after main exits
    int mindex;        // count fo memory allocations
    FILE *inFile, *outFile; // files for decoding and encoding
    int inpos;

VM(char* m,BlockData& bd,int mode);
~VM() ;
void next();
void expr(int lev);
void stmt();
int dovm(int *ttt);
void gen(int *n);
#ifdef VMJIT
int  dojit();
 #endif
int detect(int c4,int pos);
int decode(int info,int len);
int encode(int info,int len);
int block(int info1,int info2);
int doupdate(int y, int c0, int bpos,U32 c4,int pos);
void killvm( );
void decompile();
int getPrediction(int cInputs);
void updateComponents();
};
// alloc function in interpreted code
// keep track of pointers and sizes in bytes
// no bounds test
char* vmmalloc(VM* v,size_t i,int w){
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
       for (int i=0;i<smc;i++)  smA[i].Free(); 
    }
    if ( apm1>0 ) {
        for (int i=0;i<apm1;i++) apm1A[i].Free();
    }
    if ( scm>0 ) {
        for (int i=0;i<scm;i++)  scmA[i].Free();
    }
    if ( cm>0 ) {
        for (int i=0;i<cm;i++) delete cmC[i]; 
    }
   if ( mx>0 ) {
        for (int i=0;i<mx;i++) mxA[i].Free();
    }
    if ( ds>0 ) {
        for (int i=0;i<ds;i++)  dsA[i].Free();
    }
    if ( dhs>0 ) {
        for (int i=0;i<dhs;i++)  dhsA[i].Free();
    }
    // free memory allocated by vmmalloc
    if (mindex){

        for (int i=0;i<mindex;i++){
            free(mem[i]);
            //programChecker.free((U64)memSize[i]); // meaningless if MT enabled and thread count 1+ 
        }
    }
    smc=apm1=rcm=scm=cm=mx=mm=st=av=ds=dhs=currentc=totalc=initdone=mindex=0;
    if (sym) free(sym),sym=0;
    if (ast) free(ast),ast=0;
    if (text) free(text),text=0;
    //if (data) free(data),data=0;
    if (sp0) free(sp0),sp0=sp=0;
  
}
//vms - set number of components
void components(VM* v,int a,int b,int c,int d,int e,int f,int g,int h,int i,int j,int k){
    if (v->initdone==1) {kprintf("VM vms error: vms allowed only in main\n ");quit();}
    if (v->totalc>0) {kprintf("VM vms error: vms allready called\n ");quit();}
    v->smc=a, v->apm1=b,v->ds=c,v->av=d,v->scm=e, v->rcm=f,   v->cm=g, v->mx=h,v->st=i,v->mm=j,v->dhs=k;
    v->totalc= a+b+c+d+e+f+g+h+i+j+h;
    v->mcomp.resize(v->totalc); 
    if (v->totalc==0 && h>0) quit("No inputs for mixer defined VM\n");
}
//vmi - init components
enum {vmSMC=1,vmAPM1,vmDS,vmAVG,vmSCM,vmRCM,vmCM,vmMX,vmST,vmMM,vmDHS};
#ifndef NDEBUG
void printcomponent(int component){

switch (component) {
    case vmSMC: printf("SMC");
        break;  
    case vmAPM1: printf("APM");
        break;
    case vmDS: printf("DS ");
        break;
    case vmRCM: printf("RCM");
        break;
    case vmSCM: printf("SCM");
        break;
    case vmAVG:printf("AVG");
        break;
    case vmCM: printf("CM ");
        break;
    case vmMX: printf("MX ");
        break;
    case vmST:  printf("ST ");
        break;
    case vmMM:  printf("MM ");
        break;
    case vmDHS:  printf("DHS ");
        break;
    default:
        quit("printcomponent\n");
        break;
    }
    }
#endif
void initcomponent(VM* v,int component,int componentIndex, int f,int d, int indexOfInputs){
    assert(componentIndex>=0); //component index
    assert(d>=0); //component context
    if (v->initdone==1) {kprintf("VM vmi error: vmi allowed only in main\n ");quit();}
    if (v->currentc>  v->totalc) {kprintf("VM vmi error: component %d not set %d - %d\n ",component,v->currentc, v->totalc);quit();}

    const int ii=componentIndex+1;
    bool isInputs= (component==vmAPM1 || component==vmDS|| component==vmDHS || component==vmAVG || (component==vmST && indexOfInputs==-1)||(component==vmSMC && indexOfInputs==-1));
    if (indexOfInputs>=0 &&  v->x.cInputs <indexOfInputs && isInputs==false){// input sets for mixers
        v->x.cInputs++;
        v->x.mxInputs.resize(v->x.mxInputs.size()+1);
        if(v->x.mxInputs.size()<=indexOfInputs){
        v->x.mxInputs.resize(indexOfInputs+1);
        v->x.cInputs=indexOfInputs;
        }
    }
   
    switch (component) {
    case vmSMC: {if ( indexOfInputs==-1)v->prSize.resize(v->prSize.size()+1); if (ii>v->smc ) {kprintf("VM vmi error: smc(%d) defined %d, max %d\n",component,ii, v->smc);quit(); } 
                if ( indexOfInputs>=0) v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+1;
        break; }
    case vmAPM1:{v->prSize.resize(v->prSize.size()+1); if (ii>v->apm1) {kprintf("VM vmi error: apm1(%d) defined %d, max %d\n",component,ii, v->apm1);quit();} 
        break; }
    case vmDS:{v->totalc=v->totalc+indexOfInputs-1;v->mcomp.resize(v->mcomp.size()+indexOfInputs); v->prSize.resize(v->prSize.size()+indexOfInputs); if (ii>v->ds) {kprintf("VM vmi error: ds(%d) defined %d, max %d\n",component,ii, v->ds);quit();}
          if (f<1) {kprintf("VM vmi error:ds(%d) memory bits must be larger then 0.",ii);quit();}
        break;      }
    case vmDHS:{v->totalc=v->totalc+indexOfInputs-1;v->mcomp.resize(v->mcomp.size()+indexOfInputs); v->prSize.resize(v->prSize.size()+indexOfInputs); if (ii>v->dhs) {kprintf("VM vmi error: dhs(%d) defined %d, max %d\n",component,ii, v->dhs);quit();}
          if (f<1) {kprintf("VM vmi error:dhs(%d) memory bits must be larger then 0.",ii);quit();}
        break;      }
    case vmRCM: { if (ii>v->rcm ) {kprintf("VM vmi error: rcm(%d) defined %d, max %d\n",component,ii, v->rcm);quit(); }
     if ( indexOfInputs>=0) v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+1;
        break;  }
    case vmSCM: { if (ii>v->scm ) {kprintf("VM vmi error: scm(%d) defined %d, max %d\n",component,ii, v->scm);quit(); }
     if ( indexOfInputs>=0) v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+2;
        break;  }
    case vmAVG:{ v->prSize.resize(v->prSize.size()+1); if (ii>v->av ) {kprintf("VM vmi error: AVG(%d) defined %d, max %d\n",component,ii, v->av);quit();}
        break;  }
    case vmCM: {
        v->cmC.resize(v->cmC.size()+1);
        if ( indexOfInputs>=0) v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+6*d;
        break;  }
    case vmMX: {v->prSize.resize(v->prSize.size()+1); 
        break;  }
    case vmST: {if ( indexOfInputs==-1)v->prSize.resize(v->prSize.size()+1); if (ii>v->st )  {kprintf("VM vmi error: st(%d) defined %d, max %d\n",component,ii, v->st);quit();}
        if ( indexOfInputs>=0) v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+1;
        break;  }
    case vmMM: { v->x.mxInputs[indexOfInputs].ncount=v->x.mxInputs[indexOfInputs].ncount+1;
        break;  }
    default: quit("VM vmi error\n");
    }

    int prindex=0;
    if (component==vmAPM1 || component==vmDS|| component==vmDHS || component==vmAVG || (component==vmST && indexOfInputs==-1)||(component==vmSMC && indexOfInputs==-1) || component==vmMX)prindex=(v->prSize.size());

    switch (component) {
    case vmSMC: v->smA[componentIndex].Init(f, d);
        break;  
    case vmAPM1: v->apm1A[componentIndex].Init(f,d,indexOfInputs);
        break;
    case vmDS: v->dsA[componentIndex].Init(f,d,indexOfInputs);
        break;
        case vmDHS: v->dhsA[componentIndex].Init(f,d,indexOfInputs);
        break;
    case vmRCM: v->rcmA[componentIndex].Init(f<=0?4096:f*4096);
        break;
    case vmSCM: v->scmA[componentIndex].Init(f); 
        break;
    case vmAVG:v->avA[componentIndex].Init(d,indexOfInputs);
        break;
    case vmCM: v->cmC[componentIndex] = (ContextMap*)new ContextMap(f<=0?4096:f*4096,d,v->x);
        break;
    case vmMX: v->mxA[componentIndex].Init(d,f); //context,shift
        break;
    case vmST: v->stA[componentIndex].Init(f);
        break;
    case vmMM: v->mmA[componentIndex].Init(d,f);
        break;
    default:
        quit("VM vmi error\n");
        break;
    }
    int m=indexOfInputs;
    if (component==vmAVG || component==vmAPM1) m=0;
    if (indexOfInputs==-1) m=0;
     if (component==vmDS || component==vmDHS  ) {m=0;
     for (int j=0;j< indexOfInputs;j++) v->mcomp[v->currentc++] =m+((prindex-indexOfInputs+j+1)<<24)+(componentIndex<<16)+(component<<8);
     }else{
     
    v->mcomp[v->currentc++] =m+(prindex<<24)+(componentIndex<<16)+(component<<8); // 0x00iiccmm index,component, inputs
    }
#ifndef NDEBUG
    int pri=v->mcomp[v->currentc-1]>>24&0xff;
    printf("0x%08x ", v->mcomp[v->currentc-1]);
    if (pri==0) printf("      (");else printf(" pr[%d](",pri-1);
    printcomponent(v->mcomp[v->currentc-1]>>8&0xff);
    printf("[%d]) input[%d] ",v->mcomp[v->currentc-1]>>16&0xff,v->mcomp[v->currentc-1]&0xff);
    //if (v->mcomp[v->currentc-1]&0xff=vmMM) printf("pr[%d] ",v->mcomp[v->currentc-1]>>16&0xff);
    printf("\n" );
#endif 
}
//set context to component
void setcomponent(VM* v,int c,int i, U32 f){
    switch (c) {
        case vmSMC: { 
             v->smA[i].set(f,v->x.y);
             break;}
        case vmAPM1:{ 
             v->apm1A[i].cxt=(f); 
             break;}
        case vmDS:{ 
             v->dsA[i].set(f,v->x.y); 
             break;}
        case vmDHS:{ 
             v->dhsA[i].set(f,v->x.y); 
             break;}
        case vmRCM:{ 
             v->rcmA[i].set(f,v->x.c4); 
             break;}
        case vmSCM:{ 
             v->scmA[i].set(f);
             break;}
        case vmCM:{ 
             v->cmC[i]->set(f); 
             break;}
        case vmMX:{
             v->mxA[i].cxt=f;
             break;}
        case vmST:{
            //  v->stC[i]->set(f);
             break;}
        case vmMM: { 
             break;}
        default:{
             quit("VM vmx error\n");
             break;}
    }
}
//  i    size
/// pos     -2  - Seek to pos     
//  0       -2  - Seek to end
//  0       -1  - Seek to start

int readfile(VM* v,U8 *i,int size){
    assert(size>-3); 
    assert(v->inFile!=NULL);
    if (size>0)return fread (i,1,size,v->inFile);
    if (size==-2)fseek (v->inFile , 0 , SEEK_END); 
    else if (size==-1) fseek(v->inFile, v->inpos, SEEK_SET); // set to block start pos not file start pos
    else       return -1;
}
int writefile(VM* v,U8 *i,int size){
    assert(size>-3);
    assert(v->outFile!=NULL);
    if (size==-2)fseek (v->outFile , 0 , SEEK_END);
    else if (size==-1) fseek(v->outFile, 0, SEEK_SET); 
    else     return fwrite (i , 1, size, v->outFile); 
    return -1;
}
//mix all input components  
int mxc(VM* v,int input){  
    for (int i=0;i< v->totalc;i++){
        int inputIndex=v->mcomp[i] &0xff ;    // index  
        int component=(v->mcomp[i]>>8)&0xff;
        if (input==inputIndex && (v->mcomp[i]>>24)==0 && (component==vmST|| component==vmSMC || component==vmRCM ||component==vmSCM || component==vmCM )) {
            int componentIndex=v->mcomp[i]>>16;    // component index
            switch (component) { // select component and mix
            case vmSMC: v->x.mxInputs[inputIndex].add(stretch(v->smA[componentIndex].pr)); //v->smC[componentIndex]->mix(inputIndex);
                break;
            case vmRCM: v->rcmA[componentIndex].mix(v->x,inputIndex);
                break;
            case vmSCM: v->scmA[componentIndex].mix(v->x,inputIndex);
                break;
            case  vmCM: v->cmC[componentIndex]->mix(inputIndex);
                break;
            case  vmST: v->x.mxInputs[inputIndex].add(v->stA[componentIndex].pr);
                break;
            default:
                quit("VM mxp error\n");
                break;
            }
        }
    }
    return 0;
}
 
VM::VM(char* m,BlockData& bd,int mode):data1(2024*1024),x(bd),vmMode(mode),mem(0),memSize(0),membound(0),prSize(0),mcomp(0),/*mmC(0),mxC(0),*/cmC(0) {
    data=&data1[0];
    mod=m;
    smc=apm1=rcm=scm=cm=mx=st=av=mm=ds=dhs=currentc=totalc=initdone=mindex=0;
    debug=0;
    if (initvm()==-1) 
    exit(1);  //load cfg file, if error then exit
    initdone=1;
    totalc=currentc; //update total count to current count 
    
    // if mixer is used parse all input arrays
    if(x.cInputs>=0 && x.cInputs<256&& idupdate[Val]){ 
        // init input arrays
        for (int j=0;j<x.cInputs+1;j++)  {
            x.mxInputs[j].ncount=(x.mxInputs[j].ncount+15)&-16 ;
            x.mxInputs[j].n.resize(x.mxInputs[j].ncount ); 
            // resize inputs[x] to correct size
        }     
        // provide inputs array info to mixers
        for (int i=0;i<totalc;i++){
            int prindex=mcomp[i]>>24;
            int compnr=(mcomp[i]>>8)&0xff;
            // individual components
            if (prindex>0 && compnr==vmMX){
                int index=(mcomp[i]>>16)&0xff;
                int input=mcomp[i]&0xff;
                // set input 
                mxA[index].setTxWx(x.mxInputs[input].n.size(),&x.mxInputs[input].n[0]);
            }
        }
    }
}

VM::~VM() {killvm();
}

void VM::next(){
  char *pp;
  int nu;
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
      id[IDLen] = p - pp;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk == '0' && *(p)== 'x') { //Hexadecimal numbers
        p++;
        for (ival = 0; '\0' != (nu = *p); p++) {
                if ( nu >= 'a' && nu <= 'f') {
                        nu = nu - 'a' + 10;
                } else if (nu >= 'A' && nu <= 'F') {
                        nu = nu - 'A' + 10;
                } else if (nu >= '0' && nu <= '9') {
                        nu = nu - '0';
                } else {
                        tk = Num;
                        return;
                }
                ival = ival<<4;
                ival  =ival + nu;
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

void VM::expr(int lev){
  int t, *d,fc;
//char *nam;
  if (!tk) { kprintf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = iINT; }
  else if (tk == '"') {
    *++e = IMM; *++e = ival;  next(); 
    while (tk == '"') next();
    data = (char *)(((int)data + sizeof(int)) & -sizeof(int)); ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { kprintf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = iINT; if (tk == Int) next(); else if (tk == Char) { next(); ty = rCHAR; } else if (tk == Short) { next(); ty = sSHORT; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { kprintf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == rCHAR) ? sizeof(char) :(ty == sSHORT) ? sizeof(short) : sizeof(int);
    ty = iINT;
  }
  else if (tk == Id) {
    d = id; 
  /* nam=(char*)id[Name];
    printf("\n//");
        for (int y=0;y<id[IDLen];y++)printf("%c",nam[y]);
        if (id[UBound]>0)
        printf("// id-val 0x%08x, max size %d",id[Val], id[UBound]);
        printf("\n");*/
    next();
    if (tk == '(') {      
      if ((d[Val]>=VMS &&  d[Val]<H2) || d[Val]==READ|| d[Val]==WRTE){//for special functions in vm
            *++e = VTHIS;
            next();
            t = 1; //adjust stack
      }
      else{
          next();
          t=0;
      }
      fc=0;
      if ( (d[Val] >= VMS &&  d[Val]<H2) && (vmMode==VMDECODE || vmMode==VMENCODE)) {
             printf("VMS, VMI, VMX or H2 allowed only in (de)comprassion stage.");
             exit(-1);
         }
    if (d[Val] == VMS ) {fc=10 +1+1;}
    else if (d[Val] ==VMI  ) {fc=6 ;}
    else if (d[Val] == VMX  ) {fc=4 ;}
    else if (d[Val] == H2  ) {fc=2 ;}
    else if (d[Val] == READ || d[Val] == WRTE  ) {
         fc=3;
         if (!(vmMode==VMDECODE || vmMode==VMENCODE)) {
             printf("read/write allowed only in decode or encode stage.");
             exit(-1);
         }
    }
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == Comma) next(); }
      next();
      if (d[Class] == Sys) {*++e =d[Val];
      
    if (t!=fc && fc!=0){ kprintf("%d: wrong number of arguments, (%d) expected %d\n", line,t,fc); exit(-1);}
      }
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { kprintf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type]; 
      
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = iINT; }
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      else { kprintf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == rCHAR) ? LC : ((ty = d[Type]) == sSHORT) ? LS : LI;
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
  /*else if (tk == Mul) { // *name
    next(); expr(Inc);
    if (ty > iINT) ty = ty - PTR; else { kprintf("%d: bad dereference\n", line); exit(-1); }
    emit((ty == rCHAR) ? LC : (ty == sSHORT) ? LS : LI);// *++e = (ty == rCHAR) ? LC : (ty == sSHORT) ? LS : LI;
  }*/
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI || *e == LS) --e; else { kprintf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { 
  next(); *++e = IMM;
    if (tk == Num) {*++e = !ival;next(); }else { *++e = 0; *++e = PSH; expr(Inc); *++e = EQ; }
    ty = iINT;
  }
  else if (tk == '~') {
  next(); *++e = IMM;
    if (tk == Num) {*++e = ~ival;next(); }else { *++e = -1; *++e = PSH; expr(Inc); *++e = XOR; }
    ty = iINT;
  }
  else if (tk == Add) { next(); expr(Inc); ty = iINT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) {  *++e = -ival; next(); } else {  *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = iINT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else if (*e == LS) { *e = PSH; *++e = LS; }
    else { kprintf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : (ty > iINT) ?  sizeof(short) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == rCHAR) ? SC : (ty == sSHORT) ? SS : SI;
  }
 
  else { kprintf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Comma) { 
      next(); expr(Assign);
    }
    else if (tk == Assign) {
      next();
     if (ty > PTR) {  printf("%d: cant assign to pointer\n", line); exit(-1); }
      if (*e == LC || *e == LI || *e == LS) *e = PSH; else { kprintf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == rCHAR) ? SC : ((ty = t) == sSHORT) ? SS : SI;
    }
    else if (tk == Cond) {
      next();
      *++e=BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { kprintf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e=(BNZ) ;  d = ++e; expr(Lan); *d = (int)(e + 1); ty = iINT; }
    else if (tk == Lan) { next(); *++e=(BZ) ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = iINT; }
    else if (tk == Or)  { next();*++e=(PSH); expr(Xor); *++e=( OR);  ty = iINT; }
    else if (tk == Xor) { next(); *++e=(PSH); expr(And);*++e=( XOR); ty = iINT; }
    else if (tk == And) { next(); *++e=(PSH); expr(Eq);  *++e=( AND); ty = iINT; }
    else if (tk == Eq)  { next(); *++e=(PSH); expr(Lt);  *++e=( EQ);  ty = iINT; }
    else if (tk == Ne)  { next(); *++e=(PSH); expr(Lt);  *++e=( NE);  ty = iINT; }
    else if (tk == Lt)  { next(); *++e=(PSH); expr(Shl); *++e=( LT);  ty = iINT; }
    else if (tk == Gt)  { next(); *++e=(PSH); expr(Shl); *++e=( GT);  ty = iINT; }
    else if (tk == Le)  { next(); *++e=(PSH); expr(Shl); *++e=( LE);  ty = iINT; }
    else if (tk == Ge)  { next(); *++e=(PSH); expr(Shl); *++e=( GE);  ty = iINT; }
    else if (tk == Shl) { next();*++e=(PSH); expr(Add); *++e=( SHL); ty = iINT; }
    else if (tk == Shr) { next(); *++e=(PSH); expr(Add); *++e=( SHR); ty = iINT; }
    else if (tk == Add) {
      next(); *++e=(PSH); expr(Mul);
      if ((ty = t) > PTR) {*++e=(PSH); *++e=(IMM);*++e=sizeof(int); *++e=(MUL);  }//pointer
      *++e=(ADD); //*++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e=(IMM);*++e=(sizeof(int));*++e=(DIV); ty = iINT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e=(MUL); *++e=(SUB);   }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e=(MUL);   ty = iINT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc);*++e=(DIV);   ty = iINT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e=(MOD);   ty = iINT; }
    else if (tk == Inc || tk == Dec) {
        if (ty > PTR) {  printf("%d: cant assign to pointer\n", line); exit(-1); }
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
#ifdef VMBOUNDS         
      int directarray=(int)e;
      unsigned int upperbound=id[UBound]-1;
      int *boundptr;
#endif
      next(); 
      *++e=(PSH);
      expr(Assign);     
#ifdef VMBOUNDS  
      if (((int)e-directarray)==(4*3) && *(e-2)==PSH && *(e-1)==IMM && (unsigned int)*e>upperbound) printf("Array out of bounds: defined %d used %d, line %d\n",upperbound,*e, line), exit(-1);
#endif
      if (tk == ']') next(); else { kprintf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { 
        *++e=(PSH); 
#ifdef VMBOUNDS        
        //runtime bounds check
        *++e=(PSH);                                              // push index value again
        *++e=(IMM);*++e=(upperbound);*++e=(GT);  *++e=(BZ);boundptr=++e;  // compare index>upperbound
           *++e=(IMM);*++e=(line); *++e=(PSH); *++e=(BOUND); *++e=(ADJ);*++e=(1); // fail if larger
        *boundptr = (int)(e + 1);
#endif
        *++e=(IMM);*++e=(((ty = t - PTR) == rCHAR) ? 1 : ((ty = t - PTR) == sSHORT) ? 2 : 4);
        *++e=(MUL);
      } //fixed to int !!!
      else if (t < PTR) { kprintf("%d: pointer type expected\n", line); exit(-1); }
      *++e=(ADD);
      char aa=((ty = t - PTR) == rCHAR) ? LC : ((ty = t - PTR) == sSHORT) ? LS : LI;//9 i 10 s 11 c
      *++e=(aa);
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
        *++e=(BZ) ; b = ++e;
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
        *++e=(BZ) ; b = ++e;
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
        *++e=(BZ) ; b = ++e;
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
 
int vmbound(int line){
    if (line!=0)printf("Bounds error line: %d\n",line);
    exit(-1);
}
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
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,BOUN,THIS,"
         "PRTF,VMS ,VMI ,VMX ,H2  ,READ,WRTE,EXIT"[i * 5]);
    if (i < JMP) kprintf(" %d\n",*pc); //? +1MALC,MSET,MCMP,MCPY,
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
    else if (i == VMS) a=0, components(this,sp[10],sp[9],sp[8],sp[7],sp[6], sp[5], sp[4], sp[3],sp[2], sp[1],*sp);
    else if (i == VMI) a=0, initcomponent(this, sp[4], sp[3],sp[2], sp[1],*sp);
    else if (i == VMX) a=0, setcomponent(this, sp[2], sp[1],*sp);
    else if (i == H2)  a = hash1((U32)sp[1], (U32)*sp);
    else if (i == VTHIS)  *--sp;  //ignore
    else if (i == BOUND) { /*printf("exit(%d) cycle = %d\n", *sp, cycle);*/ return vmbound(*sp); }
    else if (i == EXIT) { /*printf("exit(%d) cycle = %d\n", *sp, cycle);*/ return *sp; }
    else if (i == READ) a = (int)readfile(this,(U8 *)sp[1], *sp); //pointer,lenght
    else if (i == WRTE) a = (int)writefile(this,(U8 *)sp[1], *sp); //pointer,lenght
    else { kprintf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
    // if (debug) printf("a=%d ",a);
  }

}
 #endif
 /*
 void VM::gen(int *n)
{
  int i, *a, *b;

  i = *n;
  if (i == Num) { *++e = IMM; *++e = n[1]; }
  else if (i == Loc) { *++e = LEA; *++e = n[1]; }
  else if (i == Load) { gen(n+2); *++e = (((n[1] == rCHAR) ? LC : n[1] == sSHORT) ? LS : LI); }
  else if (i == Assign) { gen((int *)n[2]); *++e = PSH; gen(n+3); *++e = ((n[1] == rCHAR) ? SC : n[1] == sSHORT) ? SS : SI; }
  else if (i == Inc || i == Dec) {
    gen(n+2);
    *++e = PSH; *++e = (((n[1] == rCHAR) ? LC : n[1] == sSHORT) ? LS : LI); *++e = PSH;
    *++e = IMM; *++e = (n[1]  > PTR) ?  sizeof(int) : (n[1] > iINT) ?  sizeof(short) : sizeof(char);;
    *++e = (i == Inc) ? ADD : SUB;
    *++e = ((n[1] == rCHAR) ? SC : n[1] == sSHORT) ? SS : SI;
  }  
  else if (i == Cond) {
    gen((int *)n[1]);
    *++e = BZ; b = ++e;
    gen((int *)n[2]);
    if (n[3]) { *b = (int)(e + 3); *++e = JMP; b = ++e; gen((int *)n[3]); }
    *b = (int)(e + 1);
  }
  else if (i == Lor) { gen((int *)n[1]); *++e = BNZ; b = ++e; gen(n+2); *b = (int)(e + 1); }
  else if (i == Lan) { gen((int *)n[1]); *++e = BZ;  b = ++e; gen(n+2); *b = (int)(e + 1); }
  else if (i == Or)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = OR; }
  else if (i == Xor) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = XOR; }
  else if (i == And) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = AND; }
  else if (i == Eq)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = EQ; }
  else if (i == Ne)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = NE; }
  else if (i == Lt)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = LT; }
  else if (i == Gt)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = GT; }
  else if (i == Le)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = LE; }
  else if (i == Ge)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = GE; }
  else if (i == Shl) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SHL; }
  else if (i == Shr) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SHR; }
  else if (i == Add) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = ADD; }
  else if (i == Sub) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SUB; }
  else if (i == Mul) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = MUL; }
  else if (i == Div) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = DIV; }
  else if (i == Mod) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = MOD; }
  else if (i == Sys || i == Fun) {
    b = (int *)n[1];
    while (b) { gen(b+1); *++e = PSH; b = (int *)*b; }
    if (i == Fun) *++e = JSR; *++e = n[2];
    if (n[3]) { *++e = ADJ; *++e = n[3]; }
  }
  else if (i == While) {
    *++e = JMP; b = ++e; gen(n+2); *b = (int)(e + 1);
    gen((int *)n[1]);
    *++e = BNZ; *++e = (int)(b + 1);
  }
  else if (i == Return) { if (n[1]) gen((int *)n[1]); *++e = LEV; }
  else if (i == '{') { gen((int *)n[1]); gen(n+2); }
  else if (i == Enter) { *++e = ENT; *++e = n[1]; gen(n+2); *++e = LEV; }
  else if (i != ';') { printf("%d: compiler error gen=%d\n", line, i); exit(-1); }
}
*/
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
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,BOUN,THIS,"
         "PRTF,VMS ,VMI ,VMX ,H2  ,READ,WRTE,EXIT"[i * 5]);
    if (i < JMP) dprintf(" 0x%x\n",*(pc+1)); //? +1
     else if (i <= ADJ) dprintf(" 0x%x\n",(int *)*pc); else dprintf("\n");
    // }
    *pc++ = ((int)je << 8) | i; // for later relocation of JMP/JSR/BZ/BNZ
 
    if (i == LEA) {
      i = 4 * *pc++; if (i < -128 || i > 127) { kprintf("jit: LEA out of bounds\n"); return -1; }
      *(int*)je = 0x458d; je = je + 2; *je++ = i;
      dprintf("\tlea eax,[ebp%s%d]\n",i>=0?"+":"",i);
    }
    else if (i == ENT ) {
       i = 4 * *pc++; if (i < -128 || i > 127) { kprintf("jit: ENT out of bounds\n"); return -1; }
       if (*(pc)==LEV && i==0){
           *je++ = 0xc3; //ret
           dprintf("\tBlank proc\n\tret\n"); 
           *pc++;
       }else{
           *(int *)je = 0xe58955; je = je + 3;
           dprintf("\tpush ebp\n\tmov ebp, esp\n",i);
           if (i > 0) { 
           *(int *)je = 0xec83; je = je + 2; 
           *(int*)je++ = i; 
           dprintf("\tsub esp,BYTE %x\n",i); 
       }
       dprintf("\tpush esi push ecx push edx  push ebx push   edi\n"); 
       *(int *)je++ = 0x56;*(int *)je++ = 0x51;*(int *)je++ = 0x52;*(int *)je++ =0x53;*(int *)je++ =0x57;
      }
    }
    else if (i == IMM) { 
    if (*(pc+1)==LI){
         dprintf("   LI\n");//dprintf("// %x:    LI\n",pc);
        *je++ = 0xa1; *(int *)je = i=*pc++; je = je + 4; dprintf("\tmov eax,DWORD [0x%x]\n",i);
        i = *pc;*pc++ = ((int)je << 8) | i; 
    }
    else if (*(pc+1)==DIV){
		dprintf("DIV optimized\n");
         i=*(pc);*pc++; // imm value
        *je++ = 0x58;      dprintf("\tpop eax\n");     //pop eax
        *je++ = 0xb9; *(int *)je =i; je = je + 4; dprintf("\tmov ecx,DWORD 0x%x\n",i);
        *je++=0x99;         dprintf("\tcdq\n");    //cdq
        *je++=0xF7; *je++=0xF9; dprintf("\tidiv eax, ecx\n");//idiv eax, ecx
         i = *pc;//Div
        *pc++ = ((int)je << 8) | i;
    }
    else {
    *je++ = 0xb8; *(int *)je = i=*pc++; je = je + 4; dprintf("\tmov eax,DWORD 0x%x\n",i);}
    } 
    else if (i == ADJ) { i = 4 * *pc++; *(int *)je = 0xc483; je = je + 2; *(int *)je = i; je++; } // add esp,BYTE (n * 4)
	else if (i == PSH && *(pc)==IMM && (*(pc+1)==4/*|| *(pc+1)==2*/)&& *(pc+2)==MUL && *(pc+3)==ADD){	//array index*int
        *(int*)je = 0x59    ;je = je + 1; // pop ecx 
        *(int *)je = 0x81048d;            // lea    eax,[ecx+eax*4]
		dprintf("array index* optimized\n");
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
          je = je + 3; 
	}
    else if (i == PSH) {  *(int *)je++ = 0x50;dprintf("\tpush eax\n"); }
    else if (i == LEV) {
         *(int *)je++ = 0x5f;*(int *)je++ =0x5b;*(int *)je++ = 0x5a;*(int *)je++ = 0x59; *(int *)je++ = 0x5e; 
         dprintf("\tpop edi pop ebx pop edx pop ecx pop esi\n");
         *(int *)je = 0xc35dec89; je = je + 4; 
         dprintf("\tmov esp, ebp\n\tpop ebp\n\tret\n"); }
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
        dprintf("    %s\n",*pc==SHR?"SHR":*pc==SHL?"SHL":*pc==SUB?"SUB":*pc==ADD?"ADD":*pc==MUL?"MUL":*pc==OR?"OR":*pc==XOR?"XOR":"AND");
        dprintf("\tmov eax,DWORD PTR [eax]\n");
        dprintf("\tmov ecx,DWORD 0x%x\n",i);
		dprintf("\t%s eax,ecx\n",*pc==SHR?"SHR":*pc==SHL?"SHL":*pc==SUB?"SUB":*pc==ADD?"ADD":*pc==MUL?"MUL":*pc==OR?"OR":*pc==XOR?"XOR":"AND");
        i = *pc;//XXX
        *pc++ = ((int)je << 8) | i; 
		
        *(int*)je = i==SHR?0xe8d3:i==SHL?0xe0d3:i==SUB?0xc829:i==ADD?0xc801:i==MUL?0xc1af0f:i==OR?0xc809:i==XOR?0xc831:0xc821; //AND
		je = je + 2;//dprintf("\tXXX  ;optimized\n");  
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
    else if (i == DIV) { 
    *je++=0x59; *(int*)je = 0xF9F79991; je = je + 4;dprintf("\txor edx,edx\tpop ecx\n\txchg ecx, eax\n\tdiv eax, ecx\n"); 
    } // pop ecx; xchg ecx,eax; idiv eax,ecx
    else if (i == MOD) { *(int*)je = 0x999159; je += 3; *(int *)je = 0x92f9f7; je += 3; dprintf("\txor edx,edx\n\tpop ecx\n\txchg ecx,eax\n\tdiv ecx\n\txchg   edx,eax" ); }
    else if (i == JMP) { ++pc; *je       = 0xe9;     je = je + 5; dprintf("\tjmp  %x\n", *(pc-1) ); } // jmp <off32>
    else if (i == JSR) { ++pc; *je       = 0xe8;     je = je + 5; dprintf("\tcall  %x\n", *(pc-1) ); } // call <off32>
    else if (i == BZ)  { ++pc; *(int*)je = 0x840fc085; je = je + 8;dprintf("\ttest eax, eax\n\tjz  %x\n", *(pc-1) ); } // test %eax, %eax; jz <off32>
    else if (i == BNZ) { ++pc; *(int*)je = 0x850fc085; je = je + 8;dprintf("\ttest eax, eax\n\tjnz  %x\n", *(pc-1)  );  } // test %eax, %eax; jnz <off32>
    else if (i==H2){
         pc++;pc++;                    // skip H2 and ADJ
         *je=0x59; je++;               //     pop    ecx
         *je=0x58; je++;               //     pop    eax
         *je=0x05; je++;               //     add    eax,0x200
         *je=0x00; je++; 
         *je=0x02; je++;            
         *je=0x00; je++; 
         *je=0x00; je++;       
         *je=0xbb; je++;               //     mov    ebx,0x305
         *je=0x05; je++; 
         *je=0x03; je++;            
         *je=0x00; je++; 
         *je=0x00; je++; 
         *je=0xf7; je++;               //     mul    ebx
         *je=0xe3; je++;     
         *je=0x01; je++;               //     add    eax,ecx //|| lea eax, [ecx+395776+eax] 8d 84 02 00 0a 06 00
         *je=0xc8; je++; 
         dprintf("// H2 start\n");
         dprintf("\tpop ecx\n\t pop    eax\n\t add    eax,0x200\n");
         dprintf("\tmov    ebx,0x305\n\t mul    ebx\n\t add    eax,ecx\n");
         dprintf("// H2 end\n");
    }
    else if (i == VTHIS) { 
    *je++ = 0xb8; 
    *(int*)je =i=(unsigned int)(size_t(this));je += 4; *(int *)je++ = 0x50;dprintf("\tmov eax,DWORD %x\n\tpush eax    ;this\n",i); } //mov ecx,this b9
    else if (i >= PRTF || i==BOUND) {
        if (i == PRTF) { tmp = (int)printf;  }
        else if (i == EXIT) { tmp = (int)exit;  }
        else if (i == BOUND) { tmp = (int)vmbound;  }
        else if (i == VMS) { tmp = (int)components;  }
        else if (i == VMI) { tmp = (int)initcomponent;  }
        else if (i == VMX) { tmp = (int)setcomponent;  }
        //else if (i == H2) { tmp = (int)h2; }
        else if (i == READ) { tmp = (int)readfile;  }
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
        else if (u == EXIT) {  dprintf("exit"); }
        else if (u == VMS) {  dprintf("vms"); }else if (u == VMI) {  dprintf("vmi"); }
        else if (u == VMX) {  dprintf("vmx"); }
        //else if (u == H2) {  dprintf("h2");  } 
        else if (u== READ)  dprintf("read");  
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
    else if (i>EXIT) {kprintf("code generation failed. relocation error\n"); return -1; }
  }
  jitReadonly(jitmem,poolsz); //make jit read/execute only
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
int VM::getPrediction(int cInputs){
    int p;
    int prindex,index,compnr,mixnr;
        // mix inputs[0..x]
    if(cInputs>=0){     
        for (int j=0;j<cInputs+1;j++) {  
           for (int i=0;i< totalc;i++){
        int inputIndex=mcomp[i] &0xff ;    // index  
        int component=(mcomp[i]>>8)&0xff;
        if (j==inputIndex && (mcomp[i]>>24)==0 && (component==vmST|| component==vmSMC || component==vmRCM ||component==vmSCM || component==vmCM )) {
            int componentIndex=mcomp[i]>>16;    // component index
            switch (component) { // select component and mix
            case vmSMC: x.mxInputs[inputIndex].add(stretch(smA[componentIndex].pr));//smC[componentIndex]->mix(inputIndex);
                break;
            case vmRCM: rcmA[componentIndex].mix(x,inputIndex);
                break;
            case vmSCM: scmA[componentIndex].mix(x,inputIndex);
                break;
            case  vmCM: cmC[componentIndex]->mix(inputIndex);
                break;
            case  vmST: x.mxInputs[inputIndex].add(stA[componentIndex].pr);
                break;
            default:
                quit("VM mxp error\n");
                break;
            }
        }
    }
       }
    }
    for (int i=0;i<totalc;i++){
        prindex=mcomp[i]>>24;
        index=(mcomp[i]>>16)&0xff;
        compnr=(mcomp[i]>>8)&0xff;
        mixnr=mcomp[i]&0xff;
        // individual components        
        if ((prindex>0 || compnr==vmMM )){
          //if (compnr!=vmMM )printcomponent(compnr),printf("(%d)=",i);
            prindex--;
            switch (compnr) {
            case vmSMC: {
                prSize[prindex]=smA[index].pr; 
                break;
            }  
            case vmAPM1: {
                prSize[prindex]=apm1A[index].p(prSize[apm1A[index].p1],x.y);
                break;
            }
            case vmDS: {
                prSize[prindex]=dsA[index].p();
                break; 
            }
             case vmDHS: {
                prSize[prindex]=dhsA[index].p();
                break; 
            }
            case vmAVG: {
                prSize[prindex]=avA[index].average(prSize[avA[index].p1],prSize[avA[index].p2]);
                break; 
            }
            case vmMM: {
                mmA[index].p=prSize[ mmA[index].i1];
               // mmA[index].mix(mixnr,x);
                x.mxInputs[mixnr].add(mmA[index].pr());
                break; 
            }                
            case vmMX: {
                prSize[prindex]=mxA[index].p();
                break;  
            }
            case vmST: {
                 prSize[prindex]=stA[index].pr1;
                 break; 
            }
            /*case vmSK: {
                 return prSize[prindex-1]=stC[index]->p();
                 break; 
            }*/
            default:{
                quit("VM vmi error\n");
                break;}
            }
           // if (compnr!=vmMM )printf("%d ",prSize[prindex-1]);
        }
        
    }  
 //   if (compnr!=vmMM )printf("  \n" );
    
    p=prSize[prSize.size()-1]; //final prediction
    return p;
}
void  VM::updateComponents(){
    int prindex,index,compnr;
    if(x.cInputs>=0){      
        for (int i=0;i<totalc;i++){
            prindex=mcomp[i]>>24;
            compnr=(mcomp[i]>>8)&0xff;
            // individual components
            if (prindex>0 && compnr==vmMX){
                index=(mcomp[i]>>16)&0xff;
                //mxC[index]->update( mcomp[i]  &0xff); //update
                mxA[index].update(x.y);
            }
        }
        for (int j=0;j<x.cInputs+1;j++)  {
            x.mxInputs[j].ncount=0; //reset
        }
    }
}
int  VM::doupdate(int y,int c0, int bpos,U32 c4,int pos){
#ifdef VMJIT
  int (*jitmain)(int,U32,int,int,int); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)(int,U32,int,int,int) >(*(unsigned*)( idupdate[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
    jitmain(pos,c4,bpos,c0,y);
  return 0;
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
     dovm((int *)idupdate[Val]);
     return 0;
#endif
    
}
static   char cVar[] = "char else enum if int short return for sizeof while printf vms vmi vmx h2 read write exit void block update main detect decode encode";
  
int VM::initvm() { 
  poolsz = 2024*1024; // arbitrary size

  if (!(sym = (int *)malloc(poolsz))) { kprintf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(text = le = e = (int *)malloc(poolsz))) { kprintf("could not malloc(%d) text area\n", poolsz); return -1; }
  //if (!(data =data0= (char *)malloc(poolsz))) { kprintf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp =sp0= (int *)malloc(poolsz))) { kprintf("could not malloc(%d) stack area\n", poolsz); return -1; }
  if (!(ast = (int *)malloc(poolsz))) { kprintf("could not malloc(%d) abstract syntax tree area\n", poolsz); return -1; }
  //ast = ((int)ast + poolsz); // abstract syntax tree is most efficiently built as a stack
 data0 =data;
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  //memset(data, 0, poolsz);
  memset(sp, 0, poolsz);
   p =cVar;// "char else enum if int short return for sizeof while printf vms vmi vmx mxs h2 read write exit void block update main detect decode encode";
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
  //char *nam;
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
      //while (tk == Mul) { next(); ty = ty + PTR; } // global pointer char *name;
      if (tk != Id) { 
		if (tk== Assign) { // look for global variable assignment
		  int negative=0;
		  next();
		  if (tk == Sub) negative=1,next(); // if negative number
		  if (tk != Num) { kprintf("%d: bad glabal variable value. %d\n", line,tk); return -1; }
		  *((int *)data)=negative?(int)-ival:(int)ival;
		  id[Class] = Glo;
          id[Val] =(int)data;
          data = data + 4;
          next();
          if (tk == Comma){next();}
          continue;
		}
		kprintf("%d: bad global declaration\n", line); return -1;
	  }
      if (id[Class]) { kprintf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        /*nam=(char*)id[Name];
        printf("\n// FUNCTION: ");
        for (int y=0;y<id[IDLen];y++)printf("%c",nam[y]);
        printf("\n");*/
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
            //while (tk == Mul) { next(); ty = ty + PTR; } // local pointer ex. char *name;
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
         *++e=(  ENT);*++e=( i - loc);
        while (tk != '}') stmt();
        *++e=( LEV);
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
        if (tk==Id && id[Class] == Num) { // array size is enum value
           i=id[Val],tk=Num;              // set to Number toke with enum value
        }
        if (tk != Num )          { kprintf("%d: bad global declaration: size=%d\n", line,i); return -1; }
        if (i==0 )               { kprintf("%d: array to small \n", line); return -1; }
        id[UBound]=i;
        next();if (tk != ']')    { kprintf("%d: missing closing braket\n", line); return -1; }
        next();if (tk != Assign) { kprintf("%d: missing array assingn \n", line); return -1; }
        next();if (tk != '{')    { kprintf("%d: missing array { \n", line); return -1; }
        next();
        int count=0;
        if (tk == '}') { 
           // type name[size]={}
           // dynamic array, i=size idz=sizeof(type)
           data = data - 4; // move back to our data pointer
           *((int *)data)=(int)vmmalloc(this,i,(idz==0)?1:(idz==1)?2:4);
           //printf("Dynamic Alloc: %d(%d)\n",(idz==0)?1:(idz==1)?2:4,i);
           data = data + 4; // for next data pointer
           next();          // next token 
        }else{
            // static content type name[size]={0,...}
            // store data after pointer
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
      }
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        *((int *)data)=0;
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
int r=0;
#ifdef VMJIT
if (dojit()!=0) return -1;
 // run jitted code
  int (*jitmain)(); // c4 vm pushes first argument first, unlike cdecl
  jitmain = reinterpret_cast< int(*)() >(*(unsigned*)(idmain[Val]) >> 8 | ((unsigned)jitmem & 0xff000000));
  r= jitmain();

#else
 // setup stack
  bp=sp = (int *)((int)sp0 + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; 
  t = sp;
  *--sp = (int)t;
r= dovm((int *)idmain[Val]);
#endif
    //no code here, JIT kills this
return r;
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


