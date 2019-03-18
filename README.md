# paq8pxv

This is paq8 like compressor, witch uses config files for compression models, detection, data decoding and encoding.
Compression models and data decoding models will be saved into final archive, so that decompressor can use them when data is extracted.
Also main config file is stored compressed. Main compression routine is stored uncompressed.

paq8pxv uses virtual machine, which compiles c like code to bytecode at runtime and executes it.
There is also x86 JIT version.

# Config files
Example detection conf:
```c
// For XXXX detection
int buf0,buf1,mystart;
int type,state,jstart,jend;
enum {DEFAULT=1,YOURTYPE}; //internal enum
enum {NONE=0,START,INFO,END,RESET=0xfffffffe,REQUEST=0xffffffff}; //external enum
// function will report its state 
// or if i=-1 then state results otherwise i is pos
// c4 is last 4 bytes
void reset(){
    state=NONE,type=DEFAULT,jstart=jend=buf0=buf1=mystart=0;
}
int detect(int c4,int i) {
    //if state parameters recuested
    if (i==REQUEST){
        if (state==NONE)  return 0xffffffff;
        if (state==START) return jstart;
        if (state==END)   return jend;
        if (state==INFO)  return 0xffffffff;
    }
    if (i==RESET){
        reset();
        return 0xffffffff;
    }
    buf1=(buf1<<8)|(buf0>>24);
    buf0=c4;
    //detect header
    if (buf1==0xFFFFFFFF && mystart==0){
        mystart=i;
    }
    if (type==DEFAULT && mystart){
        type=YOURTYPE;
        state=START; 
        jstart=mystart-4;
        return state;
    }
    if (i-mystart>0x100){
        if (type==YOURTYPE){
            state=END;
            type=DEFAULT;
            jend=i;
            return state;
      }
      state=NONE;
      type=DEFAULT;
      mystart=0;
    }
    return NONE;
}

int main() {
    reset();
}
```
Main compression routine used when compressing cfg/decode files and main config file.
Stored uncompressed at the beginning of the output file.
```c
int *cxt,*t;
int cxt1,cxt2,cxt3,cxt4,N;

// update is called by vm for every input bit
// output must be prediction 0...4095
// y    - last bit
// c0   - last 0-7 bits of the partial byte with a leading 1 bit (1-255)
// c4   - last 4,4 whole bytes, packed.  Last byte is bits 0-7.
// bpos - bits in c0 (0 to 7)
// pos  - current pos in input data
int update(int y,int c0,int bpos,int c4,int pos){
    int i,pr0;
    if (bpos==0) cxt4=cxt3,cxt3=cxt2,cxt2=cxt1,cxt1=buf(1)*256;
    for (i=0; i<N; ++i) t[cxt[i]]=smn(t[cxt[i]]);
    cxt[0]=(cxt1+c0);
    cxt[1]=(cxt2+c0+0x10000);
    cxt[2]=(cxt3+c0+0x20000);
    cxt[3]=(cxt4+c0+0x30000);
    pr0=0;
    for (i=0; i<4; ++i) pr0=pr0+smp(i,t[cxt[i]],1023);
    pr0=pr0>>2;
    return apm(0,pr0,c0,7);
}

// called at the start of every new data type
// a - info
// b - reserved (not used and set to 0)
void block(int a,int b) {}

// called once at the start of compression
int main() {
    int i;
    N=4;
    if (!(t = malloc((0x40000),sizeof(int)))) exit(-1);
    if (!(cxt = malloc(4,sizeof(int)))) exit(-1);
    // init, use N number of StateMap's, 1 APM
    vms(N,1,0,0,0,0,0,0,0);
    // init StateMap[i], context size 256, no mixer (-1)
    for (i=0;i<N;i++) vmi(1,i,256,0,-1);
    // init APM[0], context size 256, no mixer (-1)
    vmi(2,0,256,0,-1);
    cxt1=cxt2=cxt3=cxt4=0;
};
```
# Forum
https://encode.ru/threads/3064-paq8pxv-virtual-machine
# History
This is based on PAQ8PXD_V62 and PAQ8PXD_V17v2 ( https://encode.ru/showthread.php?p=47706#post47706 )
First version (v1) ( https://encode.ru/threads/1464-Paq8p...ll=1#post59098 )
Original attempt here ( https://encode.ru/threads/1464-Paq8p...ll=1#post42973 ) ( attempt to mix VM from fpaqvm (vm/jit) to paq8pxd_v16 )

# Testing results
https://docs.google.com/spreadsheets/d/1IlSwEmr385-t6EUYO9HUZyKECeTEntMOP0NIP_8LSSg
