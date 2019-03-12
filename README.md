# paq8pxv

This is paq8 like compressor, witch uses config files for compression models, detection, data decoding and encoding.
Compression models and data encoding models will be saved into final archive, so that decompressor can use them when data is extracted.

paq8pxv compressor itself just a tool. It uses virtual machine, which compiles c like code to bytecode.
There is also x86 JIT version.

# Config files
Example detection conf:
```
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
# Forum
https://encode.ru/threads/3064-paq8pxv-virtual-machine
# History
This is based on PAQ8PXD_V62 and PAQ8PXD_V17v2 ( https://encode.ru/showthread.php?p=47706#post47706 )
First version (v1) ( https://encode.ru/threads/1464-Paq8p...ll=1#post59098 )
Original attempt here ( https://encode.ru/threads/1464-Paq8p...ll=1#post42973 ) ( attempt to mix VM from fpaqvm (vm/jit) to paq8pxd_v16 )

# Testing results
https://docs.google.com/spreadsheets/d/1IlSwEmr385-t6EUYO9HUZyKECeTEntMOP0NIP_8LSSg
