# paq8pxv

This is paq8 like compressor, witch uses config files for compression models, detection, data decoding and encoding.
Compression models and data decoding models will be saved into final archive, so that decompressor can use them when data is extracted.
Also main config file (conf.pxv) is stored compressed. Main compression routine is stored uncompressed.

paq8pxv uses virtual machine, which compiles c like code to bytecode at runtime and executes it. Command line option -j allows also to use x86 JIT version.

Usage example:
__paq8pxv.exe  -1 -t1 -j -bc -br -cconf.pxv Testfile__

Testfile is compressed, 1 thread, using JIT mode, array bounds are tested at compile and runtime, config file is conf.pxv

# Config files
Main configuration is in conf.pxv file. 
* .det files are used for detecting some type of data
* .enc files are used to transform a detected file when compressing
* .dec files are used to transform a detected file when decompressing
* .cfg files are used for compression

.cfg and .dec files are compressed and added to archive if they are used.

## Example detection conf:
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
# Data detection, transform and compression

Usage is definded in main configuration file conf.pxv

|Type|Detect|Encode|Decode|Compress|Recursive|Description|
| --- | --- | --- | --- | --- | --- | --- |
| b64| b64.det|b64.enc |b64.dec | none |y| Base64 transform|
| bmp1| bmp1.det| none| none| test3img.cfg|n| 1bit .bmp image|
| bmp4| bmp4.det| none| none| test3im4.cfg|n| 4bit .bmp image|
| bmp8| bmp8.det| none| none| test3i8.cfg| n|8bit .bmp image|
| bmp24| bmp24.det| bmp24.enc| bmp24.dec| test3i24.cfg|n |24bit .bmp image|
| dec| dec.det| dec.enc| dec.dec| test3d.cfg|n | DEC Alpha executable code transform, swap byte order|
| exe| exe.det| exe.enc| exe.dec| test3exe.cfg|n |x86 executable code|
| arm| arm.det| arm.enc| arm.dec| test3d.cfg|n |arm executable code|
| text| text.det| text.enc|text.dec | test3txt.cfg| n|text|
| jpeg| jpeg.det| none| none| jpeg.cfg|n |jpeg image|
 

# General .cfg/.dec compression
Main compression routine used when compressing .cfg/.dec files and main config file (conf.pxv).
Stored uncompressed at the beginning of the output file.
```c
// update is called by vm for every input bit
// y    - last bit
// c0   - last 0-7 bits of the partial byte with a leading 1 bit (1-255)
// c4   - last 4,4 whole bytes, packed.  Last byte is bits 0-7.
// bpos - bits in c0 (0 to 7)
// pr   - last prediction
int t[5]={};
enum {SMC=1,APM1,DS,AVG,SCM,RCM,CM,MX,ST,MM,DHS};

int update(int y,int c0,int bpos,int c4,int pr){
    int i;
    if (bpos==0) {
        for (i=4; i>0; --i) t[i]=h2(t[i-1],c4&0xff);
    }
    for (i=1;i<5;++i) 
    vmx(DS,0,(c0)|(t[i]<<8));
    vmx(APM1,0,c0);
    return 0;
}

// called at the start of every new data type
// a - info
// b - reserved (not used and set to 0)
void block(int a,int b) {}

// called once at the start of compression
int main(){ 
    vms(0,1,1,3,0,0,0,0,0,0,0);
    vmi(DS,0,18,1023,4);        // pr[0]..pr[3]
    vmi(AVG,0,0,0,1);           // pr[4]=avg(pr[0],pr[1])
    vmi(AVG,1,0,2,3);           // pr[5]=avg(pr[2],pr[3])
    vmi(AVG,2,0,4,5);           // pr[6]=avg(pr[4],pr[5])
    vmi(APM1,0,256,7,6);        // pr[7]=apm(pr[6]) rate 7 -> pr[7] is final prediction
}

```

# Components
|Name| Short name| ID|Prediction|Mixer input|
| --- | --- | --- | --- | --- |  
|StateMapContext| SMC|1|yes|yes|
|APM1| APM1|2|yes|no|
|DynamicStateMap| DS |3|yes|no|
|AvgMap| AVG |4|yes|no|
|SmallStationaryContextMap| SCM|5|no|yes|
|RunContextMap |RCM|6|no|yes|
|ContextMap| CM|7|no|yes|
|Mixer |MX |8|yes|no|
|StaticMap |ST|9|yes|yes|
|MixMap| MM|10|no|yes|
|DynamicHashStateMap| DHS |11|yes|no|
|StationaryMap| SM |12|no|yes|
|SkMap| SK |13|no|yes|

[See detailed info about components](components.md)

Yes in prediction means that component outputs prediction into internal array pr[]

Yes in mixer input means that component outputs prediction into internal array inputs[], witch is used in MX component as inputs.

If both are yes then only one output can be selected in vmi function.


# Forum
https://encode.su/threads/3064-paq8pxv-virtual-machine
# History
This is based on PAQ8PXD_V62 and PAQ8PXD_V17v2 ( https://encode.su/showthread.php?p=47706#post47706 )
First version (v1) ( https://encode.su/threads/1464-Paq8pxd-dict?p=59098&viewfull=1#post59098 )
Original attempt here ( https://encode.su/threads/1464-Paq8pxd-dict?p=42973&viewfull=1#post42973 ) ( attempt to mix VM from fpaqvm (vm/jit) to paq8pxd_v16 )

# Testing results
https://docs.google.com/spreadsheets/d/1IlSwEmr385-t6EUYO9HUZyKECeTEntMOP0NIP_8LSSg
