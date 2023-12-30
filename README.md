# paq8pxv

This is paq8 like compressor, witch uses config files for compression models, detection, data decoding and encoding.
Compression models and data decoding models will be saved into final archive, so that decompressor can use them when data is extracted.
Also main config file (conf.pxv) is stored compressed. Main compression routine is stored uncompressed.

paq8pxv uses process virtual machine, which compiles C like code to bytecode at runtime and executes it. 
Command line option -j allows also to use x86 as JIT target.

## Usage example
__paq8pxv.exe  -1 -t1 -j -bc -br -cconf.pxv data__

Compress with 1 thread (t1), using JIT mode (j), array bounds are tested at compile (bc) and runtime (br), config file is __conf.pxv__ and input file is __data__.

# Contents
[Overview](#overview)

[Config files](#config-files)
* [Example detection conf](#example-detection-conf)

[Data detection, transform and compression](#data-detection-transform-and-compression)
* [Main config](#main-config)
* [lpaq1 config](#lpaq1-config)
* [text config](#text-config)

[General .cfg/.dec compression](#general-cfgdec-compression)

[Components](#components)

[Forum](#forum)

[History](#history)

[Testing results](#testing-results)

# Overview
![image](https://github.com/kaitz/paq8pxv/assets/7313300/4a01a19b-e561-46d7-9c79-19d0a95dee7f)


# Config files
Main configuration is in conf.pxv file. 
* .det files are used for detecting some type of data
* .enc files are used to transform a detected file when compressing
* .dec files are used to transform a detected file when decompressing
* .cfg files are used for compression

.cfg and .dec files are compressed and added to archive if they are used.

## Example detection conf:
```c
// My custom X type detection
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
    // If detect state parameters recuested
    if (i==REQUEST){
        if (state==NONE)  return 0xffffffff;  // No state
        if (state==START) return jstart;      // Report data start
        if (state==END)   return jend;        // Report data end
        if (state==INFO)  return 0xffffffff;  // Report info if any
    }
    if (i==RESET){
        reset();
        return 0xffffffff;
    }
    buf1=(buf1<<8)|(buf0>>24);
    buf0=c4;
    // Detect header - is first four bytes 0xFFFFFFFF
    if (buf1==0xFFFFFFFF && mystart==0){
        mystart=i;
    }
    // Found possible start, report
    if (type==DEFAULT && mystart){
        type=YOURTYPE;
        state=START; 
        jstart=mystart-4;
        return state;
    }
    // Found end, report if our type
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
## Main config
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

## lpaq1 config
lpaq1.cfg is almost identical to [lpaq1.c](http://www.mattmahoney.net/dc/text.html#1440)

Usage example: __paq8pxv.exe -1 -t1 -j -clpaq1.pxv file__

|   |lpaq1.c|lpaq1.cfg|
| --- | --- | --- | 
| Time enwik8|123.39 sec| 476.20 sec (JIT)| 
| Compressed size enwik8|19755948|19790459|
| Compressed size enwik9|164508919|164876243|
| Mem|1539 MB|1631 MB| 

## text config

Usage example: __paq8pxv.exe -1 -t1 -j -ctext.pxv file__

| --- | enwik8 | --- |  --- |  enwik9 | --- |  --- |  Mem|
| --- | --- | --- |  --- |  --- | --- |  --- | --- |
| --- | Time sec(JIT)| size  (data)| size  (archive)| Time sec(JIT)| size (data)| size  (archive)|--- |
| [txtfast.cfg](https://github.com/kaitz/paq8pxv/blob/c7639a6689f75c68fa273701010cd882582babd8/text/txtfast.cfg) | 841.93|18774819| 18777946  |8203.44 |  154261050 |  154264178 |1667 MB|
| [txtfast.cfg](https://github.com/kaitz/paq8pxv/blob/2ed67d0809ba5727356661d60d01defe13a91ae7/text/txtfast.cfg)| 919.11 |  18502390 |  18505591 |  8920.17 |  151541809 |  151545011| 1667 MB|

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
[See detailed info about components](components.md)

# Forum
[paq8pxv - virtual machine](https://encode.su/threads/3064-paq8pxv-virtual-machine ) Encode's Forum.

# History
This is based on PAQ8PXD_V62 and [PAQ8PXD_V17v2]( https://encode.su/showthread.php?p=47706#post47706 )

[First version (v1)]( https://encode.su/threads/1464-Paq8pxd-dict?p=59098&viewfull=1#post59098 )

[Original attempt here]( https://encode.su/threads/1464-Paq8pxd-dict?p=42973&viewfull=1#post42973 ) ( attempt to mix VM from fpaqvm (vm/jit) to paq8pxd_v16 )

# Testing results
[Google spreadsheet file](https://docs.google.com/spreadsheets/d/1IlSwEmr385-t6EUYO9HUZyKECeTEntMOP0NIP_8LSSg)
