# Components

|Name| Short name| ID|Prediction|Mixer input|
| --- | --- | --- | --- | --- |  
|StateMapContext| [SMC](#smc)|1|yes|yes|
|APM1| [APM1](#apm1)|2|yes|no|
|DynamicStateMap| [DS](#ds) |3|yes|no|
|AvgMap| [AVG](#avg) |4|yes|no|
|SmallStationaryContextMap| [SCM](#scm)|5|no|yes|
|RunContextMap |[RCM](#rcm)|6|no|yes|
|ContextMap| [CM](#cm)|7|no|yes|
|Mixer |[MX](#mx) |8|yes|no|
|StaticMap |[ST](#st)|9|yes|yes|
|MixMap| [MM](#mm)|10|no|yes|
|DynamicHashStateMap| [DHS](#dhs) |11|yes|no|
|StationaryMap| [SM](#sm) |12|no|yes|
|SkMap| [SK](#sk) |13|no|yes|
|ERR| [ERR](#err)|15|yes|no|
|TAPM| [TAPM](#tapm)|16|yes|no|
|UAS| [UAS](#uas)|17|yes|no|
|LMX| [LMX](#lmx)|18|yes|no|
|STA| [STA](#sta)|19|no|no|
# Functions used to set up components
## vms - component counts
vms(countOfSMC,countOfAPM1,countOfDS,...);

component counts, 11 parameters with ID order, like in Components table below. 
Only usable in function main()

```
// use:
// 0 SMC, 1 APM1,1 DS, 2 AVG, 0 ...
vms(0,1,1,2,0,0,0,0,0); 
```
## vmi - initialize component
vmi(Component,Index,parameter1,parameter2,parameter2);

or

vmi(Component,Index,parameter1,parameter2,inputsIndex);

initialize component upto number specified in vms. Only usable in function main()

## vmx - set component context
vmx(Component,Index,Context);

set component context. Only usable in function update(...)
```c
// Set APM1(0) context = c0
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is context
vmx(APM1,index,c0);
```
# Individual components

### SMC
```c
// Create SMC component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is memory size
// forth parameter is limit=1...1023
// fifth parameter is output (pr index=-1,MX input=0...MX)
//                    output=-1 use pr[index] where index=0...lastComponent
//                    output>=0 select MX component as output
//
// vmi(SMC,index,size,limit,output);
```
Prediction to mixer:
```c
// in update
vmx(SMC,0,val1);      //  set component SMC(0) context to val1
vmx(SMC,1,val2);      //  set component SMC(1) context to val2
// in main
vmi(SMC,0,0x10,1023,0);  //  mixer[0].add(smc(0).p())
vmi(SMC,1,0x10,1023,0);  //  mixer[0].add(smc(1).p())
```
Direct prediction:
```c
// in update
vmx(SMC,0,val1);      //  set component SMC(0) context to val1
vmx(SMC,1,val2);      //  set component SMC(1) context to val2
// in main
vmi(SMC,0,0x10,1023,-1);  //  pr[0]=smc(0).predict()
vmi(SMC,1,0x10,1023,-1);  //  pr[1]=smc(1).predict()
```
### APM1
```c
// Create APM1 component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is size
// forth parameter is rate
// fifth parameter is predictionIndex
//
// vmi(APM1,index,size,rate,predictionIndex);

// in update
vmx(SMC,0,val1);          //  set component SMC(0) context to val1
vmx(APM1,0,val2);      //  set component APM1(0) context to val2
// in main
vmi(SMC,0,0x10,1023,-1);  //  pr[0]=smc(0).predict()
vmi(APM1,0,0x1000,7,0);   //  pr[1]=apm(pr[0])

```
### DS
```c
// Create DS component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// fifth parameter is 
vmi(DS,0,0,1,2);
```
### AVG
```c
// Create AVG component (0)
// Calculate average and output prediction - pr[3]=(pr[1]+pr[2]+1)>>1
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third not used, set 0
// forth parameter is index into pr[] array
// fifth parameter is index into pr[] array
vmi(AVG,0,0,1,2);
```
### SCM
```c
// Create SCM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// fifth parameter is 
vmi(SCM,0,0,1,2);
```
### RCM
```c
// Create RCM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is memory*4096, must be power of two
// forth parameter is unused
// fifth parameter is predictionIndex
vmi(RCM,0,1024,0,0);
```
### CM
```c
// Create CM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is memory*4096, must be power of two
// forth parameter is count of contexts x, run mul y, mixer prediction mul z and w. y,z,w parameters are tunable.
// fifth parameter is predictionIndex

//main
//
vmi(CM,0,32*4096,x+y*0x100+z*0x10000+w*0x1000000,0);
```
### MX
```c
// Create MX component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// fifth parameter is 
vmi(MX,0,0,1,2);
```
### ST
```c
// Create ST component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is value m where pr=((m-128)*16) if fift parameter is -1, or pr=(m*16) if 0
// forth parameter is nil
// fifth parameter is output (pr index=-1,MX input=0...MX)
//                    output=-1 use pr[index] where index=0...lastComponent
//                    output>=0 select MX component as output

//main
//
vmi(ST,0,144,0,0);
```
### MM
```c
// Create MM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is option:
//         0 adds stretch(pr) to mixer
//         1 adds stretch(pr) >> 1  to mixer
//         3...x  (pr-2048 >> 3...x) to mixer
// forth parameter is pr index
// fifth parameter is mixer index

```
```c
// mixer[2].add(strech(pr[1]))
vmi(MM,0,0,1,2);
```
### DHS
```c
// Create DHS component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is input bits
// forth parameter is memory bits, memory usage is ((1<<bits)*(1<<memory))
// fifth parameter is number of contexts

//main
// creates DHS with 10 context using 256MB of memory where state count per context is 16 (1<<4)
vmi(DHS,0,4,24,10);

// update
// set DHS contexts at the start of state update
for ( i=0; i<10; i++) {  vmx(DHS,0,cxt[i]);}

// update DHS contexts states where j is in range 0...16
vmx(DHS,0,j);

```
### SM
```c
// Create SM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is memory_bits
// forth parameter is input_bits (low 8 bits), memory usage is N=((1<<memory_bits)*((1<<input_bits)-1)). pr mul value
// fifth parameter is number of contexts

//main
// creates SM
vmi(DHS,0,16,3,0);

// update
// set DHS contexts at the start of state update
vmx(SM, 0,val);


```
### SK
```c
// Create SK component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is nil
// forth parameter is nil
// fifth parameter is mixer index

//main
// creates SM
vmi(SK,0,0,0,0);

// update
// set SK value to be added to mixer, range -2047..2047
vmx(SK, 0,val);
```

### ERR
```c
// Create ERR component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is x and y, where x is first input and y is second input in range 0...4095. x and y parameters is tunable.
// forth parameter is nil
// fifth parameter is nil
// output: based on input tresholds output values are 1 or 3. 
//         1 - val>x (for low)
//         3 - val>y (for high)

//main
// creates SM
vmi(ERR,0,x+(y<<16),0,0);

// update
// set ERR value
val=y?pr^4095:pr;
a=vmx(ERR, 0,val);
```

### TAPM
```c
// Create TAPM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is parameters, tunable
// forth parameter is parameters, tunable
// fifth parameter is parameters, tunable

//main
// creates TAPM
vmi(TAPM,0,0,0,0);

// update
// set TAPM value
vmx(TAPM, 0,val);
```

### UAS
```c
// Create UAS component (0) - unaligned sparse
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is x, input size in bits. x parameter is tunable.
// forth parameter is y, input mask - ignored
// fifth parameter is z, update rate, dafault 5
// output: prediction when last 8 bits of val are set, otherwise prediction is 2048

//main
// creates UAS
vmi(UAS,0,x,y,z);
for (i=0;i<8;i++) vmi(ERR,i,e_low[i]+(e_high[i]<<16),0,0);

// update
// set UAS value, where val is shifted output of component ERR outputs
val=y?pr^4095:pr;
a=vmx(ERR, bpos,val); // ERR component for every bit pos
erra=(erra<<1)|(a&1);
if (bpos==0){ 
    val=erra;
}

vmx(UAS, 0,val);
```

### LMX
```c
// Create LMX component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is x and y, where x is first input and y is second input 
// forth parameter is weight z, if z==0 default value is 2048. z parameter is tunable.
// fifth parameter is nil
// output: x+(((z-x)*w)>>12);

//main
// creates LMX
vmi(LMX,0,x+y*256,z,0);

// update
// :none
```

### STA
```c
// Create STA component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is u and v (in range 1-63)
// forth parameter is w and x (in range 1-63)
// fifth parameter is y and z (in range 1-63)
// output: generates statetable;

//main
// creates STA
vmi(STA,0,u+v*0x10000,w+x*0x10000,y+z*0x10000);

// update
// :none
```
