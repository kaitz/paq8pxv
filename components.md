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
// sixth parameter is output (pr index=-1,MX input=0...MX)
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
// sixth parameter is predictionIndex
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
// sixth parameter is 
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
// sixth parameter is index into pr[] array
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
// sixth parameter is 
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
// sixth parameter is predictionIndex
vmi(RCM,0,1024,0,0);
```
### CM
```c
// Create CM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is memory*4096, must be power of two
// forth parameter is count of contexts
// sixth parameter is predictionIndex
vmi(CM,0,32*4096,1,0);
```
### MX
```c
// Create MX component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(MX,0,0,1,2);
```
### ST
```c
// Create ST component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(ST,0,0,1,2);
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
// sixth parameter is mixer index

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
// sixth parameter is number of contexts

//main
// creates DHS with 10 context using 256MB of memory where state count per context is 16 (1<<4)
vmi(DHS,0,4,24,10);

// update
// set DHS contexts at the start of state update
for ( i=0; i<10; i++) {  vmx(DHS,0,cxt[i]);}

// update DHS contexts states where j is in range 0...16
vmx(DHS,0,j);

```
### SK
```c
// Create SK component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(SK,0,0,1,2);
```
