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
```
// Set APM1(0) context = c0
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is context
vmx(APM1,0,c0);
```
# Individual components

### SMC
```
// Create SMC component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(SMC,0,0,1,2);
```
### APM1
```
// Create APM1 component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(APM1,0,0,1,2);
```
### DS
```
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
```
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
```
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
```
// Create RCM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(RCM,0,0,1,2);
```
### CM
```
// Create CM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(CM,0,0,1,2);
```
### MX
```
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
```
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
```
// Create MM component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(MM,0,0,1,2);
```
### DHS
```
// Create DHS component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(DHS,0,0,1,2);
```
### SK
```
// Create SK component (0)
// 
// first parameter is component ID
// second parameter is component index upto number defined in vms
// third parameter is
// forth parameter is 
// sixth parameter is 
vmi(SK,0,0,1,2);
```
