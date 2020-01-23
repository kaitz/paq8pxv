    /* paq8pxv file compressor/archiver.  Release by Kaido Orav

    Copyright (C) 2008-2019 Matt Mahoney, Serge Osnach, Alexander Ratushnyak,
    Bill Pettis, Przemyslaw Skibinski, Matthew Fite, wowtiger, Andrew Paterson,
    Jan Ondrus, Andreas Morphis, Pavel L. Holoborodko, Kaido Orav, Simon Berger,
    Neill Corlett

    LICENSE

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details at
    Visit <http://www.gnu.org/copyleft/gpl.html>.

To install and use in Windows:

- To install, put paq8pxv.exe or a shortcut to it on your desktop.
- To compress a file or folder, drop it on the paq8pxv icon.
- To decompress, drop a .paq8pxv file on the icon.

A .paq8pxv extension is added for compression, removed for decompression.
The output will go in the same folder as the input.

While paq8pxv is working, a command window will appear and report
progress.  When it is done you can close the window by pressing
ENTER or clicking [X].


COMMAND LINE INTERFACE

- To install, put paq8pxv.exe somewhere in your PATH.
- To compress:      paq8pxv [-N] file1 [file2...]
- To decompress:    paq8pxv [-d] file1.paq8pxv [dir2]
- To view contents: paq8pxv -l file1.paq8pxv

The compressed output file is named by adding ".paq8pxv" extension to
the first named file (file1.paq8pxv).  Each file that exists will be
added to the archive and its name will be stored without a path.
The option -N specifies a compression level ranging from -0
(fastest) to -8 (smallest).  The default is -5.  If there is
no option and only one file, then the program will pause when
finished until you press the ENTER key (to support drag and drop).
If file1.paq8pxv exists then it is overwritten. Level -0 only
transforms or decompresses data.

If the first named file ends in ".paq8pxv" then it is assumed to be
an archive and the files within are extracted to the same directory
as the archive unless a different directory (dir2) is specified.
The -d option forces extraction even if there is not a ".paq8pxv"
extension.  If any output file already exists, then it is compared
with the archive content and the first byte that differs is reported.
No files are overwritten or deleted.  If there is only one argument
(no -d or dir2) then the program will pause when finished until
you press ENTER.

For compression, if any named file is actually a directory, then all
files and subdirectories are compressed, preserving the directory
structure, except that empty directories are not stored, and file
attributes (timestamps, permissions, etc.) are not preserved.
During extraction, directories are created as needed.  For example:

  paq8pxv -4 c:\tmp\foo bar

compresses foo and bar (if they exist) to c:\tmp\foo.paq8pxv at level 4.

  paq8pxv -d c:\tmp\foo.paq8pxv .

extracts foo and compares bar in the current directory.  If foo and bar
are directories then their contents are extracted/compared.

There are no commands to update an existing archive or to extract
part of an archive.  Files and archives larger than 2GB are not
supported (but might work on 64-bit machines, not tested).
File names with nonprintable characters are not supported (spaces
are OK).


TO COMPILE

There are 2 files: paq8pxv.cpp (C++) and wrtpre.cpp (C++).
paq8pxv.cpp recognizes the following compiler options:

  -DWINDOWS           (to compile in Windows)
  -DUNIX              (to compile in Unix, Linux, etc)
  -DMT                (to compile with multithreading support)
  -DDEFAULT_OPTION=N  (to change the default compression level from 8 to N).

If you compile without -DWINDOWS or -DUNIX, you can still compress files,
but you cannot compress directories or create them during extraction.
You can extract directories if you manually create the empty directories
first.

Use -DEFAULT_OPTION=N to change the default compression level to support
drag and drop on machines with less than 256 MB of memory.  Use
-DDEFAULT_OPTION=4 for 128 MB, 3 for 64 MB, 2 for 32 MB, etc.


Recommended compiler commands and optimizations:

  MINGW g++ (x86,x64):
   with multithreading:
    g++ paq8pxv.cpp -DWINDOWS -DMT -msse2 -O2 -s -static -o paq8pxv.exe 
   without multithreading:
    g++ paq8pxv.cpp -DWINDOWS -msse2 -O2 -s -static -o paq8pxv.exe 

  UNIX/Linux (PC x86,x64):
   with multithreading:
    g++ paq8pxv.cpp -DUNIX -DMT -msse2 -O2 -s -static -lpthread -o paq8pxv
   without multithreading:
    g++ paq8pxv.cpp -DUNIX -msse2 -O2 -s -static -lpthread -o paq8pxv

  Non PC (e.g. PowerPC under MacOS X)
    g++ paq8pxv.cpp -O2 -DUNIX -s -o paq8pxv


ARCHIVE FILE FORMAT

An archive has the following format.  

  paq8pxv -N 
  segment offset 8 bytes
  segment size  2 bytes
  compressed segment size 2 bytes
  streams (0b00000xxx xxxxxxxx) 2 bytes
  \0 file list size
  compressed file list(
    size TAB filename CR LF
    size TAB filename CR LF
    ...)
  compressed binary data
  file segmentation data
  stream data sizes[11]

-N is the option (-0 to -15) and mode, even if a default was used.
00LMNNNN bit M is set if fast mode, 
         bit L is set if quick mode,
         if L or M are not set default to slow mode.

segment size is total size of file(s) 
compressed segment size is compressed segmentation data in bytes
at segmnet offset after compressed binary data.

file segmentation data is full list of detected blocks:
type size info
type size info
type size 
type size info
.....

info is present if block type needs extra info like in image or audio.

streams - if bit is set then stream is present. Right to left order
for stream 10 to 0. If bit is set store stream lengt to archive.

Plain file names are stored without a path.  Files in compressed
directories are stored with path relative to the compressed directory
(using UNIX style forward slashes "/").  For example, given these files:

  123 C:\dir1\file1.txt
  456 C:\dir2\file2.txt

Then

  paq8pxv archive \dir1\file1.txt \dir2

will create archive.paq8pxv 

The command:

  paq8pxv archive.paq8pxv C:\dir3

will create the files:

  C:\dir3\file1.txt
  C:\dir3\dir2\file2.txt

Decompression will fail if the first 10 bytes are not "paq8pxv -".  Sizes
are stored as decimal numbers.  CR, LF, TAB are ASCII codes
13, 10, 9 respectively.


ARITHMETIC CODING

The binary data is arithmetic coded as the shortest base 256 fixed point
number x = SUM_i x_i 256^-1-i such that p(<y) <= x < p(<=y), where y is the
input string, x_i is the i'th coded byte, p(<y) (and p(<=y)) means the
probability that a string is lexicographcally less than (less than
or equal to) y according to the model, _ denotes subscript, and ^ denotes
exponentiation.

The model p(y) for y is a conditional bit stream,
p(y) = PROD_j p(y_j | y_0..j-1) where y_0..j-1 denotes the first j
bits of y, and y_j is the next bit.  Compression depends almost entirely
on the ability to predict the next bit accurately.


MODEL MIXING

paq8pxv uses a neural network to combine a large number of models.  The
i'th model independently predicts
p1_i = p(y_j = 1 | y_0..j-1), p0_i = 1 - p1_i.
The network computes the next bit probabilty

  p1 = squash(SUM_i w_i t_i), p0 = 1 - p1                        (1)

where t_i = stretch(p1_i) is the i'th input, p1_i is the prediction of
the i'th model, p1 is the output prediction, stretch(p) = ln(p/(1-p)),
and squash(s) = 1/(1+exp(-s)).  Note that squash() and stretch() are
inverses of each other.

After bit y_j (0 or 1) is received, the network is trained:

  w_i := w_i + eta t_i (y_j - p1)                                (2)

where eta is an ad-hoc learning rate, t_i is the i'th input, (y_j - p1)
is the prediction error for the j'th input but, and w_i is the i'th
weight.  Note that this differs from back propagation:

  w_i := w_i + eta t_i (y_j - p1) p0 p1                          (3)

which is a gradient descent in weight space to minimize root mean square
error.  Rather, the goal in compression is to minimize coding cost,
which is -log(p0) if y = 1 or -log(p1) if y = 0.  Taking
the partial derivative of cost with respect to w_i yields (2).


MODELS

Most models are context models.  A function of the context (last few
bytes) is mapped by a lookup table or hash table to a state which depends
on the bit history (prior sequence of 0 and 1 bits seen in this context).
The bit history is then mapped to p1_i by a fixed or adaptive function.
There are several types of bit history states:

- Run Map. The state is (b,n) where b is the last bit seen (0 or 1) and
  n is the number of consecutive times this value was seen.  The initial
  state is (0,0).  The output is computed directly:

    t_i = (2b - 1)K log(n + 1).

  where K is ad-hoc, around 4 to 10.  When bit y_j is seen, the state
  is updated:

    (b,n) := (b,n+1) if y_j = b, else (y_j,1).

- Stationary Map.  The state is p, initially 1/2.  The output is
  t_i = stretch(p).  The state is updated at ad-hoc rate K (around 0.01):

    p := p + K(y_j - p)

- Nonstationary Map.  This is a compromise between a stationary map, which
  assumes uniform statistics, and a run map, which adapts quickly by
  discarding old statistics.  An 8 bit state represents (n0,n1,h), initially
  (0,0,0) where:

    n0 is the number of 0 bits seen "recently".
    n1 is the number of 1 bits seen "recently".
    n = n0 + n1.
    h is the full bit history for 0 <= n <= 4,
      the last bit seen (0 or 1) if 5 <= n <= 15,
      0 for n >= 16.

  The primaty output is t_i := stretch(sm(n0,n1,h)), where sm(.) is
  a stationary map with K = 1/256, initialized to
  sm(n0,n1,h) = (n1+(1/64))/(n+2/64).  Four additional inputs are also
  be computed to improve compression slightly:

    p1_i = sm(n0,n1,h)
    p0_i = 1 - p1_i
    t_i   := stretch(p_1)
    t_i+1 := K1 (p1_i - p0_i)
    t_i+2 := K2 stretch(p1) if n0 = 0, -K2 stretch(p1) if n1 = 0, else 0
    t_i+3 := K3 (-p0_i if n1 = 0, p1_i if n0 = 0, else 0)
    t_i+4 := K3 (-p0_i if n0 = 0, p1_i if n1 = 0, else 0)

  where K1..K4 are ad-hoc constants.

  h is updated as follows:
    If n < 4, append y_j to h.
    Else if n <= 16, set h := y_j.
    Else h = 0.

  The update rule is biased toward newer data in a way that allows
  n0 or n1, but not both, to grow large by discarding counts of the
  opposite bit.  Large counts are incremented probabilistically.
  Specifically, when y_j = 0 then the update rule is:

    n0 := n0 + 1, n < 29
          n0 + 1 with probability 2^(27-n0)/2 else n0, 29 <= n0 < 41
          n0, n = 41.
    n1 := n1, n1 <= 5
          round(8/3 lg n1), if n1 > 5

  swapping (n0,n1) when y_j = 1.

  Furthermore, to allow an 8 bit representation for (n0,n1,h), states
  exceeding the following values of n0 or n1 are replaced with the
  state with the closest ratio n0:n1 obtained by decrementing the
  smaller count: (41,0,h), (40,1,h), (12,2,h), (5,3,h), (4,4,h),
  (3,5,h), (2,12,h), (1,40,h), (0,41,h).  For example:
  (12,2,1) 0-> (7,1,0) because there is no state (13,2,0).

- Match Model.  The state is (c,b), initially (0,0), where c is 1 if
  the context was previously seen, else 0, and b is the next bit in
  this context.  The prediction is:

    t_i := (2b - 1)Kc log(m + 1)

  where m is the length of the context.  The update rule is c := 1,
  b := y_j.  A match model can be implemented efficiently by storing
  input in a buffer and storing pointers into the buffer into a hash
  table indexed by context.  Then c is indicated by a hash table entry
  and b can be retrieved from the buffer.


CONTEXTS

High compression is achieved by combining a large number of contexts.
Most (not all) contexts start on a byte boundary and end on the bit
immediately preceding the predicted bit.  The contexts below are
modeled with both a run map and a nonstationary map unless indicated.

ARCHITECTURE

The context models are mixed by several of several hundred neural networks
selected by a low-order context.  The outputs of these networks are
combined using a second neural network, then fed through several stages of
adaptive probability maps (APM) before arithmetic coding.

For images, only one neural network is used and its context is fixed.

An APM is a stationary map combining a context and an input probability.
The input probability is stretched and divided into 32 segments to
combine with other contexts.  The output is interpolated between two
adjacent quantized values of stretch(p1).  

PREPROCESSING

paq8pxv uses preprocessing transforms on certain data types to improve
compression.  To improve reliability, the decoding transform is
tested during compression to ensure that the input file can be
restored.  If the decoder output is not identical to the input file
due to a bug, then the transform is abandoned and the data is compressed
without a transform so that it will still decompress correctly.

The input is split into blocks with the format <type> <decoded size> <info>
where <type> is 1 byte (0 = no transform), <decoded size> is the size
of the data after decoding, which may be different than the size of <data>.
Data is stored uncompressed after compressed data ends.
The preprocessor has 3 parts:

- Detector.  Splits the input into smaller blocks depending on data type.

- Coder.  Input is a block to be compressed.  Output is a temporary
  file.  The coder determines whether a transform is to be applied
  based on file type, and if so, which one.  A coder may use lots
  of resources (memory, time) and make multiple passes through the
  input file.  The file type is stored (as one byte) during compression.

- Decoder.  Performs the inverse transform of the coder.  It uses few
  resorces (fast, low memory) and runs in a single pass (stream oriented).
  It takes input either from a file or the arithmetic decoder.  Each call
  to the decoder returns a single decoded byte.

IMPLEMENTATION

Hash tables are designed to minimize cache misses, which consume most
of the CPU time.

Most of the memory is used by the nonstationary context models.
Contexts are represented by 32 bits, possibly a hash.  These are
mapped to a bit history, represented by 1 byte.  The hash table is
organized into 64-byte buckets on cache line boundaries.  Each bucket
contains 7 x 7 bit histories, 7 16-bit checksums, and a 2 element LRU
queue packed into one byte.  Each 7 byte element represents 7 histories
for a context ending on a 3-bit boundary plus 0-2 more bits.  One
element (for bits 0-1, which have 4 unused bytes) also contains a run model
consisting of the last byte seen and a count (as 1 byte each).

Run models use 4 byte hash elements consisting of a 2 byte checksum, a
repeat count (0-255) and the byte value.  The count also serves as
a priority.

Stationary models are most appropriate for small contexts, so the
context is used as a direct table lookup without hashing.

The inner loops of the neural network prediction (1) and training (2)
algorithms are implemented in SIMD assembler, which computes 4 or more
elements at a time.

*/

#define VERSION "16"
#define PROGNAME "paq8pxv" VERSION  // Please change this if you change the program.
#define SIMD_GET_SSE                // uncomment to use SSE2 in ContexMap
#define MT                          // uncomment for multithreading, compression only
#define VMJIT                       // uncomment to compile with x86 JIT
//#define SIMD_CM_R                   // uncomment to SIMD ContextMap byterun

#define VMBOUNDS                    // uncomment to aad bounds chack at runtime

#ifdef WINDOWS
#ifdef MT
//#define PTHREAD       //uncomment to force pthread to igore windows native threads
#endif
#endif

#ifdef UNIX
#ifdef MT
#define PTHREAD 1
#endif
#endif
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
//#include <math.h>
#define NDEBUG  // remove for debugging (turns on Array bound checks)
#include <assert.h>
#ifdef MT
#include <vector>
#endif
#ifdef UNIX
#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <memory.h>
#include <cstdio>
#include <ctype.h>
#include <sys/cdefs.h>
#include <dirent.h>
#include <errno.h>
#endif

#ifdef WINDOWS
#include <windows.h>
#endif

#ifndef DEFAULT_OPTION
#define DEFAULT_OPTION 8
#endif
#include <stdint.h>
#ifdef _MSC_VER
//
typedef __int32 int32_t;
typedef unsigned __int32 uint32_t;
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;

#endif
// 8, 16, 32 bit unsigned types (adjust as appropriate)
typedef unsigned char  U8;
typedef unsigned short U16;
typedef unsigned int   U32;
typedef uint64_t   U64;
typedef signed char int8_t;

// min, max functions
#if  !defined(WINDOWS) || !defined (min)
inline int min(int a, int b) {return a<b?a:b;}
inline int max(int a, int b) {return a<b?b:a;}
#endif

#if defined(WINDOWS) || defined(_MSC_VER)
    #define atoll(S) _atoi64(S)
#endif

#ifdef _MSC_VER  
#define fseeko(a,b,c) _fseeki64(a,b,c)
#define ftello(a) _ftelli64(a)
#else
#ifndef UNIX
#ifndef fseeko
#define fseeko(a,b,c) fseeko64(a,b,c)
#endif
#ifndef ftello
#define ftello(a) ftello64(a)
#endif
#endif
#endif

#ifdef MT
#ifdef PTHREAD
#include "pthread.h"
#endif
#endif
#define ispowerof2(x) ((x&(x-1))==0)

// Error handler: print message if any, and exit
void quit(const char* message=0) {
    #ifdef  MT 
    printf("%s",message);
    #endif
  throw message;
}

// strings are equal ignoring case?
int equals(const char* a, const char* b) {
  assert(a && b);
  while (*a && *b) {
    int c1=*a;
    if (c1>='A'&&c1<='Z') c1+='a'-'A';
    int c2=*b;
    if (c2>='A'&&c2<='Z') c2+='a'-'A';
    if (c1!=c2) return 0;
    ++a;
    ++b;
  }
  return *a==*b;
}

//////////////////////// Program Checker /////////////////////

// Track time and memory used
class ProgramChecker {
    private:
  U64 memused;  // bytes allocated by Array<T> now
  U64 maxmem;   // most bytes allocated ever
  clock_t start_time;  // in ticks
public:
   void alloc(U64 n) {  // report memory allocated
    memused+=n;
    if (memused>maxmem) maxmem=memused;
  }
  void free(U64 n) {  // free memory 
  if (memused<n) memused=0;
  else  memused-=n;
  }
  ProgramChecker(): memused(0), maxmem(0) {
    start_time=clock();
    assert(sizeof(U8)==1);
    assert(sizeof(U16)==2);
    assert(sizeof(U32)==4);
    assert(sizeof(U64)==8);
    assert(sizeof(short)==2);
    assert(sizeof(int)==4);  
  }
  void print() const {  // print time and memory used
    printf("Time %1.2f sec, used %d MB (%d bytes) of memory\n",
      double(clock()-start_time)/CLOCKS_PER_SEC, (U32)((maxmem)/1024)/1024,(U32)(maxmem));
  }
} programChecker;

//////////////////////////// Array ////////////////////////////

// Array<T,Align> a(n); allocates memory for n elements of T.
// The base address is aligned if the "alignment" parameter is given.
// Constructors for T are not called, the allocated memory is initialized to 0s.
// It's the caller's responsibility to populate the array with elements.
// Parameters are checked and indexing is bounds checked if assertions are on.
// Use of copy and assignment constructors are not supported.
//
// a.size(): returns the number of T elements currently in the array.
// a.resize(newsize): grows or shrinks the array.
// a.append(x): appends x to the end of the array and reserving space for more elements if needed.
// a.pop_back(): removes the last element by reducing the size by one (but does not free memory).
#ifndef NDEBUG
static void chkindex(U64 index, U64 upper_bound) {
  if (index>=upper_bound) {
    fprintf(stderr, "out of upper bound %d\n",index);
    quit();
  }
}
#endif

template <class T, const int Align=16> class Array {
private:
  U64 used_size;
  U64 reserved_size;
  char *ptr; // Address of allocated memory (may not be aligned)
  T* data;   // Aligned base address of the elements, (ptr <= T)
  void create(U64 requested_size);
  inline U64 padding() const {return Align-1;}
  inline U64 allocated_bytes() const {return (reserved_size==0)?0:reserved_size*sizeof(T)+padding();}
public:
  explicit Array(U64 requested_size) {create(requested_size);}
  ~Array();
  T& operator[](U64 i) {
    #ifndef NDEBUG
    chkindex(i,used_size);
    #endif
    return data[i];
  }
  const T& operator[](U64 i) const {
    #ifndef NDEBUG
    chkindex(i,used_size);
    #endif
    return data[i];
  }
  U64 size() const {return used_size;}
  void resize(U64 new_size);
  void pop_back() {assert(used_size>0); --used_size; }  // decrement size
  void push_back(const T& x);  // increment size, append x
  Array(const Array&) { assert(false); } //prevent copying - this method must be public (gcc must see it but actually won't use it)
private:
  Array& operator=(const Array&); //prevent assignment
};

template<class T, const int Align> void Array<T,Align>::create(U64 requested_size) {
  assert((Align&(Align-1))==0);
  used_size=reserved_size=requested_size;
  if (requested_size==0) {
    data=0;ptr=0;
    return;
  }
  U64 bytes_to_allocate=allocated_bytes();
  ptr=(char*)calloc(bytes_to_allocate,1);
  if(!ptr){
      printf("Requested size %d MB\n",(U32)((bytes_to_allocate)/1024)/1024);
      #ifdef MT
      printf("Try using lower option ex. -s7 or reduce thread count.\n");
      #endif
      quit("Out of memory.");
  }
  U64 pad=padding();
  data=(T*)(((uintptr_t)ptr+pad) & ~(uintptr_t)pad);
  assert(ptr<=(char*)data && (char*)data<=ptr+Align);
  assert(((uintptr_t)data & (Align-1))==0); //aligned as expected?
  programChecker.alloc(bytes_to_allocate);
}

template<class T, const int Align> void Array<T,Align>::resize(U64 new_size) {
  if (new_size<=reserved_size) {
    used_size=new_size;
    return;
  }
  char *old_ptr=ptr;
  T *old_data=data;
  U64 old_size=used_size;
  programChecker.free(allocated_bytes());
  create(new_size);
  if(old_size>0) {
    assert(old_ptr && old_data);
    memcpy(data, old_data, sizeof(T)*old_size);
  }
  if(old_ptr){free(old_ptr);old_ptr=0;}
}

template<class T, const int Align> void Array<T,Align>::push_back(const T& x) {
  if(used_size==reserved_size) {
    U64 old_size=used_size;
    U64 new_size=used_size*2+16;
    resize(new_size);
    used_size=old_size;
  }
  data[used_size++]=x;
}

template<class T, const int Align> Array<T, Align>::~Array() {
  programChecker.free(allocated_bytes());
  free(ptr);
  used_size=reserved_size=0;
  data=0;ptr=0;
}


/////////////////////////// String /////////////////////////////

// A tiny subset of std::string
// size() includes NUL terminator.

class String: public Array<char> {
public:
  const char* c_str() const {return &(*this)[0];}
  void operator=(const char* s) {
    resize(strlen(s)+1);
    strcpy(&(*this)[0], s);
  }
  void operator+=(const char* s) {
    assert(s);
    pop_back();
    while (*s) push_back(*s++);
    push_back(0);
  }
  String(const char* s=""): Array<char>(1) {
    (*this)+=s;
  }
};

/////////////////////////// IO classes //////////////////////////
// These classes  take the responsibility for all the file/folder
// operations.

/////////////////////////// Folders /////////////////////////////
bool makedir(const char* dir) {
  struct stat status;
  bool success = stat(dir, &status)==0;
  if(success && (status.st_mode & S_IFDIR)!=0) return -1; //-1: directory already exists, no need to create
#ifdef WINDOWS
  return CreateDirectory(dir, 0)==TRUE;
#else
#ifdef UNIX
  return mkdir(dir, 0777)==0;
#else
  return false;
#endif
#endif
}

void makedirectories(const char* filename) {
  String path(filename);
  int start = 0;
  if(path[1]==':')start=2; //skip drive letter (c:)
  if(path[start] == '/' || path[start] == '\\')start++; //skip leading slashes (root dir)
  for (int i = start; path[i]; ++i) {
    if (path[i] == '/' || path[i] == '\\') {
      char savechar = path[i];
      path[i] = 0;
      const char* dirname = path.c_str();
      int created = makedir(dirname);
      if (created==0) {
        printf("Unable to create directory %s\n", dirname);
        quit();
      }
      if(created==1)
       // printf("Created directory %s\n", dirname);
      path[i] = savechar;
    }
  }
}
/////////////////////////// File /////////////////////////////
// The main purpose of these classes is to keep temporary files in 
// RAM as mush as possible. The default behaviour is to simply pass 
// function calls to the operating system - except in case of temporary 
// files.

// Helper function: create a temporary file
//
// On Windows when using tmpfile() the temporary file may be created 
// in the root directory causing access denied error when User Account Control (UAC) is on.
// To avoid this issue with tmpfile() we simply use fopen() instead.
// We create the temporary file in the directory where the executable is launched from. 
// Luckily the MS C runtime library provides two (MS specific) fopen() flags: "T"emporary and "D"elete.


FILE* tmpfile2(void){
    FILE *f;
#if defined(WINDOWS)    
    int i;
    char temppath[MAX_PATH]; 
    char filename[MAX_PATH];
    
    //i=GetTempPath(MAX_PATH,temppath);          //store temp file in system temp path
    i=GetModuleFileName(NULL,temppath,MAX_PATH); //store temp file in program folder
    if ((i==0) || (i>MAX_PATH)) return NULL;
    char *p=strrchr(temppath, '\\');
    if (p==0) return NULL;
    p++;*p=0;
    if (GetTempFileName(temppath,"tmp",0,filename)==0) return NULL;
    f=fopen(filename,"w+bTD");
    if (f==NULL) unlink(filename);
    return f;
#else
    f=tmpfile();  // temporary file
    if (!f) return NULL;
    return f;
#endif
}

//This is the base class.
//This is an abstract class for all the required file operations.
class File {
public:
  virtual ~File(){};// = default;
  virtual bool open(const char* filename, bool must_succeed) = 0;
  virtual void create(const char* filename) = 0;
  virtual void close() = 0;
  virtual int getc() = 0;
  virtual void putc(U8 c) = 0;
  void append(const char* s) { for (int i = 0; s[i]; i++)putc(s[i]); }
  virtual U64 blockread(U8 *ptr, U64 count) = 0;
  virtual U64 blockwrite(U8 *ptr, U64 count) = 0;
  U32 get32() { return (getc() << 24) | (getc() << 16) | (getc() << 8) | (getc()); }
  void put32(U32 x){putc((x >> 24) & 255); putc((x >> 16) & 255); putc((x >> 8) & 255); putc(x & 255);}
  U64 get64() { return ((U64)getc() << 56) | ((U64)getc() << 48) | ((U64)getc() << 40) | ((U64)getc() << 32) | (getc() << 24) | (getc() << 16) | (getc() << 8) | (getc()); }
  void put64(U64 x){putc((x >> 56) & 255);putc((x >> 48) & 255);putc((x >> 40) & 255);putc((x >> 32) & 255);putc((x >> 24) & 255); putc((x >> 16) & 255); putc((x >> 8) & 255); putc(x & 255);}
  virtual void setpos(U64 newpos) = 0;
  virtual void setend() = 0;
  virtual U64 curpos() = 0;
  virtual bool eof() = 0;
};

// This class is responsible for files on disk
// It simply passes function calls to the operating system
class FileDisk :public File {
protected:
  FILE *file;
public:
  FileDisk() {file=0;}
   ~FileDisk() {close();}
  bool open(const char *filename, bool must_succeed) {
    assert(file==0); 
    file = fopen(filename, "rb"); 
    bool success=(file!=0);
    if(!success && must_succeed)printf("FileDisk: unable to open file (%s)\n", strerror(errno));
    return success; 
  }
  void create(const char *filename) { 
    assert(file==0); 
    makedirectories(filename); 
    file=fopen(filename, "wb+");
    if (!file) quit("FileDisk: unable to create file"); 
  }
  void createtmp() { 
    assert(file==0); 
    file = tmpfile2(); 
    if (!file) quit("FileDisk: unable to create temporary file"); 
  }
  void close() { if(file) fclose(file); file=0;}
  int getc() { return fgetc(file); }
  void putc(U8 c) { fputc(c, file); }
  U64 blockread(U8 *ptr, U64 count) {return fread(ptr,1,count,file);}
  U64 blockwrite(U8 *ptr, U64 count) {return fwrite(ptr,1,count,file);}
  void setpos(U64 newpos) { fseeko(file, newpos, SEEK_SET); }
  void setend() { fseeko(file, 0, SEEK_END); }
  U64 curpos() { return ftello(file); }
  bool eof() { return feof(file)!=0; }
};

// This class is responsible for temporary files in RAM or on disk
// Initially it uses RAM for temporary file content.
// In case of the content size in RAM grows too large, it is written to disk, 
// the RAM is freed and all subsequent file operations will use the file on disk.
class FileTmp :public File {
private:
  //file content in ram
  Array<U8> *content_in_ram; //content of file
  U64 filepos;
  U64 filesize;
  void forget_content_in_ram()
  {
    if (content_in_ram) {
      delete content_in_ram;
      content_in_ram = 0;
      filepos = 0;
      filesize = 0;
    }
  }
  //file on disk
  FileDisk *file_on_disk;
  void forget_file_on_disk()
  {
    if (file_on_disk) {
      (*file_on_disk).close(); 
      delete file_on_disk;
      file_on_disk = 0;
    }
  }
  //switch: ram->disk
  const U32 MAX_RAM_FOR_TMP_CONTENT ; //64 MB (per file)
  void ram_to_disk()
  {
    assert(file_on_disk==0);
    file_on_disk = new FileDisk();
    (*file_on_disk).createtmp();
    if(filesize>0)
      (*file_on_disk).blockwrite(&((*content_in_ram)[0]), filesize);
    (*file_on_disk).setpos(filepos);
    forget_content_in_ram();
  }
public:
  FileTmp(): MAX_RAM_FOR_TMP_CONTENT( 16 * 1024 * 1024){content_in_ram=new Array<U8>(0); filepos=0; filesize=0; file_on_disk = 0;}
  ~FileTmp() {close();}
  bool open(const char *filename, bool must_succeed) { assert(false); return false; } //this method is forbidden for temporary files
  void create(const char *filename) { assert(false); } //this method is forbidden for temporary files
  void close() {
    forget_content_in_ram();
    forget_file_on_disk();
  }
  int getc() {
    if(content_in_ram)
    {
      if (filepos >= filesize)
        return EOF; 
      else {
        U8 c = (*content_in_ram)[(U32)filepos];
        filepos++; 
        return c; 
      }
    }
    else return (*file_on_disk).getc();
  }
  void putc(U8 c) {
    if(content_in_ram) {
      if (filepos < MAX_RAM_FOR_TMP_CONTENT) {
        if (filepos == filesize) { (*content_in_ram).push_back(c); filesize++; }
        else 
        (*content_in_ram)[(U32)filepos] = c;
        filepos++;
        //filesize++;
        return;
      }
      else ram_to_disk();
    }
    (*file_on_disk).putc(c);
  }
  U64 blockread(U8 *ptr, U64 count) {
    if(content_in_ram)
    {
      U64 available = filesize - filepos;
      if (available<count)count = available;
      if(count>0) memcpy(ptr, &((*content_in_ram)[(U32)filepos]), count);
      filepos += count;
      return count;
    }
    else return (*file_on_disk).blockread(ptr,count);
  }
  U64 blockwrite(U8 *ptr, U64 count) {
    if(content_in_ram) {
      if (filepos+count <= MAX_RAM_FOR_TMP_CONTENT) 
      { 
        (*content_in_ram).resize((U32)(filepos + count));
        if(count>0)memcpy(&((*content_in_ram)[(U32)filepos]), ptr, count);
        filesize += count;
        filepos += count;
        return count;
      }
      else ram_to_disk();
    }
    return (*file_on_disk).blockwrite(ptr,count);
  }
  void setpos(U64 newpos) { 
    if(content_in_ram) {
      if(newpos>filesize)ram_to_disk(); //panic: we don't support seeking past end of file - let's switch to disk
      else {filepos = newpos; return;}
    }  
     (*file_on_disk).setpos(newpos);
  }
  void setend() { 
    if(content_in_ram) filepos = filesize;
    else (*file_on_disk).setend();
  }
  U64 curpos() { 
    if(content_in_ram) return filepos;
    else return (*file_on_disk).curpos();
  }
  bool eof() { 
    if(content_in_ram)return filepos >= filesize;
    else return (*file_on_disk).eof();
  }
};

//////////////////////////// rnd ///////////////////////////////

// 32-bit pseudo random number generator
class Random{
  Array<U32> table;
  int i;
public:
  Random(): table(64) {
    table[0]=123456789;
    table[1]=987654321;
    for (int j=0; j<62; j++) table[j+2]=table[j+1]*11+table[j]*23/16;
    i=0;
  }
  U32 operator()() {
    return ++i, table[i&63]=table[(i-24)&63]^table[(i-55)&63];
  }
} ;


// Buffer for file segment info 
// type size info(if not -1)
class Segment {
  Array<U8> b;
public:
    U32 pos;  //size of buffer
    U64 hpos; //header pos points to segment info at archive end
    //int count; //count of segments
  Segment(int i=0): b(i),pos(0),hpos(0)/*,count(0)*/ {}
  void setsize(int i) {
    if (!i) return;
    assert(i>0);
    b.resize(i);
  }
  U8& operator[](U32 i) {
      if (i>=b.size()) setsize(i+1);
    return b[i];
  }
  U8 operator()(U32 i) const {
    assert(i>=0);
    assert(i<=b.size());
    return b[i];
  }
  
  // put 8 bytes to segment buffer
  void put8(U64 num){
    if ((pos+8)>=b.size()) setsize(pos+8);
    b[pos++]=(num>>56)&0xff;
    b[pos++]=(num>>48)&0xff;
    b[pos++]=(num>>40)&0xff;
    b[pos++]=(num>>32)&0xff;
    b[pos++]=(num>>24)&0xff;
    b[pos++]=(num>>16)&0xff;
    b[pos++]=(num>>8)&0xff;
    b[pos++]=num&0xff;  
  }
  void put4(U32 num){
    if ((pos+4)>=b.size()) setsize(pos+4);
    b[pos++]=(num>>24)&0xff;
    b[pos++]=(num>>16)&0xff;
    b[pos++]=(num>>8)&0xff;
    b[pos++]=num&0xff;  
  }
  void put1(U8 num){
    if (pos>=b.size()) setsize(pos+1);
    b[pos++]=num;
  }
  int size() const {
    return b.size();
  }
};

/////////////////////// Global context /////////////////////////
//bool modeQuick=false;
U8 level=DEFAULT_OPTION;  // Compression level 0 to 15
U64 MEM(){
     return 0x10000UL<<level;
}
int defaultType;
Segment segment; //for file segments type size info(if not -1)
 int streamCount;
File **filestreams;
const int datatypecount=39+1+1+1;

// Contain all global data usable between models
class BlockData {
public: 
    int y; // Last bit, 0 or 1, set by encoder
    int c0; // Last 0-7 bits of the partial byte with a leading 1 bit (1-255)
    U32 c4;//,c8; // Last 4,4 whole bytes, packed.  Last byte is bits 0-7.
    int bpos; // bits in c0 (0 to 7)
    int blpos; // Relative position in block
    int filetype;
    int finfo;
    struct Inputs{
        int ncount;     // mixer input count
        //int wcount;     // mixer weights count
        Array<short,32> n;      // input array
        void add(int p){ n[ncount++]=p; }
    } ;
    Array<Inputs> mxInputs; // array of inputs
    int cInputs;
BlockData():y(0), c0(1), c4(0),bpos(0),blpos(0),filetype(defaultType),finfo(-1),mxInputs(0),cInputs(-1) {
        // Set globals according to option
        assert(level<=9);
    }
~BlockData(){ }
};
///////////////////////////// ilog //////////////////////////////

// ilog(x) = round(log2(x) * 16), 0 <= x < 64K
class Ilog {
  Array<U8> t;
public:
  int operator()(U16 x) const {return t[x];}
  Ilog();
} ilog;

// Compute lookup table by numerical integration of 1/x
Ilog::Ilog(): t(65536) {
  U32 x=14155776;
  for (int i=2; i<65536; ++i) {
    x+=774541002/(i*2-1);  // numerator is 2^29/ln 2
    t[i]=x>>24;
  }
}

inline U32 BitCount(U32 v) {
  v -= ((v >> 1) & 0x55555555);
  v = ((v >> 2) & 0x33333333) + (v & 0x33333333);
  v = ((v >> 4) + v) & 0x0f0f0f0f;
  v = ((v >> 8) + v) & 0x00ff00ff;
  v = ((v >> 16) + v) & 0x0000ffff;
  return v;
}

// ilog2
// returns floor(log2(x)), e.g. 30->4  31->4  32->5,  33->5
#ifdef _MSC_VER
#include <intrin.h>
inline U32 ilog2(U32 x) {
  DWORD tmp=0;
  if(x!=0)_BitScanReverse(&tmp,x);
  return tmp;
}
#elif __GNUC__
inline U32 ilog2(U32 x) {
  if(x!=0)x=31-__builtin_clz(x);
  return x;
}
#else
inline U32 ilog2(U32 x) {
  //copy the leading "1" bit to its left (0x03000000 -> 0x03ffffff)
  x |= (x >> 1);
  x |= (x >> 2);
  x |= (x >> 4);
  x |= (x >> 8);
  x |= (x >>16);
  //how many trailing bits do we have (except the first)? 
  return BitCount(x >> 1);
}
#endif
///////////////////////// state table ////////////////////////

// State table:
//   nex(state, 0) = next state if bit y is 0, 0 <= state < 256
//   nex(state, 1) = next state if bit y is 1
//   nex(state, 2) = number of zeros in bit history represented by state
//   nex(state, 3) = number of ones represented
//
// States represent a bit history within some context.
// State 0 is the starting state (no bits seen).
// States 1-30 represent all possible sequences of 1-4 bits.
// States 31-252 represent a pair of counts, (n0,n1), the number
//   of 0 and 1 bits respectively.  If n0+n1 < 16 then there are
//   two states for each pair, depending on if a 0 or 1 was the last
//   bit seen.
// If n0 and n1 are too large, then there is no state to represent this
// pair, so another state with about the same ratio of n0/n1 is substituted.
// Also, when a bit is observed and the count of the opposite bit is large,
// then part of this count is discarded to favor newer data over old.

#if 1 // change to #if 0 to generate this table at run time (4% slower)
static const U8 State_table[256][4]={
  {  1,  2, 0, 0},{  3,  5, 1, 0},{  4,  6, 0, 1},{  7, 10, 2, 0}, // 0-3
  {  8, 12, 1, 1},{  9, 13, 1, 1},{ 11, 14, 0, 2},{ 15, 19, 3, 0}, // 4-7
  { 16, 23, 2, 1},{ 17, 24, 2, 1},{ 18, 25, 2, 1},{ 20, 27, 1, 2}, // 8-11
  { 21, 28, 1, 2},{ 22, 29, 1, 2},{ 26, 30, 0, 3},{ 31, 33, 4, 0}, // 12-15
  { 32, 35, 3, 1},{ 32, 35, 3, 1},{ 32, 35, 3, 1},{ 32, 35, 3, 1}, // 16-19
  { 34, 37, 2, 2},{ 34, 37, 2, 2},{ 34, 37, 2, 2},{ 34, 37, 2, 2}, // 20-23
  { 34, 37, 2, 2},{ 34, 37, 2, 2},{ 36, 39, 1, 3},{ 36, 39, 1, 3}, // 24-27
  { 36, 39, 1, 3},{ 36, 39, 1, 3},{ 38, 40, 0, 4},{ 41, 43, 5, 0}, // 28-31
  { 42, 45, 4, 1},{ 42, 45, 4, 1},{ 44, 47, 3, 2},{ 44, 47, 3, 2}, // 32-35
  { 46, 49, 2, 3},{ 46, 49, 2, 3},{ 48, 51, 1, 4},{ 48, 51, 1, 4}, // 36-39
  { 50, 52, 0, 5},{ 53, 43, 6, 0},{ 54, 57, 5, 1},{ 54, 57, 5, 1}, // 40-43
  { 56, 59, 4, 2},{ 56, 59, 4, 2},{ 58, 61, 3, 3},{ 58, 61, 3, 3}, // 44-47
  { 60, 63, 2, 4},{ 60, 63, 2, 4},{ 62, 65, 1, 5},{ 62, 65, 1, 5}, // 48-51
  { 50, 66, 0, 6},{ 67, 55, 7, 0},{ 68, 57, 6, 1},{ 68, 57, 6, 1}, // 52-55
  { 70, 73, 5, 2},{ 70, 73, 5, 2},{ 72, 75, 4, 3},{ 72, 75, 4, 3}, // 56-59
  { 74, 77, 3, 4},{ 74, 77, 3, 4},{ 76, 79, 2, 5},{ 76, 79, 2, 5}, // 60-63
  { 62, 81, 1, 6},{ 62, 81, 1, 6},{ 64, 82, 0, 7},{ 83, 69, 8, 0}, // 64-67
  { 84, 71, 7, 1},{ 84, 71, 7, 1},{ 86, 73, 6, 2},{ 86, 73, 6, 2}, // 68-71
  { 44, 59, 5, 3},{ 44, 59, 5, 3},{ 58, 61, 4, 4},{ 58, 61, 4, 4}, // 72-75
  { 60, 49, 3, 5},{ 60, 49, 3, 5},{ 76, 89, 2, 6},{ 76, 89, 2, 6}, // 76-79
  { 78, 91, 1, 7},{ 78, 91, 1, 7},{ 80, 92, 0, 8},{ 93, 69, 9, 0}, // 80-83
  { 94, 87, 8, 1},{ 94, 87, 8, 1},{ 96, 45, 7, 2},{ 96, 45, 7, 2}, // 84-87
  { 48, 99, 2, 7},{ 48, 99, 2, 7},{ 88,101, 1, 8},{ 88,101, 1, 8}, // 88-91
  { 80,102, 0, 9},{103, 69,10, 0},{104, 87, 9, 1},{104, 87, 9, 1}, // 92-95
  {106, 57, 8, 2},{106, 57, 8, 2},{ 62,109, 2, 8},{ 62,109, 2, 8}, // 96-99
  { 88,111, 1, 9},{ 88,111, 1, 9},{ 80,112, 0,10},{113, 85,11, 0}, // 100-103
  {114, 87,10, 1},{114, 87,10, 1},{116, 57, 9, 2},{116, 57, 9, 2}, // 104-107
  { 62,119, 2, 9},{ 62,119, 2, 9},{ 88,121, 1,10},{ 88,121, 1,10}, // 108-111
  { 90,122, 0,11},{123, 85,12, 0},{124, 97,11, 1},{124, 97,11, 1}, // 112-115
  {126, 57,10, 2},{126, 57,10, 2},{ 62,129, 2,10},{ 62,129, 2,10}, // 116-119
  { 98,131, 1,11},{ 98,131, 1,11},{ 90,132, 0,12},{133, 85,13, 0}, // 120-123
  {134, 97,12, 1},{134, 97,12, 1},{136, 57,11, 2},{136, 57,11, 2}, // 124-127
  { 62,139, 2,11},{ 62,139, 2,11},{ 98,141, 1,12},{ 98,141, 1,12}, // 128-131
  { 90,142, 0,13},{143, 95,14, 0},{144, 97,13, 1},{144, 97,13, 1}, // 132-135
  { 68, 57,12, 2},{ 68, 57,12, 2},{ 62, 81, 2,12},{ 62, 81, 2,12}, // 136-139
  { 98,147, 1,13},{ 98,147, 1,13},{100,148, 0,14},{149, 95,15, 0}, // 140-143
  {150,107,14, 1},{150,107,14, 1},{108,151, 1,14},{108,151, 1,14}, // 144-147
  {100,152, 0,15},{153, 95,16, 0},{154,107,15, 1},{108,155, 1,15}, // 148-151
  {100,156, 0,16},{157, 95,17, 0},{158,107,16, 1},{108,159, 1,16}, // 152-155
  {100,160, 0,17},{161,105,18, 0},{162,107,17, 1},{108,163, 1,17}, // 156-159
  {110,164, 0,18},{165,105,19, 0},{166,117,18, 1},{118,167, 1,18}, // 160-163
  {110,168, 0,19},{169,105,20, 0},{170,117,19, 1},{118,171, 1,19}, // 164-167
  {110,172, 0,20},{173,105,21, 0},{174,117,20, 1},{118,175, 1,20}, // 168-171
  {110,176, 0,21},{177,105,22, 0},{178,117,21, 1},{118,179, 1,21}, // 172-175
  {110,180, 0,22},{181,115,23, 0},{182,117,22, 1},{118,183, 1,22}, // 176-179
  {120,184, 0,23},{185,115,24, 0},{186,127,23, 1},{128,187, 1,23}, // 180-183
  {120,188, 0,24},{189,115,25, 0},{190,127,24, 1},{128,191, 1,24}, // 184-187
  {120,192, 0,25},{193,115,26, 0},{194,127,25, 1},{128,195, 1,25}, // 188-191
  {120,196, 0,26},{197,115,27, 0},{198,127,26, 1},{128,199, 1,26}, // 192-195
  {120,200, 0,27},{201,115,28, 0},{202,127,27, 1},{128,203, 1,27}, // 196-199
  {120,204, 0,28},{205,115,29, 0},{206,127,28, 1},{128,207, 1,28}, // 200-203
  {120,208, 0,29},{209,125,30, 0},{210,127,29, 1},{128,211, 1,29}, // 204-207
  {130,212, 0,30},{213,125,31, 0},{214,137,30, 1},{138,215, 1,30}, // 208-211
  {130,216, 0,31},{217,125,32, 0},{218,137,31, 1},{138,219, 1,31}, // 212-215
  {130,220, 0,32},{221,125,33, 0},{222,137,32, 1},{138,223, 1,32}, // 216-219
  {130,224, 0,33},{225,125,34, 0},{226,137,33, 1},{138,227, 1,33}, // 220-223
  {130,228, 0,34},{229,125,35, 0},{230,137,34, 1},{138,231, 1,34}, // 224-227
  {130,232, 0,35},{233,125,36, 0},{234,137,35, 1},{138,235, 1,35}, // 228-231
  {130,236, 0,36},{237,125,37, 0},{238,137,36, 1},{138,239, 1,36}, // 232-235
  {130,240, 0,37},{241,125,38, 0},{242,137,37, 1},{138,243, 1,37}, // 236-239
  {130,244, 0,38},{245,135,39, 0},{246,137,38, 1},{138,247, 1,38}, // 240-243
  {140,248, 0,39},{249,135,40, 0},{250, 69,39, 1},{ 80,251, 1,39}, // 244-247
  {140,252, 0,40},{249,135,41, 0},{250, 69,40, 1},{ 80,251, 1,40}, // 248-251
  {140,252, 0,41}};  // 252, 253-255 are reserved

#define nex(state,sel) State_table[state][sel]

// The code used to generate the above table at run time (4% slower).
// To print the table, uncomment the 4 lines of print statements below.
// In this code x,y = n0,n1 is the number of 0,1 bits represented by a state.
#else

class StateTable {
  Array<U8> ns;  // state*4 -> next state if 0, if 1, n0, n1
  enum {B=5, N=64}; // sizes of b, t
  static const int b[B];  // x -> max y, y -> max x
  static U8 t[N][N][2];  // x,y -> state number, number of states
  int num_states(int x, int y);  // compute t[x][y][1]
  void discount(int& x);  // set new value of x after 1 or y after 0
  void next_state(int& x, int& y, int b);  // new (x,y) after bit b
public:
  int operator()(int state, int sel) {return ns[state*4+sel];}
  StateTable();
} nex;

const int StateTable::b[B]={42,41,13,6,5};  // x -> max y, y -> max x
U8 StateTable::t[N][N][2];

int StateTable::num_states(int x, int y) {
  if (x<y) return num_states(y, x);
  if (x<0 || y<0 || x>=N || y>=N || y>=B || x>=b[y]) return 0;

  // States 0-30 are a history of the last 0-4 bits
  if (x+y<=4) {  // x+y choose x = (x+y)!/x!y!
    int r=1;
    for (int i=x+1; i<=x+y; ++i) r*=i;
    for (int i=2; i<=y; ++i) r/=i;
    return r;
  }

  // States 31-255 represent a 0,1 count and possibly the last bit
  // if the state is reachable by either a 0 or 1.
  else
    return 1+(y>0 && x+y<16);
}

// New value of count x if the opposite bit is observed
void StateTable::discount(int& x) {
  if (x>2) x=ilog(x)/6-1;
}

// compute next x,y (0 to N) given input b (0 or 1)
void StateTable::next_state(int& x, int& y, int b) {
  if (x<y)
    next_state(y, x, 1-b);
  else {
    if (b) {
      ++y;
      discount(x);
    }
    else {
      ++x;
      discount(y);
    }
    while (!t[x][y][1]) {
      if (y<2) --x;
      else {
        x=(x*(y-1)+(y/2))/y;
        --y;
      }
    }
  }
}

// Initialize next state table ns[state*4] -> next if 0, next if 1, x, y
StateTable::StateTable(): ns(1024) {

  // Assign states
  int state=0;
  for (int i=0; i<256; ++i) {
    for (int y=0; y<=i; ++y) {
      int x=i-y;
      int n=num_states(x, y);
      if (n) {
        t[x][y][0]=state;
        t[x][y][1]=n;
        state+=n;
      }
    }
  }

  // Print/generate next state table
  state=0;
  for (int i=0; i<N; ++i) {
    for (int y=0; y<=i; ++y) {
      int x=i-y;
      for (int k=0; k<t[x][y][1]; ++k) {
        int x0=x, y0=y, x1=x, y1=y;  // next x,y for input 0,1
        int ns0=0, ns1=0;
        if (state<15) {
          ++x0;
          ++y1;
          ns0=t[x0][y0][0]+state-t[x][y][0];
          ns1=t[x1][y1][0]+state-t[x][y][0];
          if (x>0) ns1+=t[x-1][y+1][1];
          ns[state*4]=ns0;
          ns[state*4+1]=ns1;
          ns[state*4+2]=x;
          ns[state*4+3]=y;
        }
        else if (t[x][y][1]) {
          next_state(x0, y0, 0);
          next_state(x1, y1, 1);
          ns[state*4]=ns0=t[x0][y0][0];
          ns[state*4+1]=ns1=t[x1][y1][0]+(t[x1][y1][1]>1);
          ns[state*4+2]=x;
          ns[state*4+3]=y;
        }
          // uncomment to print table above
//        printf("{%3d,%3d,%2d,%2d},", ns[state*4], ns[state*4+1],
//          ns[state*4+2], ns[state*4+3]);
//        if (state%4==3) printf(" // %d-%d\n  ", state-3, state);
        assert(state>=0 && state<256);
        assert(t[x][y][1]>0);
        assert(t[x][y][0]<=state);
        assert(t[x][y][0]+t[x][y][1]>state);
        assert(t[x][y][1]<=6);
        assert(t[x0][y0][1]>0);
        assert(t[x1][y1][1]>0);
        assert(ns0-t[x0][y0][0]<t[x0][y0][1]);
        assert(ns0-t[x0][y0][0]>=0);
        assert(ns1-t[x1][y1][0]<t[x1][y1][1]);
        assert(ns1-t[x1][y1][0]>=0);
        ++state;
      }
    }
  }
//  printf("%d states\n", state); exit(0);  // uncomment to print table above
}

#endif

///////////////////////////// Squash //////////////////////////////
// return p = 1/(1 + exp(-d)), d scaled by 8 bits, p scaled by 12 bits
class Squash {
  Array<U16> t;
public:
  Squash();
  int operator()(int p) const {
   if (p<-2047) return  0; 
   if (p>2047) return  4095;
   return t[p+2048];
  }
} squash;

Squash::Squash(): t(4096) {
int ts[33]={1,2,3,6,10,16,27,45,73,120,194,310,488,747,1101,
    1546,2047,2549,2994,3348,3607,3785,3901,3975,4022,
    4050,4068,4079,4085,4089,4092,4093,4094};
  int w,d;
  for (int i=-2047; i<=2047; ++i){
    w=i&127;
  d=(i>>7)+16;
  t[i+2048]=(ts[d]*(128-w)+ts[(d+1)]*w+64) >> 7;
    }
}
//////////////////////////// Stretch ///////////////////////////////

// Inverse of squash. d = ln(p/(1-p)), d scaled by 8 bits, p by 12 bits.
// d has range -2047 to 2047 representing -8 to 8.  p has range 0 to 4095.

class Stretch {
  Array<short> t;
public:
  Stretch();
  int operator()(int p) const {
    assert(p>=0 && p<4096);
    return t[p];
  }
} stretch;

Stretch::Stretch(): t(4096) {
  int pi=0;
  for (int x=-2047; x<=2047; ++x) {  // invert squash()
    int i=squash(x);
    for (int j=pi; j<=i; ++j)
      t[j]=x;
    pi=i+1;
  }
  t[4095]=2047;
}

//////////////////////////// Mixer /////////////////////////////

// Mixer m(N, M, S=1, w=0) combines models using M neural networks with
//   N inputs each, of which up to S may be selected.  If S > 1 then
//   the outputs of these neural networks are combined using another
//   neural network (with parameters S, 1, 1).  If S = 1 then the
//   output is direct.  The weights are initially w (+-32K).
//   It is used as follows:
// m.update() trains the network where the expected output is the
//   last bit (in the global variable y).
// m.add(stretch(p)) inputs prediction from one of N models.  The
//   prediction should be positive to predict a 1 bit, negative for 0,
//   nominally +-256 to +-2K.  The maximum allowed value is +-32K but
//   using such large values may cause overflow if N is large.
// m.set(cxt, range) selects cxt as one of 'range' neural networks to
//   use.  0 <= cxt < range.  Should be called up to S times such
//   that the total of the ranges is <= M.
// m.p() returns the output prediction that the next bit is 1 as a
//   12 bit number (0 to 4095).

#if !defined(__GNUC__)

#if (2 == _M_IX86_FP) // 2 if /arch:SSE2 was used.
# define __SSE2__
#elif (1 == _M_IX86_FP) // 1 if /arch:SSE was used.
# define __SSE__
#endif

#endif /* __GNUC__ */

#if defined(__AVX2__)
#include <immintrin.h>
#define OPTIMIZE "AVX2-"
#elif defined(__SSE4_1__)   
#include<smmintrin.h>
#elif   defined(__SSSE3__)
#include<tmmintrin.h>
#elif defined(__SSE2__) 
#include <emmintrin.h>
#define OPTIMIZE "SSE2-"

#elif defined(__SSE__)
#include <xmmintrin.h>
#define OPTIMIZE "SSE-"
#endif
/**
 * Vector product a*b of n signed words, returning signed integer scaled down by 8 bits.
 * n is rounded up to a multiple of 8.
 */
//static int dot_product (const short* const t, const short* const w, int n);

/**
 * Train n neural network weights w[n] on inputs t[n] and err.
 * w[i] += ((t[i]*2*err)+(1<<16))>>17 bounded to +- 32K.
 * n is rounded up to a multiple of 8.
 */
//static void train (const short* const t, short* const w, int n, const int e);
struct ErrorInfo {
  U32 Data[2], Sum, Mask, Collected;
};

inline U32 SQR(U32 x) {
  return x*x;
}
#if defined(__MMX__)
typedef __m128i XMM;
#endif
#if defined(__AVX2__)
typedef __m256i YMM;
#endif
#define DEFAULT_LEARNING_RATE 7
//////////////////////////// APM1 //////////////////////////////

// APM1 maps a probability and a context into a new probability
// that bit y will next be 1.  After each guess it updates
// its state to improve future guesses.  Methods:
//
// APM1 a(N) creates with N contexts, uses 66*N bytes memory.
// a.p(pr, cx, rate=7) returned adjusted probability in context cx (0 to
//   N-1).  rate determines the learning rate (smaller = faster, default 7).
//   Probabilities are scaled 12 bits (0-4095).

class APM1 {
  int index;     // last p, context
  const int N;   // number of contexts
  Array<U16> t;  // [N][33]:  p, context -> p
  BlockData& x;
  int mask;
  int rate,cxt;
  int p1;
public:
  APM1(int n,int r,int d,BlockData& x);
  void set(int c){cxt=c;
  }
  inline int i1() {return p1;
  }
  int p(int pr=2048) {
    assert(pr>=0 && pr<4096 && rate>0 && rate<32);
    
    pr=stretch(pr);
    int g=(x.y<<16)+(x.y<<rate)-x.y-x.y;
    t[index] += (g-t[index]) >> rate;
    t[index+1] += (g-t[index+1]) >> rate;
    const int w=pr&127;  // interpolation weight (33 points)
    index=((pr+2048)>>7)+(cxt&mask)*33;
    return (t[index]*(128-w)+t[index+1]*w) >> 11;
  }
};

// maps p, cxt -> p initially
APM1::APM1(int n,int r,int d,BlockData& bd): index(0), N(n), t(n*33),x(bd),mask(n-1),rate(r),cxt(0),p1(d) {
    assert(ispowerof2(n));
  for (int i=0; i<N; ++i)
    for (int j=0; j<33; ++j)
      t[i*33+j] = i==0 ? squash((j-16)*128)*16 : t[j];
}

class Mixer {
private: 
   int N, M;   // max inputs, max contexts, max context sets
   short*tx; // N inputs from add()  
  Array<short, 32> wx; // N*M weights
  int cxt;  // S contexts
  //int ncxt;        // number of contexts (0 to S)
  //int base;        // offset of next context
  //int nx;          // Number of inputs in tx, 0 to N  
  int pr;   // last result (scaled 12 bits)
 // ErrorInfo info; 
 // int rates; // learning rates
 
  int shift1; 
public:    
  BlockData& x;
  Mixer(int m,BlockData& bd,int s);
  
#if defined(__AVX2__)
 int dot_product (const short* const t, const short* const w, int n) {
  assert(n == ((n + 15) & -16));
  __m256i sum = _mm256_setzero_si256 ();
  while ((n -= 16) >= 0) { // Each loop sums 16 products
    __m256i tmp = _mm256_madd_epi16 (*(__m256i *) &t[n], *(__m256i *) &w[n]); // t[n] * w[n] + t[n+1] * w[n+1]
    tmp = _mm256_srai_epi32 (tmp, 8); //                                        (t[n] * w[n] + t[n+1] * w[n+1]) >> 8
    sum = _mm256_add_epi32 (sum, tmp); //                                sum += (t[n] * w[n] + t[n+1] * w[n+1]) >> 8
  } 
   sum =_mm256_hadd_epi32(sum,_mm256_setzero_si256 ());       //add [1]=[1]+[2], [2]=[3]+[4], [3]=0, [4]=0, [5]=[5]+[6], [6]=[7]+[8], [7]=0, [8]=0
   sum =_mm256_hadd_epi32(sum,_mm256_setzero_si256 ());       //add [1]=[1]+[2], [2]=0,       [3]=0, [4]=0, [5]=[5]+[6], [6]=0,       [7]=0, [8]=0
   __m128i lo = _mm256_extractf128_si256(sum, 0);
   __m128i hi = _mm256_extractf128_si256(sum, 1);
   __m128i newsum = _mm_add_epi32(lo, hi);                    //sum last two
   return _mm_cvtsi128_si32(newsum);
}

 void train (const short* const t, short* const w, int n, const int e) {
  assert(n == ((n + 15) & -16));
  if (e) {
    const __m256i one = _mm256_set1_epi16 (1);
    const __m256i err = _mm256_set1_epi16 (short(e));
    while ((n -= 16) >= 0) { // Each iteration adjusts 16 weights
      __m256i tmp = _mm256_adds_epi16 (*(__m256i *) &t[n], *(__m256i *) &t[n]); // t[n] * 2
      tmp = _mm256_mulhi_epi16 (tmp, err); //                                     (t[n] * 2 * err) >> 16
      tmp = _mm256_adds_epi16 (tmp, one); //                                     ((t[n] * 2 * err) >> 16) + 1
      tmp = _mm256_srai_epi16 (tmp, 1); //                                      (((t[n] * 2 * err) >> 16) + 1) >> 1
      tmp = _mm256_adds_epi16 (tmp, *(__m256i *) &w[n]); //                    ((((t[n] * 2 * err) >> 16) + 1) >> 1) + w[n]
      *(__m256i *) &w[n] = tmp; //                                          save the new eight weights, bounded to +- 32K
    }
  }
}

#elif defined(__SSE2__) || defined(__SSSE3__)
 int dot_product (const short* const t, const short* const w, int n) {
  assert(n == ((n + 15) & -16));
  XMM sum = _mm_setzero_si128 ();
  while ((n -= 8) >= 0) { // Each loop sums eight products
    XMM tmp = _mm_madd_epi16 (*(XMM *) &t[n], *(XMM *) &w[n]); // t[n] * w[n] + t[n+1] * w[n+1]
    tmp = _mm_srai_epi32 (tmp, 8); //                                        (t[n] * w[n] + t[n+1] * w[n+1]) >> 8
    sum = _mm_add_epi32 (sum, tmp); //                                sum += (t[n] * w[n] + t[n+1] * w[n+1]) >> 8
  }
  #if  defined(__SSSE3__)
  sum=_mm_hadd_epi32 (sum,sum);
  sum=_mm_hadd_epi32 (sum,sum);
 #else
  sum = _mm_add_epi32(sum, _mm_srli_si128 (sum, 8));
  sum = _mm_add_epi32(sum, _mm_srli_si128 (sum, 4));
  #endif

  return _mm_cvtsi128_si32 (sum); //                     ...  and scale back to integer
}

 void train (const short* const t, short* const w, int n, const int e) {
  assert(n == ((n + 15) & -16));
  if (e) {
    const XMM one = _mm_set1_epi16 (1);
    const XMM err = _mm_set1_epi16 (short(e));
    while ((n -= 8) >= 0) { // Each iteration adjusts eight weights
      XMM tmp = _mm_adds_epi16 (*(XMM *) &t[n], *(XMM *) &t[n]); // t[n] * 2
      tmp = _mm_mulhi_epi16 (tmp, err); //                                     (t[n] * 2 * err) >> 16
      tmp = _mm_adds_epi16 (tmp, one); //                                     ((t[n] * 2 * err) >> 16) + 1
      tmp = _mm_srai_epi16 (tmp, 1); //                                      (((t[n] * 2 * err) >> 16) + 1) >> 1
      tmp = _mm_adds_epi16 (tmp, *(XMM *) &w[n]); //                    ((((t[n] * 2 * err) >> 16) + 1) >> 1) + w[n]
      *(XMM *) &w[n] = tmp; //                                          save the new eight weights, bounded to +- 32K
    }
  }
}

#elif defined(__SSE__)
 int dot_product (const short* const t, const short* const w, int n) {
  assert(n == ((n + 15) & -16));
  __m64 sum = _mm_setzero_si64 ();
  while ((n -= 8) >= 0) { // Each loop sums eight products
    __m64 tmp = _mm_madd_pi16 (*(__m64 *) &t[n], *(__m64 *) &w[n]); //   t[n] * w[n] + t[n+1] * w[n+1]
    tmp = _mm_srai_pi32 (tmp, 8); //                                    (t[n] * w[n] + t[n+1] * w[n+1]) >> 8
    sum = _mm_add_pi32 (sum, tmp); //                            sum += (t[n] * w[n] + t[n+1] * w[n+1]) >> 8

    tmp = _mm_madd_pi16 (*(__m64 *) &t[n + 4], *(__m64 *) &w[n + 4]); // t[n+4] * w[n+4] + t[n+5] * w[n+5]
    tmp = _mm_srai_pi32 (tmp, 8); //                                    (t[n+4] * w[n+4] + t[n+5] * w[n+5]) >> 8
    sum = _mm_add_pi32 (sum, tmp); //                            sum += (t[n+4] * w[n+4] + t[n+5] * w[n+5]) >> 8
  }
  sum = _mm_add_pi32 (sum, _mm_srli_si64 (sum, 32)); // Add eight sums together ...
  const int retval = _mm_cvtsi64_si32 (sum); //                     ...  and scale back to integer
  _mm_empty(); // Empty the multimedia state
  return retval;
}

 void train (const short* const t, short* const w, int n, const int e) {
  assert(n == ((n + 15) & -16));
  if (e) {
    const __m64 one = _mm_set1_pi16 (1);
    const __m64 err = _mm_set1_pi16 (short(e));
    while ((n -= 8) >= 0) { // Each iteration adjusts eight weights
      __m64 tmp = _mm_adds_pi16 (*(__m64 *) &t[n], *(__m64 *) &t[n]); //   t[n] * 2
      tmp = _mm_mulhi_pi16 (tmp, err); //                                 (t[n] * 2 * err) >> 16
      tmp = _mm_adds_pi16 (tmp, one); //                                 ((t[n] * 2 * err) >> 16) + 1
      tmp = _mm_srai_pi16 (tmp, 1); //                                  (((t[n] * 2 * err) >> 16) + 1) >> 1
      tmp = _mm_adds_pi16 (tmp, *(__m64 *) &w[n]); //                  ((((t[n] * 2 * err) >> 16) + 1) >> 1) + w[n]
      *(__m64 *) &w[n] = tmp; //                                       save the new four weights, bounded to +- 32K

      tmp = _mm_adds_pi16 (*(__m64 *) &t[n + 4], *(__m64 *) &t[n + 4]); // t[n+4] * 2
      tmp = _mm_mulhi_pi16 (tmp, err); //                                 (t[n+4] * 2 * err) >> 16
      tmp = _mm_adds_pi16 (tmp, one); //                                 ((t[n+4] * 2 * err) >> 16) + 1
      tmp = _mm_srai_pi16 (tmp, 1); //                                  (((t[n+4] * 2 * err) >> 16) + 1) >> 1
      tmp = _mm_adds_pi16 (tmp, *(__m64 *) &w[n + 4]); //              ((((t[n+4] * 2 * err) >> 16) + 1) >> 1) + w[n]
      *(__m64 *) &w[n + 4] = tmp; //                                   save the new four weights, bounded to +- 32K
    }
    _mm_empty(); // Empty the multimedia state
  }
}
#else

// dot_product returns dot product t*w of n elements.  n is rounded
// up to a multiple of 8.  Result is scaled down by 8 bits.
int dot_product(short *t, short *w, int n) {
  int sum=0;
  n=(n+15)&-16;
  for (int i=0; i<n; i+=2)
    sum+=(t[i]*w[i]+t[i+1]*w[i+1]) >> 8;
  return sum;
}

// Train neural network weights w[n] given inputs t[n] and err.
// w[i] += t[i]*err, i=0..n-1.  t, w, err are signed 16 bits (+- 32K).
// err is scaled 16 bits (representing +- 1/2).  w[i] is clamped to +- 32K
// and rounded.  n is rounded up to a multiple of 8.

void train(short *t, short *w, int n, int err) {
  n=(n+15)&-16;
  for (int i=0; i<n; ++i) {
    int wt=w[i]+(((t[i]*err*2>>16)+1)>>1);
    if (wt<-32768) wt=-32768;
    if (wt>32767) wt=32767;
    w[i]=wt;
  }
}
#endif 

  // Adjust weights to minimize coding cost of last prediction
  void update(int m) {
      int err=((x.y<<12)-pr)*7;
      assert(err>=-32768 && err<32768);
      train(&tx[0], &wx[cxt*N], N, err);
  }
 /*  void update1(int m) {
    int target=x.y<<12;
    if(nx>0)
    for (int i=0; i<ncxt; ++i) {
      int err=target-pr[i];
      train(&tx[0], &wx[cxt[i]*N], nx, err*rates[i]);

        U32 logErr=min(0xF,ilog2(abs(err)));
        info[i].Sum-=SQR(info[i].Data[1]>>28);
        info[i].Data[1]<<=4; info[i].Data[1]|=info[i].Data[0]>>28;
        info[i].Data[0]<<=4; info[i].Data[0]|=logErr;
        info[i].Sum+=SQR(logErr);
        info[i].Collected+=info[i].Collected<4096;
        info[i].Mask<<=1; info[i].Mask|=(logErr<=((info[i].Data[0]>>4)&0xF));
        U32 count=BitCount(info[i].Mask);
        if (info[i].Collected>=64 && (info[i].Sum>1500+U32(rates[i])*64 || count<9 || (info[i].Mask&0xFF)==0)){
          rates[i]=DEFAULT_LEARNING_RATE;
          memset(&info[i], 0, sizeof(ErrorInfo));
        }
        else if (info[i].Collected==4096 && info[i].Sum>=56 && info[i].Sum<=144 && count>28-U32(rates[i]) && ((info[i].Mask&0xFF)==0xFF)){
          rates[i]-=rates[i]>2;
          memset(&info[i], 0, sizeof(ErrorInfo));
        }
    }
  }*/
 
  /*
    #if defined(__MMX__)
  void addXMM(XMM a){
    assert(nx+8<N);
    _mm_storeu_si128 ((XMM *) &tx[nx],a);
    nx=nx+8;
  }
  #endif
  #if defined(__AVX2__)
   void addYMM(YMM a){
    assert(nx+16<N);
    _mm256_storeu_si256 ((YMM *) &tx[nx],a);
    nx=nx+16;
  }
  #endif*/
  // Set a context (call S times, sum of ranges <= M)
  void set(int cx) {
    assert(cx>=0);
    assert(cx<M);
    cxt=cx;
  }
  // predict next bit
  int p( int m) {
        assert(cxt<M);
    return pr=squash(dot_product(&tx[0], &wx[cxt*N], N)>>( shift1));
  }
  void setTxWx(int n,short* mn){
      N=(n+15)&-16;
      wx.resize(N*M);
      tx=mn; 
      for (int i=0; i<N*M; ++i)
       wx[i]=0;
  }
  ~Mixer();
};

Mixer::~Mixer() {

}

Mixer::Mixer(int m, BlockData& bd, int s):
     M(m), wx(0) , cxt(0), shift1(s),x(bd){
  assert( M>0);

    pr=2048; //initial p=0.5
  //  rates = DEFAULT_LEARNING_RATE;
  //  memset(&info, 0, sizeof(ErrorInfo));
 
}

//////////////////////////// StateMap, APM //////////////////////////

// A StateMap maps a context to a probability.  Methods:
//
// Statemap sm(n) creates a StateMap with n contexts using 4*n bytes memory.
// sm.p(y, cx, limit) converts state cx (0..n-1) to a probability (0..4095).
//     that the next y=1, updating the previous prediction with y (0..1).
//     limit (1..1023, default 1023) is the maximum count for computing a
//     prediction.  Larger values are better for stationary sources.

static int dt[1024];  // i -> 16K/(i+i+3)

class StateMap {
protected:
  const int N;  // Number of contexts
  int cxt;      // Context of last prediction
  Array<U32> t;       // cxt -> prediction in high 22 bits, count in low 10 bits
  inline void update(int y, int limit) {
    assert(cxt>=0 && cxt<N);
    assert(y==0 || y==1);
    U32 *p=&t[cxt], p0=p[0];
    int n=p0&1023, pr=p0>>10;  // count, prediction
    //if (n<limit) ++p0;
    //else p0=(p0&0xfffffc00)|limit;
    p0+=(n<limit);
    p0+=(((y<<22)-pr)>>3)*dt[n]&0xfffffc00;
    p[0]=p0;
  }

public:
  StateMap(int n=256);
  void Reset(int Rate=0){
    for (int i=0; i<N; ++i)
      t[i]=(t[i]&0xfffffc00)|min(Rate, t[i]&0x3FF);
  }
  // update bit y (0..1), predict next bit in context cx
  int p(int cx, int y,int limit=1023) {
    assert(cx>=0 && cx<N);
    assert(limit>0 && limit<1024);
    assert(y==0 || y==1);
    update(y,limit);
    return t[cxt=cx]>>20;
  }
};

StateMap::StateMap(int n): N(n), cxt(0), t(n) {
  for (int i=0; i<N; ++i)
    t[i]=1<<31;
}

 
class StateMapContext {
protected:
  const int N;  // Number of contexts
  int cxt;      // Context of last prediction
  Array<U32> t;       // cxt -> prediction in high 22 bits, count in low 10 bits
  int pr;
  int mask;
  int limit;
  BlockData& x;
  int cx;
  inline void update() {
    assert(cxt>=0 && cxt<N);
    assert(x.y==0 || x.y==1);
    U32 *p=&t[cxt], p0=p[0];
    int n=p0&1023, pr1=p0>>10;  // count, prediction
    //if (n<limit) ++p0;
    //else p0=(p0&0xfffffc00)|limit;
    p0+=(n<limit);
    p0+=(((x.y<<22)-pr1)>>3)*dt[n]&0xfffffc00;
    p[0]=p0;
}
public:
  StateMapContext(int n, int lim, BlockData& x);//
   void set(int c) {   
    cx=c;
    update();
    pr=t[cxt=(cx&mask)]>>20;
  }
  // update bit y (0..1), predict next bit in context cx
  inline int p() {
    
    //update();
    return pr;
  }
  void mix(int m) {   
    x.mxInputs[m].add(stretch(p()));
   // m.add();
  }
};

StateMapContext::StateMapContext(int n,int lim, BlockData& bd): N(n), cxt(0), t(n),pr(2048), mask(n-1),limit(lim), x(bd),cx(0) {
    assert(ispowerof2(n));
    assert(limit>0 && limit<1024);
  for (int i=0; i<N; ++i)
    t[i]=1<<31; 
} 

//////////////////////////// hash //////////////////////////////

// Hash 2-5 ints.
inline U32 hash0(U32 a, U32 b, U32 c=0xffffffff, U32 d=0xffffffff,
    U32 e=0xffffffff) {
  U32 h=a*200002981u+b*30005491u+c*50004239u+d*70004807u+e*110002499u;
  return h^h>>9^a>>2^b>>3^c>>4^d>>5^e>>6;
}
inline U32 hash1(U32 a, U32 b) {
  return (a  + 512) * 773+ b;
}

///////////////////////////// BH ////////////////////////////////

// A BH maps a 32 bit hash to an array of B bytes (checksum and B-2 values)
//
// BH bh(N); creates N element table with B bytes each.
//   N must be a power of 2.  The first byte of each element is
//   reserved for a checksum to detect collisions.  The remaining
//   B-1 bytes are values, prioritized by the first value.  This
//   byte is 0 to mark an unused element.
//
// bh[i] returns a pointer to the i'th element, such that
//   bh[i][0] is a checksum of i, bh[i][1] is the priority, and
//   bh[i][2..B-1] are other values (0-255).
//   The low lg(n) bits as an index into the table.
//   If a collision is detected, up to M nearby locations in the same
//   cache line are tested and the first matching checksum or
//   empty element is returned.
//   If no match or empty element is found, then the lowest priority
//   element is replaced.

// 2 byte checksum with LRU replacement (except last 2 by priority)
template <U32 B> class BH {
  enum {M=8};  // search limit
  Array<U8, 64> t; // elements
  //Array<U8> tmp;
  U8 tmp[B];
  U32 n; // size-1
public:
  BH(U32 i): t(i*B), n(i-1) {
    //printf("BH %0.0f, i %d B %d power %d\n",(i*B)+0.0,i,B,(i&(i-1))==0);
    assert(B>=2 && i>0 && (i&(i-1))==0); // size a power of 2?
    
  }
  U8* operator[](U32 i);
};

template <U32 B>
inline  U8* BH<B>::operator[](U32 i) {
  U16 chk=(i>>16^i)&0xffff;
  i=i*M&n;
  U8 *p;
  U16 *cp;
  int j;
  for (j=0; j<M; ++j) {
    p=&t[(i+j)*B];
    cp=(U16*)p;
    if (p[2]==0) {*cp=chk;break;}
    if (*cp==chk) break;  // found
  }
  if (j==0) return p+1;  // front
  //static U8 tmp[B];  // element to move to front
  if (j==M) {
    --j;
    memset(&tmp, 0, B);
    memmove(&tmp, &chk, 2);
    if (M>2 && t[(i+j)*B+2]>t[(i+j-1)*B+2]) --j;
  }
  else memcpy(&tmp, cp, B);
  memmove(&t[(i+1)*B], &t[i*B], j*B);
  memcpy(&t[i*B], &tmp, B);
  return &t[i*B+1];
}
 
/////////////////////////// ContextMap /////////////////////////
//
// A ContextMap maps contexts to a bit histories and makes predictions
// to a Mixer.  Methods common to all classes:
//
// ContextMap cm(M, C); creates using about M bytes of memory (a power
//   of 2) for C contexts.
// cm.set(cx);  sets the next context to cx, called up to C times
//   cx is an arbitrary 32 bit value that identifies the context.
//   It should be called before predicting the first bit of each byte.
// cm.mix(m) updates Mixer m with the next prediction.  Returns 1
//   if context cx is found, else 0.  Then it extends all the contexts with
//   global bit y.  It should be called for every bit:
//
//     if (bpos==0)
//       for (int i=0; i<C; ++i) cm.set(cxt[i]);
//     cm.mix(m);
//
// The different types are as follows:
//
// - RunContextMap.  The bit history is a count of 0-255 consecutive
//     zeros or ones.  Uses 4 bytes per whole byte context.  C=1.
//     The context should be a hash.
// - SmallStationaryContextMap.  0 <= cx < M/512.
//     The state is a 16-bit probability that is adjusted after each
//     prediction.  C=1.
// - ContextMap.  For large contexts, C >= 1.  Context need not be hashed.



// A RunContextMap maps a context into the next byte and a repeat
// count up to M.  Size should be a power of 2.  Memory usage is 3M/4.
class RunContextMap {
  BH<4> t;
  U8* cp;
  BlockData& x;
public:
  RunContextMap(int m,BlockData& bd): t(m/4),x(bd) {cp=t[0]+1;}
  void set(U32 cx) {  // update count
    if (cp[0]==0 || cp[1]!=(U8)x.c4) cp[0]=1, cp[1]=(U8)x.c4;
    else if (cp[0]<255) ++cp[0];
    cp=t[cx]+1;
  }
  int p() {  // predict next bit
    if ((cp[1]+256)>>(8-x.bpos)==x.c0)
      return ((cp[1]>>(7-x.bpos)&1)*2-1)*ilog(cp[0]+1)*8;
    else
      return 0;
  }
  int mix(int m) {  // return run length
    x.mxInputs[m].add(p());
    return cp[0]!=0;
  }
};

/*
Map for modelling contexts of (nearly-)stationary data.
The context is looked up directly. For each bit modelled, a 16bit prediction is stored.
The adaptation rate is controlled by the caller, see mix().

- BitsOfContext: How many bits to use for each context. Higher bits are discarded.
- InputBits: How many bits [1..8] of input are to be modelled for each context.
New contexts must be set at those intervals.

Uses (2^(BitsOfContext+1))*((2^InputBits)-1) bytes of memory.
*/

class SmallStationaryContextMap {
  Array<U16> Data;
  int Context, Mask, Stride, bCount, bTotal, B;
  U16 *cp;
  BlockData& x;
public:
  SmallStationaryContextMap(int BitsOfContext, BlockData& bd, int InputBits = 8) : 
  Data((1ull<<BitsOfContext)*((1ull<<InputBits)-1)), 
  Context(0), Mask((1<<BitsOfContext)-1), 
  Stride((1<<InputBits)-1), bCount(0), bTotal(InputBits), B(0)  ,x(bd)
  {
   // assert(BitsOfContext<=16);
    assert(InputBits>0 && InputBits<=8);
    Reset();
    cp=&Data[0];
  }
  void set(U32 ctx) {
    Context = (ctx&Mask)*Stride;
    bCount=B=0;
  }
  void Reset() {
    for (U32 i=0; i<Data.size(); ++i)
      Data[i]=0x7FFF;
  }
  void mix(int m, const int rate = 7, const int Multiplier = 1, const int Divisor = 4) {
    *cp+=((x.y<<16)-(*cp)+(1<<(rate-1)))>>rate;
    B+=(x.y && B>0);
    cp = &Data[Context+B];
    int Prediction = (*cp)>>4;
    x.mxInputs[m].add((stretch(Prediction)*Multiplier)/Divisor);
    x.mxInputs[m].add(((Prediction-2048)*Multiplier)/(Divisor*2));
    bCount++; B+=B+1;
    if (bCount==bTotal)
      bCount=B=0;
  }
};

/*
  Map for modelling contexts of (nearly-)stationary data.
  The context is looked up directly. For each bit modelled, a 32bit element stores
  a 22 bit prediction and a 10 bit adaptation rate offset.

  - BitsOfContext: How many bits to use for each context. Higher bits are discarded.
  - InputBits: How many bits [1..8] of input are to be modelled for each context.
    New contexts must be set at those intervals.
  - Rate: Initial adaptation rate offset [0..1023]. Lower offsets mean faster adaptation.
    Will be increased on every occurrence until the higher bound is reached.

    Uses (2^(BitsOfContext+2))*((2^InputBits)-1) bytes of memory.
*/

class StationaryMap {
  Array<U32> Data;
  int Context, Mask, Stride, bCount, bTotal, B;
  U32 *cp;
  BlockData& x;
public:
  StationaryMap(int BitsOfContext,BlockData& bd, int InputBits = 8, int Rate = 0): 
  
  Data((1ull<<BitsOfContext)*((1ull<<InputBits)-1)), Context(0), 
  Mask((1<<BitsOfContext)-1), Stride((1<<InputBits)-1), bCount(0), bTotal(InputBits), B(0),x(bd) {
    assert(InputBits>0 && InputBits<=8);
    assert(BitsOfContext+InputBits<=24);
    Reset(Rate);
    cp=&Data[0];
  }
  void set(U32 ctx) {
    Context = (ctx&Mask)*Stride;
    bCount=B=0;
  }
  void Reset( int Rate = 0 ){
    for (U32 i=0; i<Data.size(); ++i)
      Data[i]=(0x7FF<<20)|min(1023,Rate);
  }
  void mix(int m, const int Multiplier = 1, const int Divisor = 4, const U16 Limit = 1023) {
    // update
    U32 Count = min(min(Limit,0x3FF), ((*cp)&0x3FF)+1);
    int Prediction = (*cp)>>10, Error = (x.y<<22)-Prediction;
    Error = ((Error/8)*dt[Count])/1024;
    Prediction = min(0x3FFFFF,max(0,Prediction+Error));
    *cp = (Prediction<<10)|Count;
    // predict
    B+=(x.y && B>0);
    cp=&Data[Context+B];
    Prediction = (*cp)>>20;
    x.mxInputs[m].add((stretch(Prediction)*Multiplier)/Divisor);
    x.mxInputs[m].add(((Prediction-2048)*Multiplier)/(Divisor*2));
    bCount++; B+=B+1;
    if (bCount==bTotal)
      bCount=B=0;
  }
};
class MixMap {
int pr,p1;
  BlockData& x;
public:
  MixMap(int m,BlockData& bd): pr(2048),p1(m),x(bd) {   
  }
  inline int i1(){ return p1;  } //index 1
  void set(U32 cx) { pr=cx;       }
  inline int p() {   return pr;  }
  int mix(int m) {
    x.mxInputs[m].add(stretch(pr));
    return 0;
  }
};

class StaticMap {
int pr;
int pr1;
  BlockData& x;
public:
  StaticMap(int m,BlockData& bd): pr((m-128)*16),pr1(m*16),x(bd) {
  if (m>255 || m<0) printf("StaticMap must be 0..255\n"),quit();
  }
  void set(U32 cx) {
   //   if (cx>4096) cx=4096;
  //pr1=cx;  
  //pr=stretch(cx);    
  }
  inline int p() {
      return pr1;
  }
  int mix(int m) {
    x.mxInputs[m].add(pr);
    return 0;
  }
};

class AvgMap {
int pr;
int p1,p2; // index to prediction array
  BlockData& x;

public:
  AvgMap(int a,int b,BlockData& bd): pr(0),p1(a),p2(b),x(bd) {
  }
  inline int average(int a,int b){
      return pr=(a+b+1)>>1;
  }
  inline int i1(){ return p1;  } //index 1
  inline int i2(){ return p2;  } //index 2
  inline int p() { return pr;  }
  int mix(int m) {
    x.mxInputs[m].add(stretch(pr));
    return 0;
  }
};

class DynamicSMap {
int state;
StateMap *sm;
Array<int> cxt;
U32 mask;
Array<int>  pr;
int limit;
Array<U8> CxtState;
  BlockData& x;
  int index;
  int count;
public:
  DynamicSMap(int m,int lim,int c,BlockData& bd): state(0),cxt(c),mask((1<<m)-1),pr(c),limit(lim),CxtState(mask+1),x(bd),index(0),count(c) {
  sm=new StateMap[c];
  }
  void set(U32 cx) {//da hell is hapening??????????? nothing, all good :D
       CxtState[cxt[index]]=nex(CxtState[cxt[index]],x.y);       //update state
       cxt[index]=(cx)&mask;                                     // get new context
       pr[index]=sm[index].p(CxtState[cxt[index]],x.y,limit);    // predict from new context
       index++;
       if (index==count) index=0;
  }
  inline int p() {
      int pr0=pr[index++];
      if (index==count) index=0;
      return pr0;
  }
  int mix(int m) {
    return 0;
  }
};

#define ispowerof2(x) ((x&(x-1))==0)
class DynamicHSMap {
  int state;
  StateMap *sm;
  Array<int>  pr;
  int limit;
  BlockData& x;
  int index;
  const int count;
  const int hashElementCount;
  const int hashSearchLimit;
  Array<U8*> cp;
  const int bitscount;
  U32 n;
  Array<U8,32> t;
  public:
  DynamicHSMap(int bits,int membits,int countOfContexts,BlockData& bd): 
  state(0),pr(countOfContexts),
  limit(1023),x(bd),index(0),count(countOfContexts),
  // for jpeg there is 3 bits -> 8
  // for bmp4 there is 4 bits -> 16
  hashElementCount((1<<bits)), //+1 for checksum 
  hashSearchLimit(4),
  cp(countOfContexts),  
  bitscount(bits),
  n((1<<membits)-1),
  t(hashElementCount*(1<<membits))
  {
     /* printf("hashElementCount %d\n",hashElementCount);
      printf("countOfContexts %d\n",countOfContexts);
      printf("bits %d\n",bits);
      printf("n %d\n",n);
      printf("t.size %d\n",t.size());
      printf("membits %d %d %d\n",membits,1<<membits,hashElementCount*(1<<membits));*/
    sm=new StateMap[countOfContexts];
    for (int i=0;i<countOfContexts;i++)
      cp[i]=&t[0]+1;
  }
  void set(U32 cx) {
   if (cp[count-1])  for ( int i=0; i<count; ++i) *cp[i]=nex( *cp[i],x.y);   //update state
      if (cx>255){
         cp[index]=find(cx)+1 ;                                      // find new
         pr[index]=sm[index].p(*cp[index],x.y,limit);
         index++;
         if (index==count) index=0;
      }else{
          if (cx==0) {
          index=0;
          return;}
        for ( int i=0; i<count; ++i) {
        cp[i]+=cx;
        pr[i]=sm[i].p(*cp[i],x.y,limit);                             // predict from new context
        }
        index=0;
      }      
  }
  inline int p() {
      int pr0=pr[index++];
      if (index==count) index=0;
      return pr0;
  }
  int mix(int m) {
    return 0;
  }
  U8* find(U32 i) {
    U8 chk=(i>>24^i>>12^i);
    i&=n;
    int bi=i, b=256;  // best replacement so far
    U8 *p;
    for (int j=0; j<hashSearchLimit; ++j) {
      p=&t[(i^j)*hashElementCount];
      if (p[0]==chk) return p;  // match
      else if (p[1]==0) return p[0]=chk, p;  // empty
      else if (p[1]<b) b=p[1], bi=i^j;  // best replacement so far
    }
    p=&t[bi*hashElementCount];  // replacement element
    memset(p, 0, hashElementCount);
    p[0]=chk;
  return p;
}
};
/*
class IndirectMap {
  Array<U8> Data;
  StateMap Map;
  const int mask, maskbits, stride;
  int Context, bCount, bTotal, B;
  U8 *cp;
public:
  IndirectMap(int BitsOfContext, int InputBits = 8): Data((1ull<<BitsOfContext)*((1ull<<InputBits)-1)), mask((1<<BitsOfContext)-1), maskbits(BitsOfContext), stride((1<<InputBits)-1), Context(0), bCount(0), bTotal(InputBits), B(0) {
    assert(InputBits>0 && InputBits<=8);
    assert(BitsOfContext+InputBits<=24);
    cp=&Data[0];
  }
  void set(const U32 ctx) {
    Context = (ctx&mask)*stride;
    bCount=B=0;
  }
  void mix(Mixer& m, const int Multiplier = 1, const int Divisor = 4, const U16 Limit = 1023) {
    // update
    *cp = nex(*cp, m.x.y);
    // predict
    B+=(m.x.y && B>0);
    cp=&Data[Context+B];
    const U8 state = *cp;
    const int p1 = Map.p(state,m.x.y, Limit);
    m.add((stretch(p1)*Multiplier)/Divisor);
    m.add(((p1-2048)*Multiplier)/(Divisor*2));
    bCount++; B+=B+1;
    if (bCount==bTotal)
      bCount=B=0;
  }
};*/
// Context map for large contexts.  Most modeling uses this type of context
// map.  It includes a built in RunContextMap to predict the last byte seen
// in the same context, and also bit-level contexts that map to a bit
// history state.
//
// Bit histories are stored in a hash table.  The table is organized into
// 64-byte buckets alinged on cache page boundaries.  Each bucket contains
// a hash chain of 7 elements, plus a 2 element queue (packed into 1 byte)
// of the last 2 elements accessed for LRU replacement.  Each element has
// a 2 byte checksum for detecting collisions, and an array of 7 bit history
// states indexed by the last 0 to 2 bits of context.  The buckets are indexed
// by a context ending after 0, 2, or 5 bits of the current byte.  Thus, each
// byte modeled results in 3 main memory accesses per context, with all other
// accesses to cache.
//
// On bits 0, 2 and 5, the context is updated and a new bucket is selected.
// The most recently accessed element is tried first, by comparing the
// 16 bit checksum, then the 7 elements are searched linearly.  If no match
// is found, then the element with the lowest priority among the 5 elements
// not in the LRU queue is replaced.  After a replacement, the queue is
// emptied (so that consecutive misses favor a LFU replacement policy).
// In all cases, the found/replaced element is put in the front of the queue.
//
// The priority is the state number of the first element (the one with 0
// additional bits of context).  The states are sorted by increasing n0+n1
// (number of bits seen), implementing a LFU replacement policy.
//
// When the context ends on a byte boundary (bit 0), only 3 of the 7 bit
// history states are used.  The remaining 4 bytes implement a run model
// as follows: <count:7,d:1> <b1> <unused> <unused> where <b1> is the last byte
// seen, possibly repeated.  <count:7,d:1> is a 7 bit count and a 1 bit
// flag (represented by count * 2 + d).  If d=0 then <count> = 1..127 is the
// number of repeats of <b1> and no other bytes have been seen.  If d is 1 then
// other byte values have been seen in this context prior to the last <count>
// copies of <b1>.
//
// As an optimization, the last two hash elements of each byte (representing
// contexts with 2-7 bits) are not updated until a context is seen for
// a second time.  This is indicated by <count,d> = <1,0> (2).  After update,
// <count,d> is updated to <2,0> or <1,1> (4 or 3).
#if defined(__AVX2__)
#define MALIGN 32
#else
#define MALIGN 16
#endif
inline U64 CMlimit(U64 size){
    //if (size>(0x100000000UL)) return (0x100000000UL); //limit to 4GB, using this will consume lots of memory above level 11
    if (size>(0x80000000UL)) return (0x80000000UL); //limit to 2GB
    return (size);
}
class ContextMap {
  const int C;  // max number of contexts
  class E {  // hash element, 64 bytes
    U16 chk[7];  // byte context checksums
    U8 last;     // last 2 accesses (0-6) in low, high nibble
  public:
    U8 bh[7][7]; // byte context, 3-bit context -> bit history state
      // bh[][0] = 1st bit, bh[][1,2] = 2nd bit, bh[][3..6] = 3rd bit
      // bh[][0] is also a replacement priority, 0 = empty
    U8* get(U16 chk);  // Find element (0-6) matching checksum.
      // If not found, insert or replace lowest priority (not last).
  };
  Array<E, 64> t;  // bit histories for bits 0-1, 2-4, 5-7
    // For 0-1, also contains a run count in bh[][4] and value in bh[][5]
    // and pending update count in bh[7]
  Array<U8*> cp;   // C pointers to current bit history
  Array<U8*> cp0;  // First element of 7 element array containing cp[i]
  Array<U32> cxt;  // C whole byte contexts (hashes)
  Array<U8*> runp; // C [0..3] = count, value, unused, unused
  Array<short, MALIGN>  r0;   //for rle 
  Array<short, MALIGN>  r1;
  Array<short, MALIGN>  r0i;
  Array<short, MALIGN>  rmask; // mask for skiped context
  StateMap *sm;    // C maps of state -> p
  int cn;          // Next context to set by set()
  //void update(U32 cx, int c);  // train model that context cx predicts c
  Random rnd;
  int result;
  BlockData& x;
  
  int mix1(int m, int cc, int bp, int c1, int y1);
    // mix() with global context passed as arguments to improve speed.
    
public:
  ContextMap(U64 m, int c ,BlockData& bd);  // m = memory in bytes, a power of 2, C = c
  ~ContextMap();
  void set(U32 cx, int next=-1);   // set next whole byte context to cx
    // if next is 0 then set order does not matter
  int mix(int m) {return mix1(m,  x.c0,  x.bpos, (U8) x.c4,  x.y);}
  int get() {return result;}
  int inputs();
};

#if defined(SIMD_GET_SSE) 

#define xmmbshl(x,n)  _mm_slli_si128(x,n) // xm <<= 8*n  -- BYTE shift left
#define xmmbshr(x,n)  _mm_srli_si128(x,n) // xm >>= 8*n  -- BYTE shift right
#define xmmshl64(x,n) _mm_slli_epi64(x,n) // xm.hi <<= n, xm.lo <<= n
#define xmmshr64(x,n) _mm_srli_epi64(x,n) // xm.hi >>= n, xm.lo >>= n
#define xmmand(a,b)   _mm_and_si128(a,b)
#define xmmor(a,b)    _mm_or_si128(a,b)
#define xmmxor(a,b)   _mm_xor_si128(a,b)
#define xmmzero       _mm_setzero_si128()
#ifdef _MSC_VER
#include <intrin.h>
U32 __inline clz(U32 value){ //asume newer version of vc
    unsigned long leading_zero = 0;
    if (_BitScanReverse( &leading_zero, value)) return 31-leading_zero;
    else return 32;
}
  U32 __inline ctz(U32 x ){
    unsigned long leading_zero = 0;
   if (_BitScanForward(&leading_zero, x )) {
       return  leading_zero;
    }
    else{
         return 0; // Same remarks as above
    }
}

#elif ((__GNUC__ >= 4) || ((__GNUC__ == 3) && (__GNUC_MINOR__ >= 4)))
U32 __inline clz(U32 value){ 
    return __builtin_clz(value);
}
U32 __inline ctz(U32 value){ 
    return __builtin_ctz(value);
}
#endif
#endif

// Find or create hash element matching checksum ch
inline U8* ContextMap::E::get(U16 ch) {
    
  if (chk[last&15]==ch) return &bh[last&15][0];
  int b=0xffff, bi=0;
#if defined(SIMD_GET_SSE)   && defined(__MMX__)
  const XMM xmmch=_mm_set1_epi16 (short(ch)); //fill 8 ch values
//load 8 values, discard last one as only 7 are needed.
//reverse order and compare 7 chk values to ch
//get mask is set get first index and return value  
  XMM tmp=_mm_load_si128 ((XMM *) &chk[0]); //load 8 values (8th will be discarded)
#if defined(__SSSE3__)
#include <xmmintrin.h>
 // const XMM vm=;// initialise vector mask 
  tmp=_mm_shuffle_epi8(tmp,_mm_setr_epi8(14,15,12,13,10,11,8,9,6,7,4,5,2,3,0,1));   
#elif   defined(__SSE2__) 
  tmp=_mm_shufflelo_epi16(tmp,0x1B); //swap order for mask  (0,1,2,3)
  tmp=_mm_shufflehi_epi16(tmp,0x1B);                      //(0,1,2,3)
  tmp=_mm_shuffle_epi32(tmp,0x4E);                        //(1,0,3,2)   
#endif
  tmp=_mm_cmpeq_epi16 (tmp,xmmch); //compare ch values
  tmp=_mm_packs_epi16(tmp,xmmzero); //pack result
  U32 t=(_mm_movemask_epi8(tmp))>>1; //get mask of comparsion, bit is set if eq, discard 8th bit
  U32 a;    //index into bh or 7 if not found
  if(t){
      a=(clz(t)-1)&7;
      return last=last<<4|a, (U8*)&bh[a][0];
  }

 XMM   lastl=_mm_set1_epi8((last&15));
 XMM   lasth=_mm_set1_epi8((last>>4));
 XMM   one1  =_mm_set1_epi8(1);
 XMM   vm=_mm_setr_epi8(0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7);

 XMM   lastx=_mm_unpacklo_epi64(lastl,lasth); //last&15 last>>4
 XMM   eq0  =_mm_cmpeq_epi8 (lastx,vm); //compare   values

 eq0=_mm_or_si128(eq0,_mm_srli_si128 (eq0, 8));    //or low values with high

 lastx = _mm_and_si128(one1, eq0);                //set to 1 if eq
 XMM sum1 = _mm_sad_epu8(lastx,xmmzero);        //cout values, abs(a0 - b0) + abs(a1 - b1) .... up to b8
 const U32 pcount=_mm_cvtsi128_si32(sum1); //population count
/*for (int i=0; i<7; ++i) {
   bh[i][0]=i+1;
    
  }*/
 U32 t0=(~_mm_movemask_epi8(eq0));
for (int i=pcount; i<7; ++i) {
    int bitt =ctz(t0);     //get index 
//#if ((__GNUC__ >= 4) || ((__GNUC__ == 3) && (__GNUC_MINOR__ >= 4)))    
//asm("btr %1,%0" : "+r"(t0) : "r"(bitt)); // clear bit set and test again https://gcc.gnu.org/bugzilla/show_bug.cgi?id=47769
//#else
    t0 &= ~(1 << bitt); // clear bit set and test again
//#endif 
   int pri=bh[bitt][0];
    if (pri<b  ) b=pri, bi=bitt;


  }  
  /*
  //uncomment above SIMD version and comment out code below to use full SIMD (SSE2) version
  for (int i=0; i<7; ++i) {
    int pri=bh[i][0];
    if (pri<b && (last&15)!=i && (last>>4)!=i) b=pri, bi=i;
  }*/
  return last=0xf0|bi, chk[bi]=ch, (U8*)memset(&bh[bi][0], 0, 7);
#else
  for (int i=0; i<7; ++i) {
    if (chk[i]==ch) return last=last<<4|i, (U8*)&bh[i][0];
    int pri=bh[i][0];
    if (pri<b && (last&15)!=i && last>>4!=i) b=pri, bi=i;
  }
  return last=0xf0|bi, chk[bi]=ch, (U8*)memset(&bh[bi][0], 0, 7);
#endif
}

// Construct using m bytes of memory for c contexts(c+7)&-8
ContextMap::ContextMap(U64 m, int c, BlockData& bd): C(c),  t(m>>6), cp(C), cp0(C),
    cxt(C), runp(C), r0(C),r1(C),r0i(C),rmask(C),cn(0),result(0),x(bd) {
  assert(m>=64 && (m&m-1)==0);  // power of 2?
  assert(sizeof(E)==64);
  sm=new StateMap[C];
  for (int i=0; i<C; ++i) {
    cp0[i]=cp[i]=&t[0].bh[0][0];
    runp[i]=cp[i]+3;
  }
}

ContextMap::~ContextMap() {
  delete[] sm;
}

// Set the i'th context to cx
inline void ContextMap::set(U32 cx, int next) {
  int i=cn++;
  //i&=next;
 //assert(i>=0 && i<C);
  if (cx==0){ cxt[i]=0; rmask[i]=0;}
  else{
  cx=cx*987654323+i;  // permute (don't hash) cx to spread the distribution
  cx=cx<<16|cx>>16;
  cxt[i]=cx*123456791+i;
  rmask[i]=-1;
  }
}
// Predict to mixer m from bit history state s, using sm to map s to
// a probability.

inline int mix2(BlockData& x, int m, int s, StateMap& sm) {
  int p1=sm.p(s,x.y);
  int n0=-!nex(s,2);
  int n1=-!nex(s,3);
  int st=stretch(p1)>>2;
  x.mxInputs[m].add(st); //m.add(st);
  p1>>=4;
  int p0=255-p1;
  x.mxInputs[m].add(p1-p0);//m.add(p1-p0);
  x.mxInputs[m].add(st*(n1-n0));//m.add(st*(n1-n0));
  x.mxInputs[m].add((p1&n0)-(p0&n1));//m.add((p1&n0)-(p0&n1));
  x.mxInputs[m].add((p1&n1)-(p0&n0));//m.add((p1&n1)-(p0&n0));
  return s>0;
}
// Update the model with bit y1, and predict next bit to mixer m.
// Context: cc=c0, bp=bpos, c1=buf(1), y1=y.
int ContextMap::mix1(int m, int cc, int bp, int c1, int y1) {
  // Update model with y
   result=0;

  for (int i=0; i<cn; ++i) {
   if(cxt[i]){
    if (cp[i]) {
      assert(cp[i]>=&t[0].bh[0][0] && cp[i]<=&t[t.size()-1].bh[6][6]);
      assert(((long long)(cp[i])&63)>=15);
      int ns=nex(*cp[i], y1);
      if (ns>=204 && rnd() << ((452-ns)>>3)) ns-=4;  // probabilistic increment
      *cp[i]=ns;
    }

    // Update context pointers
    if (x.bpos>1 && runp[i][0]==0)
    {
     cp[i]=0;
    }
    else
    {
     U16 chksum=cxt[i]>>16;
     U64 tmask=t.size()-1;
     switch(x.bpos)
     {
      case 1: case 3: case 6: cp[i]=cp0[i]+1+(cc&1); break;
      case 4: case 7: cp[i]=cp0[i]+3+(cc&3); break;
      case 2: case 5: cp0[i]=cp[i]=t[(cxt[i]+cc)&tmask].get(chksum); break;
      default:
      {
       cp0[i]=cp[i]=t[(cxt[i]+cc)&tmask].get(chksum);
       // Update pending bit histories for bits 2-7
       if (cp0[i][3]==2) {
         const int c=cp0[i][4]+256;
         U8 *p=t[(cxt[i]+(c>>6))&tmask].get(chksum);
         p[0]=1+((c>>5)&1);
         p[1+((c>>5)&1)]=1+((c>>4)&1);
         p[3+((c>>4)&3)]=1+((c>>3)&1);
         p=t[(cxt[i]+(c>>3))&tmask].get(chksum);
         p[0]=1+((c>>2)&1);
         p[1+((c>>2)&1)]=1+((c>>1)&1);
         p[3+((c>>1)&3)]=1+(c&1);
         cp0[i][6]=0;
       }
       // Update run count of previous context
       if (runp[i][0]==0)  // new context
         runp[i][0]=2, runp[i][1]=c1;
       else if (runp[i][1]!=c1)  // different byte in context
         runp[i][0]=1, runp[i][1]=c1;
       else if (runp[i][0]<254)  // same byte in context
         runp[i][0]+=2;
       else if (runp[i][0]==255)
         runp[i][0]=128;
       runp[i]=cp0[i]+3;
      } break;
     }
    }
    // predict from bit context
    int s = 0;
    if (cp[i]) s = *cp[i];
    if (s>0) result++;
    mix2(x,m, s, sm[i]);


  }else{
    for (int i=0; i<(inputs()-1); i++)
       x.mxInputs[m].add(0);// m.add(0);     
  }
  }
    // predict from last byte in context
     
     for (int i=0; i<cn; ++i) {
         U8 a=runp[i][0];
         U8 b=runp[i][1];
         r0[i]=a;
         r1[i]=b;
         r0i[i]=ilog(a+1);
     }

#if defined(SIMD_CM_R ) && defined(__AVX2__)
    const int bsh=(8-bp);
    const int bsh1=(7-bp);
    int cnc=(cn/16)*16;
    int i;
    
    for ( i=0; i<(cnc); i=i+16) {
        YMM b1=_mm256_set1_epi16(1<<bsh1);
        YMM x0=_mm256_setzero_si256();
        YMM x1=_mm256_set1_epi16(1);
        YMM b256=_mm256_set1_epi16(256);
        YMM runm=_mm256_load_si256 ((YMM  *) &rmask[i]);
        YMM b=_mm256_load_si256 ((YMM  *) &r1[i]);
        YMM xcc=_mm256_set1_epi16(cc);
        //(r1[i  ]+256)>>(8-bp)==cc
        
        YMM  xr1=_mm256_add_epi16 (b, b256);
        xr1=_mm256_srli_epi16 (xr1, bsh);
        xr1=_mm256_cmpeq_epi16(xr1,xcc); //(a == b) ? 0xffff : 0x0
        //b                           //((r1[i  ]>>(7-bp)&1)*2-1) 
       
        YMM xb=_mm256_and_si256 (b, b1); //test if bit set                   //>>(7-bp)&1)*2
        xb=_mm256_cmpeq_epi16(xb,x0);//compare and if eq set to -1, else 0   //
        xb= _mm256_or_si256(xb,x1); // or with 1                              // -1
        //c                                                       //((r0i[i  ])<<(2+(~r0[i  ]&1)))
        YMM xr0i=_mm256_load_si256 ((YMM  *) &r0i[i]);//, x0);           //r0i[i]
        YMM  c=_mm256_load_si256 ((YMM  *) &r0[i]);//, x0);              //~r0[i]&1
        YMM xc=_mm256_andnot_si256 (c,x1);  
        YMM r0ia= _mm256_add_epi16 (x1,xc);                          //1+(~r0[i]&1) result is 2 or 1 for multiplay |
        xc=_mm256_slli_epi16(xr0i, 2);                               //r0i[i]<<2                                  | 
        xc=_mm256_mullo_epi16(xc,r0ia);                              //(r0i[i]<<2*)  ~r0[i]&1?1+(~r0[i]&1):1      <-
        //b*c                                                     // (r0i[i  ])<<(2+(~r0[i  ]&1)))
        YMM xr=_mm256_sign_epi16(xc,xb); 
        YMM xresult=_mm256_and_si256(xr,xr1);   //(r1[i  ]+256)>>(8-bp)==cc?xr:0
        xresult=_mm256_and_si256(xresult,runm); //mask out skiped context
        //store result
        m.addYMM(xresult);
    }
     int cnc1=((cn-cnc)/8)*8;
        if (cnc1){
        i=cnc;
        cnc=cnc+8;
        XMM x0=_mm_setzero_si128();
        XMM x1=_mm_set1_epi16(1);
        XMM b1=_mm_set1_epi16(1<<bsh1);
        XMM xcc=_mm_set1_epi16(cc);
        XMM b256=_mm_set1_epi16(256);
        XMM runm=_mm_load_si128 ((XMM  *) &rmask[i]);
        XMM b=_mm_load_si128 ((XMM  *) &r1[i]);
        //(r1[i  ]+256)>>(8-bp)==cc
        XMM  xr1=_mm_add_epi16 (b, b256);
        xr1=_mm_srli_epi16 (xr1, bsh);
        xr1=_mm_cmpeq_epi16(xr1,xcc); //(a == b) ? 0xffff : 0x0
        //b                           //((r1[i  ]>>(7-bp)&1)*2-1) 
        XMM xb=_mm_and_si128 (b, b1); //test if bit set                   //>>(7-bp)&1)*2
        xb=_mm_cmpeq_epi16(xb,x0);//compare and if eq set to -1, else 0   //
        xb= _mm_or_si128(xb,x1); // or with 1                              // -1
         //c                                                       //((r0i[i  ])<<(2+(~r0[i  ]&1)))
        XMM xr0i=_mm_load_si128 ((XMM  *) &r0i[i]);           //r0i[i]
        XMM  c=_mm_load_si128 ((XMM  *) &r0[i]);              //~r0[i]&1
        XMM xc=_mm_andnot_si128 (c,x1);  
        XMM r0ia= _mm_add_epi16 (x1,xc);                          //1+(~r0[i]&1) result is 2 or 1 for multiplay |
        xc=_mm_slli_epi16(xr0i, 2);                               //r0i[i]<<2                                  | 
        xc=_mm_mullo_epi16(xc,r0ia);                              //(r0i[i]<<2*)  ~r0[i]&1?1+(~r0[i]&1):1      <-
        //b*c                                                     // (r0i[i  ])<<(2+(~r0[i  ]&1)))
        XMM xr=_mm_sign_epi16(xc,xb); 
        XMM xresult=_mm_and_si128(xr,xr1);   //(r1[i  ]+256)>>(8-bp)==cc?xr:0
        xresult=_mm_and_si128(xresult,runm); //mask out skiped context
        //store result
        m.addXMM(xresult);
    }
    //do remaining 
    for (int i=cnc; i<cn; ++i) {
        if (rmask[i] &&( (r1[i  ]+256)>>(8-bp)==cc)) {
            m.add(((r1[i  ]>>(7-bp)&1)*2-1) *((r0i[i  ])<<(2+(~r0[i  ]&1)))); }
        else   m.add(0);
    }
#elif defined(SIMD_CM_R ) && defined(__SSSE3__) 
    int cnc=(cn/8)*8;
    const int bsh=(8-bp);
     const int bsh1=(7-bp);
     XMM  x0=_mm_setzero_si128();
     XMM  x1=_mm_set1_epi16(1);
     XMM  b1=_mm_set1_epi16(1<<bsh1);
     XMM xcc=_mm_set1_epi16(cc);
     XMM   b256=_mm_set1_epi16(256);
    for (int i=0; i<(cnc); i=i+8) {
        
        XMM   runm=_mm_load_si128 ((XMM  *) &rmask[i]);
        XMM   b=_mm_load_si128 ((XMM  *) &r1[i]);
        //(r1[i  ]+256)>>(8-bp)==cc
        XMM  xr1=_mm_add_epi16 (b, b256);
        //XMM  xr1=_mm_add_epi16 (*(XMM  *) &r1[i], _mm_set1_epi16(256));
        xr1=_mm_srli_epi16 (xr1, bsh);
        xr1=_mm_cmpeq_epi16(xr1,xcc); //(a == b) ? 0xffff : 0x0
        //b                           //((r1[i  ]>>(7-bp)&1)*2-1) 
        XMM xb=_mm_and_si128 (b, b1); //test if bit set                   //>>(7-bp)&1)*2
        xb=_mm_cmpeq_epi16(xb,x0);//compare and if eq set to -1, else 0   //
        xb= _mm_or_si128(xb,x1); // or with 1                              // -1
        //c                                                       //((r0i[i  ])<<(2+(~r0[i  ]&1)))
        XMM xr0i=_mm_load_si128 ((XMM  *) &r0i[i]);           //r0i[i]
        XMM  c=_mm_load_si128 ((XMM  *) &r0[i]);              //~r0[i]&1
        XMM xc=_mm_andnot_si128 (c,x1);  
        XMM r0ia= _mm_add_epi16 (x1,xc);                          //1+(~r0[i]&1) result is 2 or 1 for multiplay |
        xc=_mm_slli_epi16(xr0i, 2);                               //r0i[i]<<2                                  | 
        xc=_mm_mullo_epi16(xc,r0ia);                              //(r0i[i]<<2*)  ~r0[i]&1?1+(~r0[i]&1):1      <-
        //b*c                                                     // (r0i[i  ])<<(2+(~r0[i  ]&1)))
        XMM xr=_mm_sign_epi16(xc,xb);// _mm_mullo_epi16(xc,xb); slower ?
        XMM xresult=_mm_and_si128(xr,xr1);   //(r1[i  ]+256)>>(8-bp)==cc?xr:0
        xresult=_mm_and_si128(xresult,runm); //mask out skiped context
        //store result
        m.addXMM(xresult);
    }
    //do remaining 
    for (int i=cnc; i<cn; ++i) {
        if (rmask[i] &&( (r1[i  ]+256)>>bsh==cc)) {
            m.add(((r1[i  ]>>bsh1&1)*2-1) *((r0i[i  ])<<(2+(~r0[i  ]&1)))); }
        else   m.add(0);
    }
#else          
    for (int i=0; i<cn; ++i) {
        if (rmask[i] && ((r1[i  ]+256)>>(8-bp)==cc)) {
            x.mxInputs[m].add(((r1[i  ]>>(7-bp)&1)*2-1) *((r0i[i  ])<<(2+(~r0[i  ]&1)))); }
        else   x.mxInputs[m].add(0);//m.add(0);
      }
#endif    
   
  if (bp==7) cn=0;
  return result;
}
int ContextMap::inputs() {
    return 6;
}
  
//////////////////////////// Predictor /////////////////////////
// A Predictor estimates the probability that the next bit of
// uncompressed data is 1.  Methods:
// p() returns P(1) as a 12 bit number (0-4095).
// update(y) trains the predictor with the actual bit (0 or 1).
#include "vm.cpp"

//general predicor class
class Predictor {
public:
 BlockData x; //maintains current global data block
 int pr;  
  VM vm;
  Predictor(char *m): pr(2048),vm(m,x,VMCOMPRESS) {setdebug(0); }
  int p()  {assert(pr>=0 && pr<4096); return pr;} 
  ~Predictor(){ vm.killvm( );}
  void set() {  vm.block(x.finfo,0);  }
  void setdebug(int a){      vm.debug=a;  }
  void update()  {
    //update0(); // Update global context: pos, bpos, c0, c4
    x.c0+=x.c0+x.y;
    if (x.c0>=256) {
        x.c4=(x.c4<<8)+(x.c0&0xff);
        x.c0=1;
        ++x.blpos;
    }
    x.bpos=(x.bpos+1)&7;
    vm.updateComponents();
    pr=vm.doupdate(x.y,x.c0,x.bpos,x.c4,p());
    if (pr!=0) quit();
    //printf("%d",x.cInputs);
    pr=vm.getPrediction(x.cInputs);
    //printf("%d",x.cInputs);
  }
};
//////////////////////////// Encoder ////////////////////////////

// An Encoder does arithmetic encoding.  Methods:
// Encoder(COMPRESS, f) creates encoder for compression to archive f, which
//   must be open past any header for writing in binary mode.
// Encoder(DECOMPRESS, f) creates encoder for decompression from archive f,
//   which must be open past any header for reading in binary mode.
// code(i) in COMPRESS mode compresses bit i (0 or 1) to file f.
// code() in DECOMPRESS mode returns the next decompressed bit from file f.
//   Global y is set to the last bit coded or decoded by code().
// compress(c) in COMPRESS mode compresses one byte.
// decompress() in DECOMPRESS mode decompresses and returns one byte.
// flush() should be called exactly once after compression is done and
//   before closing f.  It does nothing in DECOMPRESS mode.
// size() returns current length of archive
// setFile(f) sets alternate source to FILE* f for decompress() in COMPRESS
//   mode (for testing transforms).
// If level (global) is 0, then data is stored without arithmetic coding.

typedef enum {COMPRESS, DECOMPRESS} Mode;
class Encoder {
private:
  const Mode mode;       // Compress or decompress?
  File* archive;         // Compressed data file
  U32 x1, x2;            // Range, initially [0, 1), scaled by 2^32
  U32 x;                 // Decompress mode: last 4 input bytes of archive
  File*alt;             // decompress() source in COMPRESS mode

  // Compress bit y or return decompressed bit
  void code(int i=0) {
    int p=predictor->p();
    p+=p==0;
    assert(p>0 && p<4096);
    U32 xmid=x1 + ((x2-x1)>>12)*p + (((x2-x1)&0xfff)*p>>12);
    assert(xmid>=x1 && xmid<x2);
    predictor->x.y=i;
    i ? (x2=xmid) : (x1=xmid+1);
    predictor->update();
    while (((x1^x2)&0xff000000)==0) {  // pass equal leading bytes of range
      archive->putc(x2>>24);
      x1<<=8;
      x2=(x2<<8)+255;
    }
  }
  int decode() {
    int p=predictor->p();
    p+=p==0;
    assert(p>0 && p<4096);
    U32 xmid=x1 + ((x2-x1)>>12)*p + (((x2-x1)&0xfff)*p>>12);
    assert(xmid>=x1 && xmid<x2);
    x<=xmid ? (x2=xmid,predictor->x.y=1) : (x1=xmid+1,predictor->x.y=0);
    predictor->update();
    while (((x1^x2)&0xff000000)==0) {  // pass equal leading bytes of range
      x1<<=8;
      x2=(x2<<8)+255;
      x=(x<<8)+(archive->getc()&255);  // EOF is OK
    }
    return predictor->x.y;
  }

public:
  Predictor *predictor;
  Encoder(Mode m, File* f,char *model);
  Mode getMode() const {return mode;}
  U64 size() const {return  archive->curpos();}  // length of archive so far
  void flush();  // call this when compression is finished
  void setFile(File* f) {alt=f;}

  // Compress one byte
  void compress(int c) {
    assert(mode==COMPRESS);
    if (level==0)
      archive->putc(c);
    else {
      for (int i=7; i>=0; --i)
        code((c>>i)&1);
    }
  }

  // Decompress and return one byte
  int decompress() {
    if (mode==COMPRESS) {
      assert(alt);
      return alt->getc();
    }
    else if (level==0){
     int a;
     a=archive->getc();
      return a ;}
    else {
      int c=0;
      for (int i=0; i<8; ++i)
        c+=c+decode();
      
      return c;
    }
  }
  ~Encoder(){
  if (predictor) delete predictor;
   }
};

Encoder::Encoder(Mode m, File* f,char *model):
    mode(m), archive(f), x1(0), x2(0xffffffff), x(0), alt(0) {        
    if (model!=0)         predictor=new Predictor(model);
    else predictor=0;
    // x = first 4 bytes of archive
    if (level>0 && mode==DECOMPRESS) {
      for (int i=0; i<4; ++i)
        x=(x<<8)+(archive->getc()&255);
    }
    for (int i=0; i<1024; ++i)
      dt[i]=16384/(i+i+3);
}

void Encoder::flush() {
  if (mode==COMPRESS && level>0)
    archive->put32(x1);  // Flush first unequal byte of range
}
 
/////////////////////////// Filters /////////////////////////////////
//
// Before compression, data is encoded in blocks with the following format:
//
//   <type> <size> <encoded-data>
//
// Type is 1 byte (type Filetype): DEFAULT=0, JPEG, EXE, ...
// Size is 4 bytes in big-endian format.
// Encoded-data decodes to <size> bytes.  The encoded size might be
// different.  Encoded data is designed to be more compressible.
//
//   void encode(File* in, File* out, int n);
//
// Reads n bytes of in (open in "rb" mode) and encodes one or
// more blocks to temporary file out (open in "wb+" mode).
// The file pointer of in is advanced n bytes.  The file pointer of
// out is positioned after the last byte written.
//
//   en.setFile(File* out);
//   int decode(Encoder& en);
//
// Decodes and returns one byte.  Input is from en.decompress(), which
// reads from out if in COMPRESS mode.  During compression, n calls
// to decode() must exactly match n bytes of in, or else it is compressed
// as type 0 without encoding.
//
//   Filetype detect(File* in, int n, Filetype type);
//
// Reads n bytes of in, and detects when the type changes to
// something else.  If it does, then the file pointer is repositioned
// to the start of the change and the new type is returned.  If the type
// does not change, then it repositions the file pointer n bytes ahead
// and returns the old type.
//
// For each type X there are the following 2 functions:
//
//   void encode_X(File* in, File* out, int n, ...);
//
// encodes n bytes from in to out.
//
//   int decode_X(Encoder& en);
//
// decodes one byte from en and returns it.  decode() and decode_X()
// maintain state information using static variables.


struct vStream {
    U32 stream;    //id for stream
    char  model[16];     // model for stream, will be stored in archive if stream is used
    int size;      // size of above model
    U8  enabled;  // 1 if atleast one type uses it othewise 0
};      
struct vType {
    int type;      // -1 its recursive type
                   //  0 its unknown data type
                   // +1 its known data type
    U32 streamId;  //  id for stream
    char detect[16]; // model for detection
    int dsize;     // size of above model, -1 if no model
    char decode[16]; // model for decode, will be stored in archive
    int desize;    // size of above model, -1 if no model
    char encode[16]; // model for encode
    int ensize;    // size of above model, -1 if no model
    int used;
};
Array<vStream> vStreams(0);
Array<vType> vTypes(0);
BlockData z;
VM   **vmDetect;
VM   **vmEncode;
VM   **vmDecode;
VM   **vmStream;
enum {NONE=0,START,INFO,END,RESET=0xfffffffe,REQUEST=0xffffffff}; 

int detect(File* in, U64 n, int type, int &info, int &info2, int it=0,int s1=0) {
    U32 buf0=0;  // last 8 bytes
    U64 start= in->curpos();
info=-1;
    static int deth=0,detd=0;  // detected header/data size in bytes
    static int dett;      // detected block type
    if (deth >1) return  in->setpos(start+deth),deth=0,dett;
    else if (deth ==-1) return  in->setpos(start),deth=0,dett;
    else if (detd) return  in->setpos( start+detd),detd=0,defaultType;
    int dstate=0;
    for (int j=0;j<vTypes.size();j++){
        if ( vTypes[j].dsize!=-1){
            //reset states            
            dstate=vmDetect[j]->detect(0,RESET);
            if (dstate!=-1 ){     }
        }
    }
    for (U64 i=0; i<n; ++i) {
        int c=in->getc();
        if (c==EOF) return (-1);
        buf0=buf0<<8|c;
        
        for (int j=0;j<vTypes.size();j++){
            if ( vTypes[j].dsize!=-1){
                //open type detection file and load into memory
                dstate=vmDetect[j]->detect(buf0,i);
                if (dstate==START && type==defaultType){ //start vTypes[j].type
                    //start
                    //request current state data
                    int jst=vmDetect[j]->detect(buf0,REQUEST);
                    return  in->setpos(start+jst), j;// rturn array index for now // vTypes[j].type;
                }
                if (dstate==INFO){ //info
                    info=vmDetect[j]->detect(buf0,REQUEST);
                    // expect START state next
                }
                if (dstate==END){ //end
                    //request current state data
                    
                    int jst=vmDetect[j]->detect(buf0,REQUEST);
                    return in->setpos( start+jst),defaultType;
                }
            }
        }
    }
    return type;
}

typedef enum {FDECOMPRESS, FCOMPARE, FDISCARD} FMode;

void encode_file(File* in, File* out, int len, int info,int type) {
    //set in and out file for vm read/write
    assert(vTypes[type].ensize!=-1);
    vmEncode[type]->inpos=in->curpos();
    vmEncode[type]->inFile=in;
    vmEncode[type]->outFile=out;
    //encode file
    vmEncode[type]->encode(info,len);
}

uint64_t decode_file(Encoder& en, int size, File *out,int info, FMode mode, uint64_t &diffFound, int type) {
    assert(vTypes[type].ensize!=-1);
    File *e; // for compare
    File *d; // for decompression
    if (mode==FCOMPARE ){
       e=new FileTmp();
       vmDecode[type]->outFile=e;
    } else {
        vmDecode[type]->outFile=out;
    }    
    d=new FileTmp();
    for (int i=0; i<size; i++) d->putc(en.decompress());
    d->setpos(0);
    vmDecode[type]->inFile=d;
    int jst=vmDecode[type]->decode(info,size);
    if (mode==FCOMPARE ){
       int outsize=(U32)e->curpos();
       e->setpos(0);
       for (int i=0; i<jst; i++) {
           if (e->getc()!=out->getc() && !diffFound) diffFound=i;   
       }
       return (U32)e->curpos();
    }
    if (mode==FDECOMPRESS) {
        return jst;
    }
    return size;
}

uint64_t decode_file(File *in, int size, File *out,int info, FMode mode, uint64_t &diffFound, int type) {
    assert(vTypes[type].ensize!=-1);
    File *e; // for compare
    if (mode==FCOMPARE ){
       e=new FileTmp();
       vmDecode[type]->outFile=e;
    } else {
        vmDecode[type]->outFile=out;
    }
    in->setpos(0);
    vmDecode[type]->inFile=in;
    int jst=vmDecode[type]->decode(info,size);
    if (mode==FCOMPARE ){
       int outsize=(U32)e->curpos();
       e->setpos(0);
       for (int i=0; i<jst; i++) {
           if (e->getc()!=out->getc() && !diffFound) diffFound=i;   
       }
       return (U32)e->curpos();
    }
    if (mode==FDECOMPRESS) {
        return jst;
    }    
    return size;
}

// Print progress: n is the number of bytes compressed or decompressed
void printStatus(U64 n, U64 size,int tid=-1) {
if (level>0 && tid>=0)  fprintf(stderr,"%2d %6.2f%%\b\b\b\b\b\b\b\b\b\b",tid, float(100)*n/(size+1)), fflush(stdout);
else if (level>0)  fprintf(stderr,"%6.2f%%\b\b\b\b\b\b\b", float(100)*n/(size+1)), fflush(stdout);
}

//////////////////// Compress, Decompress ////////////////////////////

//for block statistics, levels 0-5
U64 typenamess[datatypecount][5]={0}; //total type size for levels 0-5
U32 typenamesc[datatypecount][5]={0}; //total type count for levels 0-5
int itcount=0;               //level count

int getstreamid(int type){
    if (type<vTypes.size())return vTypes[type].streamId;//return typet[type][STREAM];
    
    return -1;
}

bool isstreamtype(int type,int streamid){
   // assert(streamid<streamCount);
   // assert(type<TYPELAST);
    if (type<vTypes.size() && vTypes[type].streamId==streamid) return true;
    return false;
}

void direct_encode_blockstream(int type, File*in, U64 len, Encoder &en, U64 s1, U64 s2, int info=0) {
  assert(s1<(s1+len));
  segment[segment.pos++]=type&0xff;
  segment.put8(len);
  segment.put4(info);
  int srid=getstreamid(type);
  for (U64 j=s1; j<s1+len; ++j) filestreams[srid]->putc(in->getc());
}

void DetectRecursive(File*in, U64 n, Encoder &en, char *blstr, int it, U64 s1, U64 s2);

void transform_encode_block(int type, File*in, U64 len, Encoder &en, int info, int info2, char *blstr, int it, U64 s1, U64 s2, U64 begin) {
    //encode data if type has encode defined
    if (vTypes[type].type!=defaultType && vTypes[type].ensize!=-1 ) { // skip if encode data is missing
        U64 diffFound=0;
        FileTmp* tmp;
        tmp=new FileTmp;
        encode_file(in, tmp, int(len), info==-1?(U32)begin:info,type);
        const U64 tmpsize= tmp->curpos();
        int tfail=0;
        tmp->setpos(0);
        en.setFile(tmp);
        int ts=0;
        if ( vTypes[type].type>=defaultType){
            in->setpos(begin);
            decode_file(en, int(tmpsize), in, info==-1?(U32)begin:info, FCOMPARE, diffFound,type);
        }else{
            in->setpos(begin);
            decode_file(tmp, int(tmpsize), in, info==-1?(U32)begin:info, FCOMPARE, diffFound,type);
        }
      tfail=(diffFound || tmp->getc()!=EOF || ts ); 
        // Test fails, compress without transform
        if (tfail) {
            printf(" Transform fails at %0lu, skipping...\n", diffFound-1);
             in->setpos(begin);
            direct_encode_blockstream(defaultType, in, len, en, s1, s2);
            typenamess[type][it]-=len,  typenamesc[type][it]--;       // if type fails set
            typenamess[defaultType][it]+=len,  typenamesc[defaultType][it]++; // default info
        } else {
            tmp->setpos(0);
            vTypes[type].used=1;
            if (vTypes[type].type>=defaultType ){
            
            direct_encode_blockstream(type, tmp, tmpsize, en, s1, s2, info==-1?(U32)begin:info);
            } else if (vTypes[type].type<defaultType) { // recursive
                segment.put1(type);
                segment.put8(tmpsize);
                segment.put4(info==-1?(U32)begin:info);
				if (info>0){ 
					// not really, split header and data
					// add header to defaultType and direct encode data
					//tmp->setpos(0);
					int hdrsize=( tmp->getc()<<8)+(tmp->getc()); // must be present in encoded file
					tmp->setpos(0);
					typenamess[defaultType][it]+=hdrsize,  typenamesc[defaultType][it]++; 
					direct_encode_blockstream(defaultType, tmp, hdrsize, en,0, s2);
					// process data
					typenamess[type][it]+=tmpsize,  typenamesc[type][it]++;
					direct_encode_blockstream(type, tmp, tmpsize-hdrsize, en, s1, s2, info);
				}else{
					// do recursion
					DetectRecursive( tmp, tmpsize, en, blstr,it+1, 0, tmpsize);//it+1
				}
                tmp->close();
                return;
            }         
        }
        tmp->close();
    } else {         
            const int i1=info;
            direct_encode_blockstream(type, in, len, en, s1, s2, i1);
         
    }    
}

void DetectRecursive(File*in, U64 n, Encoder &en, char *blstr, int it=0, U64 s1=0, U64 s2=0) {
  //static const char* audiotypes[6]={"8b mono","8b stereo","16b mono","16b stereo","32b mono","32b stereo"};
  int type=defaultType;
  int blnum=0, info=-1,info2;  // image width or audio type
  U64 begin= in->curpos(), end0=begin+n;
  char b2[32];
  strcpy(b2, blstr);
  if (b2[0]) strcat(b2, "-");
  if (it==5) {
    direct_encode_blockstream(defaultType, in, n, en, s1, s2);
    return;
  }
  s2+=n;

  // Transform and test in blocks
  while (n>0) {
    int nextType=detect(in, n, type, info,info2,it,s1);
    U64 end= in->curpos();
     in->setpos( begin);
    if (end>end0) {  // if some detection reports longer then actual size file is
      end=begin+1;
      type=defaultType;
    }
    U64 len=U64(end-begin);
    if (begin>end) len=0;
    if (len>=2147483646) {  // fix me, len is int, must be U32  or do not allow larger then +int block size
      len=2147483646;
      type=defaultType;
    }
    if (len>0) {
    if (it>itcount)    itcount=it;

    typenamess[type][it]+=len,  typenamesc[type][it]++; 
      sprintf(blstr,"%s%d",b2,blnum++);
      
      printf(" %-11s | %-9s |%10.0d [%d - %d]",blstr,type==defaultType?"default":vTypes[type].detect,(U32)len,(U32)begin,(U32)end-1);
      printf("\n");
      transform_encode_block(type, in, len, en, info,info2, blstr, it, s1, s2, begin);
      
      s1+=len;
      n-=len;
    }
    
    type=nextType;
    begin=end;
  }
}

// Compress a file. Split filesize bytes into blocks by type.
// For each block, output
// <type> <size> and call encode_X to convert to type X.
// Test transform and compress.
void DetectStreams(const char* filename, U64 filesize) {
  FileTmp tmp;
  Encoder en(COMPRESS, &tmp,0);
  assert(en.getMode()==COMPRESS);
  assert(filename && filename[0]);
  FileDisk in;
  in.open(filename,true);
  printf("Block segmentation:\n");
  char blstr[32]="";
  DetectRecursive(&in, filesize, en, blstr);
  in.close();
  tmp.close();
}

U64 decompressStreamRecursive(File*out, U64 size, Encoder& en, FMode mode, int it=0, U64 s1=0, U64 s2=0) {
    int type;
    U64 len=0L, i=0L;
    U64 diffFound=0L;
    int info=-1;
    s2+=size;
    while (i<size) {
        type=segment(segment.pos++);
        for (int k=0; k<8; k++) len=len<<8,len+=segment(segment.pos++);
        for (int k=info=0; k<4; ++k) info=(info<<8)+segment(segment.pos++);
        int srid=getstreamid(type);
        if (srid>=0) en.setFile(filestreams[srid]);
        // deocode file if type has decode defined
        if (vTypes[type].type>=defaultType && vTypes[type].desize!=-1) {
            len=decode_file(en, int(len), out, info, mode, diffFound,type);
        } else if (vTypes[type].type<defaultType) {
            File  *tmp;
            tmp = new FileTmp();
            decompressStreamRecursive(tmp, len, en, FDECOMPRESS, it+1, s1+i, s2-len);
            tmp->setpos(0);
            len=decode_file(tmp, int(len), out, info, mode, diffFound,type);
            tmp->close();
        }
        else {
            for (U64 j=i+s1; j<i+s1+len; ++j) {
                if (!(j&0x1fff)) printStatus(j, s2);
                if (mode==FDECOMPRESS) out->putc(en.decompress());
                else if (mode==FCOMPARE) {
                    int a=out->getc();
                    int b=en.decompress();
                    if (a!=b && !diffFound) {
                        mode=FDISCARD;
                        diffFound=j+1;
                        printf("Diff found: %d",diffFound);
                        quit("");
                    }
                } else en.decompress();
            }
        }
        i+=len;
    }
    return diffFound;
}

// Decompress a file from datastream
void DecodeStreams(const char* filename, U64 filesize) {
  FMode mode=FDECOMPRESS;
  assert(filename && filename[0]);
  FileTmp  tmp;
  Encoder en(COMPRESS, &tmp,0);
  // Test if output file exists.  If so, then compare.
  FileDisk f;
  bool success=f.open(filename,true);
  if (success) mode=FCOMPARE,printf("Comparing");
  else {
    // Create file
    f.create(filename);
    mode=FDECOMPRESS, printf("Extracting");
  }
  printf(" %s %0lu -> \n", filename, filesize);

  // Decompress/Compare
  U64 r=decompressStreamRecursive(&f, filesize, en, mode);
  if (mode==FCOMPARE && !r && f.getc()!=EOF) printf("file is longer\n");
  else if (mode==FCOMPARE && r) printf("differ at %0lu\n",r-1);
  else if (mode==FCOMPARE) printf("identical\n");
  else printf("done   \n");
  f.close();
  tmp.close();
}

//////////////////////////// User Interface ////////////////////////////


// int expand(String& archive, String& s, const char* fname, int base) {
// Given file name fname, print its length and base name (beginning
// at fname+base) to archive in format "%ld\t%s\r\n" and append the
// full name (including path) to String s in format "%s\n".  If fname
// is a directory then substitute all of its regular files and recursively
// expand any subdirectories.  Base initially points to the first
// character after the last / in fname, but in subdirectories includes
// the path from the topmost directory.  Return the number of files
// whose names are appended to s and archive.

// Same as expand() except fname is an ordinary file
int putsize(String& archive, String& s, const char* fname, int base) {
  int result=0;
  FileDisk f;
  bool success=f.open(fname,true);
  if (success) {
    f.setend();
    U64 len=f.curpos();
    if (len>=0) {
      static char blk[24];
      sprintf(blk, "%0.0f\t", len+0.0);
      archive+=blk;
      archive+=(fname+base);
      archive+="\n";
      s+=fname;
      s+="\n";
      ++result;
    }
    f.close();
  }
  return result;
}

#ifdef WINDOWS

int expand(String& archive, String& s, const char* fname, int base) {
  int result=0;
  DWORD attr=GetFileAttributes(fname);
  if ((attr != 0xFFFFFFFF) && (attr & FILE_ATTRIBUTE_DIRECTORY)) {
    WIN32_FIND_DATA ffd;
    String fdir(fname);
    fdir+="/*";
    HANDLE h=FindFirstFile(fdir.c_str(), &ffd);
    while (h!=INVALID_HANDLE_VALUE) {
      if (!equals(ffd.cFileName, ".") && !equals(ffd.cFileName, "..")) {
        String d(fname);
        d+="/";
        d+=ffd.cFileName;
        result+=expand(archive, s, d.c_str(), base);
      }
      if (FindNextFile(h, &ffd)!=TRUE) break;
    }
    FindClose(h);
  }
  else // ordinary file
    result=putsize(archive, s, fname, base);
  return result;
}

#else
#ifdef UNIX

int expand(String& archive, String& s, const char* fname, int base) {
  int result=0;
  struct stat sb;
  if (stat(fname, &sb)<0) return 0;

  // If a regular file and readable, get file size
  if (sb.st_mode & S_IFREG && sb.st_mode & 0400)
    result+=putsize(archive, s, fname, base);

  // If a directory with read and execute permission, traverse it
  else if (sb.st_mode & S_IFDIR && sb.st_mode & 0400 && sb.st_mode & 0100) {
    DIR *dirp=opendir(fname);
    if (!dirp) {
      perror("opendir");
      return result;
    }
    dirent *dp;
    while(errno=0, (dp=readdir(dirp))!=0) {
      if (!equals(dp->d_name, ".") && !equals(dp->d_name, "..")) {
        String d(fname);
        d+="/";
        d+=dp->d_name;
        result+=expand(archive, s, d.c_str(), base);
      }
    }
    if (errno) perror("readdir");
    closedir(dirp);
  }
  else printf("%s is not a readable file or directory\n", fname);
  return result;
}

#else  // Not WINDOWS or UNIX, ignore directories

int expand(String& archive, String& s, const char* fname, int base) {
  return putsize(archive, s, fname, base);
}

#endif
#endif
char *dmodel;
Array<U64> filestreamsize(0);
static char   pp[] ="int t[5]={};"
"enum {SMC=1,APM1,DS,AVG,SCM,RCM,CM,MX,ST,MM,DHS};"
"int update(int y,int c0,int bpos,int c4,int pr){ int i;"
" if (bpos==0) {for (i=4; i>0; --i) t[i]=h2(t[i-1],c4&0xff);}"
" for (i=1;i<5;++i) vmx(DS,0,(c0)|(t[i]<<8));"
" vmx(APM1,0,c0); return 0;}"
"void block(int a,int b){} int main(){ vms(0,1,1,3,0,0,0,0,0,0,0);"
" vmi(DS,0,18,1023,4); vmi(AVG,0,0,0,1);"
" vmi(AVG,1,0,2,3); vmi(AVG,2,0,4,5); vmi(APM1,0,256,7,6);}";
void compressStream(int streamid,U64 size, File* in, File* out) {
    int i; //stream
    i=streamid;
    Encoder* threadencode;
    U64 datasegmentsize;
    U64 datasegmentlen=0;
    int datasegmentpos=0;
    int datasegmentinfo=0;
    //U64 scompsize=0;
    U32 currentpos;
    int modelSize=0;
    int modelSizeCompressed=0;
    currentpos=(U32)out->curpos();
    U8 *p;
    datasegmentsize=size;
    // datastreams
    FILE *moin;
    moin=0; 
    p=0;
    if (level>0){
        moin=fopen(vStreams[i].model, "rb");
        if(moin){
            File *modelo;//open tmp file for compressed config file
            modelo= new FileTmp();
            Encoder* enm;
            enm=new Encoder(COMPRESS, modelo,pp);
            enm->predictor->set();
            
            fseek ( moin , 0 , SEEK_END );
            int fsz=ftello(moin); 
            modelSize=fsz;
            assert(fsz>0);
            fseek ( moin , 0 , SEEK_SET );
            //compress model file
            enm->compress(fsz>>24); enm->compress(fsz>>16); enm->compress(fsz>>8); enm->compress(fsz); // config file length
            for (int k=0;k<fsz;++k) enm->compress(getc(moin));
            enm->flush();
            delete enm;

            fsz=modelo->curpos();
            modelSizeCompressed=fsz;
            modelo->setpos(0);
            p = (U8 *)calloc(fsz+1,1); 
            modelo->blockread(p,fsz);
            p[fsz] = 0;
            out->blockwrite(&p[0],fsz);
            //read again model file
            fseek ( moin , 0 , SEEK_END );
            fsz=ftello(moin); 
            fseek ( moin , 0 , SEEK_SET );
            free(p);
            //read config file for compression
            p = (U8 *)calloc(fsz+1,1); 
            fread( p, 1,fsz,moin); 
            p[fsz] = 0;
            //close compressed and uncomressed model files
            fclose(moin); 
            modelo->close();// fclose();
        }
        else quit("Config file not found.");        
    }

    printf("Compressing %s   stream(%d).  Total %d\n",vStreams[i].model,i,(U32)datasegmentsize); 
    threadencode=new Encoder (COMPRESS, out,(char *)p); 
    
    while (datasegmentsize>0) {
        while (datasegmentlen==0){
            int datasegmenttype=segment(datasegmentpos++);
            for (int ii=0; ii<8; ii++) datasegmentlen<<=8,datasegmentlen+=segment(datasegmentpos++);
            for (int ii=0; ii<4; ii++) datasegmentinfo=(datasegmentinfo<<8)+segment(datasegmentpos++);
            if (vTypes[datasegmenttype].type<defaultType || !(isstreamtype(datasegmenttype,i)))datasegmentlen=0;
            if (level>0){
                threadencode->predictor->x.filetype=datasegmenttype;
                threadencode->predictor->x.blpos=0;
                threadencode->predictor->x.finfo=datasegmentinfo;
                threadencode->predictor->set();
                threadencode->predictor->setdebug(0);
            }
        }
        for (U64 k=0; k<datasegmentlen; ++k) {
            if (!(datasegmentsize&0x1fff)) printStatus(size-datasegmentsize, size,i);
            threadencode->compress(in->getc());
            datasegmentsize--;
        }
        datasegmentlen=0;
    }
    threadencode->flush();
    
    delete threadencode;
    printf("Stream(%d) compressed from %d to %d bytes\n",i,(U32)size, ((U32)out->curpos()-(U32)currentpos)-modelSizeCompressed);
    printf("    Model compressed from %d to %d bytes\n",modelSize, modelSizeCompressed);
}

#ifdef MT
//multithreading code from pzpaq.cpp v0.05
#ifdef PTHREAD
pthread_cond_t cv=PTHREAD_COND_INITIALIZER;  // to signal FINISHED
pthread_mutex_t mutex=PTHREAD_MUTEX_INITIALIZER; // protects cv
typedef pthread_t pthread_tx;
#else
HANDLE mutex;  // protects Job::state
typedef HANDLE pthread_tx;
#endif



typedef enum {READY, RUNNING, FINISHED_ERR, FINISHED, ERR, OK} State;
// Instructions to thread to compress or decompress one block.
struct Job {
  State state;        // job state, protected by mutex
  int id;
  int streamid;
  U64 datasegmentsize;
  int command;
  File*in;
  File*out;
  pthread_tx tid;      // thread ID (for scheduler)
  Job();
  void print(int i) const;
};

// Initialize
Job::Job(): state(READY),id(0),streamid(-1),datasegmentsize(0),command(-1) {
  // tid is not initialized until state==RUNNING
}

// Print contents
void Job::print(int i=0) const {
  fprintf(stderr,
      "Job %d: state=%d stream=%d\n", i, state,streamid);
}
bool append(File* out, File* in) {
  if (!in) {
    quit("append in error\n");
    return false;
  }
  if (!out) {
    quit("append out error\n");
    return false;
  }
  const int BUFSIZE=4096*64;
  U8 buf[BUFSIZE];
  int n;
  while ((n=in->blockread(buf, BUFSIZE ))>0)
    out->blockwrite(buf,   n  );
  return true;
}

void decompress(const Job& job) {
}        

#define check(f) { \
  int rc=f; \
  if (rc) fprintf(stderr, "Line %d: %s: error %d\n", __LINE__, #f, rc); \
}
// Worker thread
#ifdef PTHREAD
void*
#else
DWORD
#endif
thread(void *arg) {

  // Do the work and receive status in msg
  Job* job=(Job*)arg;
  const char* result=0;  // error message unless OK
  try {
    if (job->command==0) 
      compressStream(job->streamid,job->datasegmentsize,job->in,job->out);
    else if (job->command==1)
      decompress(*job); 
  }
  catch (const char* msg) {
    result=msg;
  }
// Call f and check that the return code is 0

  // Let controlling thread know we're done and the result
#ifdef PTHREAD
  check(pthread_mutex_lock(&mutex));
  job->state=result?FINISHED_ERR:FINISHED;
  check(pthread_cond_signal(&cv));
  check(pthread_mutex_unlock(&mutex));
#else
  WaitForSingleObject(mutex, INFINITE);
  job->state=result?FINISHED_ERR:FINISHED;
  ReleaseMutex(mutex);
#endif
  return 0;
}
#endif

// read global config file conf.pxv
void readConfigFile(FILE *fp){ 
    char str[60];
    int result;//, findNL;
    int ssize=-1,tsize=-1; // stream index
    /* opening file for reading */
    if(fp == NULL) quit("Error opening conf.pxv file\n");
    while (fgets (str, 60, fp)!=NULL){   
        // remove comment
        if (str[0]=='/' || str[0]=='\r' || str[0]=='\n') continue;
        char *ptr = strtok(str," ");
        if (ptr == NULL)  quit("bad config: bad line");
        //first parameter
        //printf("%s ", ptr);
        // read stream data
        // will fail if out of order stream <> model or type <> parameters
        result = strcmp(ptr, "stream");
        if (result==0){
            ssize=vStreams.size();
            vStreams.resize(ssize+1);
            // find next parameter
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config: stream id not found"); 
            int sid=atoi(ptr);
            if (ssize!=sid) quit("bad config: stream id must be in order"); 
            if (sid<0) quit("bad config: stream id not >=0"); 
            vStreams[ssize].stream=sid;
            continue;
        }
        result = strcmp(ptr, "model");
        if (result==0 && ssize>=0){
            ptr = strtok(NULL, " \t\n\r");
            if (ptr == NULL)  quit("bad config: model file name not found"); 
            int fsize=strlen(ptr);
            if (  fsize >15 ||fsize==0) quit("bad config: model filename > 15 0");
            strcpy(vStreams[ssize].model,ptr);
           // printf("stream id=%d model=%s \n",vStreams[ssize].stream,vStreams[ssize].model);
            continue;
        }
        //read type data
        result = strcmp(ptr, "type");
        if (result==0){
            tsize=vTypes.size();
            vTypes.resize(tsize+1);
            // find next parameter
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config: type id not found"); 
            int sid=atoi(ptr);
            vTypes[tsize].type=sid;
            continue;
        }
        result = strcmp(ptr, "detect");
        if (result==0 && tsize>=0){
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config:   file name not found"); 
            int fsize=strlen(ptr);
            if (  fsize >15 ||fsize==0) quit("bad config:   filename > 15 0");
            int sid=atoi(ptr);
            if (sid==-1){
                vTypes[tsize].dsize=sid;        // set decode -1
               // printf("type id=%d no detect model (%d)\n",vTypes[tsize].type,vTypes[tsize].dsize);
            }else{ 
                strcpy(vTypes[tsize].detect,ptr); // copy config file name
             //   printf("type id=%d model=%s \n",vTypes[tsize].type,vTypes[tsize].detect);
            }
            continue;
        }
        result = strcmp(ptr, "encode");
        if (result==0 && tsize>=0){
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config:   file name not found"); 
            int fsize=strlen(ptr);
            if (  fsize >15 ||fsize==0) quit("bad config:   filename > 15 0");
            int sid=atoi(ptr);
            if (sid==-1){
                vTypes[tsize].ensize=sid;        // set decode -1
              //  printf("type id=%d no encode model\n",vTypes[tsize].type);
            }else{ 
                if ( vTypes[tsize].desize==-1) quit("bad config:encode   type d/encode not defined");
                strcpy(vTypes[tsize].encode,ptr); // copy config file name
               // printf("type id=%d model=%s \n",vTypes[tsize].type,vTypes[tsize].encode);
            }
            continue;
        }
        result = strcmp(ptr, "decode");
        if (result==0 && tsize>=0){
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config:   file name not found"); 
            int fsize=strlen(ptr);
            if (  fsize >15 ||fsize==0) quit("bad config:   filename > 15 0");
            int sid=atoi(ptr);
            if (sid==-1){
                vTypes[tsize].desize=sid;        // set decode -1
             //   printf("type id=%d no decode model\n",vTypes[tsize].type);
            }
            else{ 
            if ( vTypes[tsize].ensize==-1) quit("bad config:decode   type d/encode not defined");
                strcpy(vTypes[tsize].decode,ptr); // copy config file name
               // printf("type id=%d model=%s \n",vTypes[tsize].type,vTypes[tsize].decode);
            }
            continue;
        }
        result = strcmp(ptr, "compress");
        if (result==0 && tsize>=0){
            ptr = strtok(NULL, " \t\n\r/");
            if (ptr == NULL)  quit("bad config:   wrong stream id"); 
            int fsize=strlen(ptr);
            if (  fsize >15 ||fsize==0 || ptr[0]>'9') quit("bad config:   compress parameter wrong");
            int sid=atoi(ptr);
            vTypes[tsize].streamId=sid;        // set type model for compression
           // printf("type id=%d stream id=%d\n",vTypes[tsize].type,vTypes[tsize].streamId);
            continue;
        }
        printf("Bad line %s \n",ptr);
    }
    fclose(fp);
    // printf("Total streams %d, total types %d\n",(U32)vStreams.size(),(U32)vTypes.size());
    //check if type -> stream is present
    //mark stream enabled
    for (int i=0; i<(int)vTypes.size();i++){
        vTypes[i].used=0;
        int sidt=vTypes[i].streamId;
        for (int j=0; j<(int)vStreams.size();j++){          
            if (sidt==vStreams[j].stream){               
               vStreams[j].enabled=1;
               break;
            } 
        }
    }
    
    /*for (int j=0; j<(int)vStreams.size();j++){  
        printf("Stream %d model %s ",j,vStreams[j].model);        
        if (vStreams[j].enabled==1)  printf("enabled\n");
        else printf("disabled\n");
    }*/
    //create temporary files for streams
    streamCount=(int)vStreams.size();
    if (streamCount>16) quit("Max 16 streams allowed.");
    filestreams = new File*[streamCount];
    for (int i=0;i<streamCount;i++) filestreams[i]= new FileTmp();
    filestreamsize.resize(streamCount);
    //quit("end config");
}   

void createDetectVM(){
   // FILE *f;
    char *detectModel;
    vmDetect = new VM*[vTypes.size()];
    for (int i=0;i<vTypes.size();i++){
        if ( vTypes[i].dsize!=-1){
            //open type detection file and load into memory
            //printf("File %s\n",vTypes[i].detect);
            FILE *f=fopen(vTypes[i].detect,"rb");
            if(f==NULL) quit("Error opening detect file\n");
            fseeko(f, 0, SEEK_END);
            U32 size=(U32)ftello(f);
            if (size<97) quit("Input model to small\n");
            fseeko(f, 0, SEEK_SET);        
            detectModel = (char *)calloc(size+1,1);
            fread( detectModel, 1,size,f);  
            fclose(f);
            //cread VM for type
            vmDetect[i]= new VM(detectModel,  z,VMDETECT);
            free(detectModel);
        }
    }
}
void createEncodeVM(){
    //FILE *f;
    char *encodeModel;
    vmEncode = new VM*[vTypes.size()];
    for (int i=0;i<vTypes.size();i++){
        if ( vTypes[i].ensize!=-1){
            //open type encode file and load into memory
            //printf("File %s\n",vTypes[i].encode);
            FILE *f=fopen(vTypes[i].encode,"rb");
            if(f==NULL) quit("Error opening encode file\n");
            fseeko(f, 0, SEEK_END);
            U32 size=(U32)ftello(f);
            if (size<97) quit("Input model to small\n");
            fseeko(f, 0, SEEK_SET);        
            encodeModel = (char *)calloc(size+1,1);
            fread( encodeModel, 1,size,f);  
            fclose(f);
            //cread VM for type
            vmEncode[i]= new VM(encodeModel,  z,VMENCODE);
            free(encodeModel);
        }
    }
}

void createDecodeVM(){
    //FILE *f;
    char *decodeModel;
    vmDecode = new VM*[vTypes.size()];
    for (int i=0;i<vTypes.size();i++){
        if ( vTypes[i].desize!=-1){
            //printf("File %s\n",vTypes[i].decode);
            //open type decode file and load into memory
            FILE *f=fopen(vTypes[i].decode,"rb");
            if(f==NULL) quit("Error opening decode file\n");
            fseeko(f, 0, SEEK_END);
            U32 size=(U32)ftello(f);
            if (size<97) quit("Input model to small\n");
            fseeko(f, 0, SEEK_SET);        
            decodeModel = (char *)calloc(size+1,1);
            fread( decodeModel, 1,size,f);  
            fclose(f);
            //cread VM for type
            vmDecode[i]= new VM(decodeModel,  z,VMDECODE);
            free(decodeModel);
        }
    }
}

void createStreamVM(){
    // create array for Stream VM's
    vmStream = new VM*[vStreams.size()];
    // actual VM is created when compressing or decompressing
}

int getUnknownType(){    
    for (int i=0;i<vTypes.size();i++){
        if ( vTypes[i].dsize==-1) return i; //return index
    }
    //not found
    return -1;
}

// compress all decode models into archive including main config fail
void CompressType(File *out){
    File *modelo;//open tmp file for compressed config file
    Encoder* enm;
    modelo=new FileTmp();
    enm=new Encoder(COMPRESS, modelo,pp);
    enm->predictor->set();
    FILE *in;
    U8 *p;
    int fsz;
    int insize=out->curpos();
    // compress main config file
    in=fopen("conf.pxv", "rb");
    fseek(in,0,SEEK_END);
    fsz=ftello(in); 
    fseek(in,0,SEEK_SET);
    enm->compress(fsz>>24); enm->compress(fsz>>16); enm->compress(fsz>>8); enm->compress(fsz); // config file length
    for (int k=0;k<fsz;++k) enm->compress(getc(in));
    fclose(in); 

    for (int i=0; i<(int)vTypes.size();i++){
        // compress type decode model if it was used in transform_encode_block
        if (vTypes[i].used==1 && vTypes[i].desize!=-1){
            in=fopen(vTypes[i].decode, "rb");
            fseek(in,0,SEEK_END);
            fsz=ftello(in); 
            fseek(in,0,SEEK_SET);
            //compress model file
            enm->compress(fsz>>24); enm->compress(fsz>>16); enm->compress(fsz>>8); enm->compress(fsz); // config file length
            for (int k=0;k<fsz;++k) enm->compress(getc(in));
            fclose(in); 
        } else{
            enm->compress(0); enm->compress(0); enm->compress(0); enm->compress(0); // config file length
        }     
    }
    enm->flush();
    fsz=modelo->curpos();
    modelo->setpos(0);
    p = (U8 *)calloc(fsz+1,1); 
    modelo->blockread(p,fsz);
    out->blockwrite(&p[0],fsz);
    //read again model file
    free(p);
    delete enm;
    modelo->close();
    printf("Decode compressed to : %d\n", (int)out->curpos()-insize);
}
// Decompress any decode function used by type and create decodeVM for it
void DecompressType(File *out){
    char *decodeModel;
    Encoder* enm;
    enm=new Encoder(DECOMPRESS, out,dmodel);
    int len;    
    // decompress config file from archive
    FILE *conf=tmpfile2();
    len=enm->decompress()<<24; //decompress compressed model lenght
    len+=enm->decompress()<<16;
    len+=enm->decompress()<<8;
    len+=enm->decompress();
    for (int k=0;k<len;++k) putc(enm->decompress(),conf); 
    fseek(conf,0,SEEK_SET);
    readConfigFile(conf);
    if ((defaultType=getUnknownType())==-1) quit("Default type not defined (type x detect -1)");
    //decompress type files if present
    vmDecode = new VM*[vTypes.size()];
    for (int i=0; i<(int)vTypes.size();i++){
        len=enm->decompress()<<24; //decompress compressed model lenght
        len+=enm->decompress()<<16;
        len+=enm->decompress()<<8;
        len+=enm->decompress();
        if (len>0){
            //int a=0;
            decodeModel = (char *)calloc(len+1,1);
            for (int k=0;k<len;++k) decodeModel[k]=enm->decompress(); 
            //cread VM for type
            vmDecode[i]= new VM(decodeModel,  z,VMDECODE);
            free(decodeModel);
        } else{
            // no config file
        }
    }
    delete enm;
}
// To compress to file1.paq8pxv: paq8pxv [-n] file1 [file2...]
// To decompress: paq8pxv file1.paq8pxv [output_dir]
int main(int argc, char** argv) {
    bool pause=argc<=2;  // Pause when done?
    try {

        // Get option
        bool doExtract=false;  // -d option
        bool doList=false;  // -l option
        char* aopt;
        aopt=&argv[1][0];
        
#ifdef MT 
        int topt=1;
        if (argc>1 && aopt[0]=='-' && aopt[1]  && strlen(aopt)<=6) {
#else
        if (argc>1 && aopt[0]=='-' && aopt[1]  && strlen(aopt)<=4) {    
#endif
            if (aopt[1]=='d' && !aopt[2])
                doExtract=true;
            else if (aopt[1]=='l' && !aopt[2])
                doList=true;
            else if (aopt[2]>='0' && aopt[2]<='9' && strlen(aopt)==3 && aopt[1]=='s'){
                level=aopt[2]-'0';
            }
#ifdef MT 
            else if (aopt[2]>='0' && aopt[2]<='9'&& (aopt[4]<='9' && aopt[4]>'0') && strlen(aopt)==5 && 
            (aopt[1]=='s')){
                topt=aopt[4]-'0';
                level=aopt[2]-'0';}
#endif
            else
                quit("Valid options are -s0 through -s9, -d, -l\n");
            --argc;
            ++argv;
            pause=false;
        }

        // Print help message quick 
        if (argc<2) {
            printf(PROGNAME " archiver (C) 2019, Matt Mahoney et al.\n"
            "Free under GPL, http://www.gnu.org/licenses/gpl.txt\n");
#ifdef __GNUC__     
            printf("Compiled %s, compiler gcc version %d.%d.%d\n\n",__DATE__, __GNUC__, __GNUC_MINOR__,__GNUC_PATCHLEVEL__);
#endif
#ifdef __clang_major__
            printf("Compiled %s, compiler clang version %d.%d\n\n",__DATE__, __clang_major__, __clang_minor__);
#endif
#ifdef            _MSC_VER 
            printf("Compiled %s, compiler Visual Studio version %d\n\n",__DATE__, _MSC_VER);
#endif
#ifdef VMJIT
            printf("Compiled with VM x86 JIT.\n"); 
#else
            printf("Compiled with VM emulation.\n");
#endif
#ifdef MT
printf("Multithreading enabled with %s.\n",
#ifdef PTHREAD
"PTHREAD"
#else
"windows native threads"
#endif
);

#if defined(__AVX2__)
printf("Compiled with AVX2\n");
#elif defined(__SSE4_1__)   
printf("Compiled with SSE41\n");
#elif  defined(__SSSE3__)
printf("Compiled with SSSE3\n");
#elif defined(__SSE2__) 
printf("Compiled with SSE2\n");
#elif defined(__SSE__)
printf("Compiled with SSE\n");
#else
printf("No vector instrucionts\n");
#endif
#endif
printf("\n");

            printf(
#ifdef WINDOWS
            "To compress or extract, drop a file or folder on the "
            PROGNAME " icon.\n"
            "The output will be put in the same folder as the input.\n"
            "\n"
            "Or from a command window: "
#endif
            "To compress:\n"
            "  " PROGNAME " -slevel file               (compresses to file." PROGNAME ")\n"
            "  " PROGNAME " -slevel archive files...   (creates archive." PROGNAME ")\n"
            "  " PROGNAME " file                       (level -%d pause when done)\n"
            "level: -s0          store\n"
            "  -s1...-s3         (uses 393, 398, 409 MB)\n"
            "  -s4...-s9         (uses 1.2  1.3  1.5  1.9 2.7 4.9 GB)\n"
            "  -s10...-s15       (uses 7.0  9.0 11.1 27.0   x.x x.x GB)\n"
#ifdef MT 
            "  to use multithreading -level:threads (1-9, compression only)\n"
            "  " PROGNAME " -s4:2 file (use level 4 threads 2)\n\n"
#endif            
#if defined(WINDOWS) || defined (UNIX)
            "You may also compress directories.\n"
#endif
            "\n"
            "To extract or compare:\n"
            "  " PROGNAME " -d dir1/archive." PROGNAME "      (extract to dir1)\n"
            "  " PROGNAME " -d dir1/archive." PROGNAME " dir2 (extract to dir2)\n"
            "  " PROGNAME " archive." PROGNAME "              (extract, pause when done)\n"
            "\n"
            "To view contents: " PROGNAME " -l archive." PROGNAME "\n"
            "\n",
            DEFAULT_OPTION);
            quit();
        }
       
        File* archive=0;               // compressed file
        int files=0;                   // number of files to compress/decompress
        Array<const char*> fname(1);   // file names (resized to files)
        Array<U64> fsize(1);           // file lengths (resized to files)
        U16 streambit=0;               //bit is set if stream has size, 11-0
        // Compress or decompress?  Get archive name
        Mode mode=COMPRESS;
        String archiveName(argv[1]);
        {
            const int prognamesize=strlen(PROGNAME);
            const int arg1size=strlen(argv[1]);
            if (arg1size>prognamesize+1 && argv[1][arg1size-prognamesize-1]=='.'
                    && equals(PROGNAME, argv[1]+arg1size-prognamesize)) {
                mode=DECOMPRESS;
            }
            else if (doExtract || doList)
            mode=DECOMPRESS;
            else {
                archiveName+=".";
                archiveName+=PROGNAME;
            }
        }
        if (mode==COMPRESS){
           FILE *conf;
           conf = fopen("conf.pxv" , "rb");
           readConfigFile(conf);
           if ((defaultType=getUnknownType())==-1) quit("Default type not defined (type x detect -1)"); //
           createDetectVM();
           createEncodeVM();
           createDecodeVM();  // stored in archive, at header
           createStreamVM();  // stored in archive, on file per stream
        }
        // Compress: write archive header, get file names and sizes
        String header_string;
        String filenames;
        
        if (mode==COMPRESS) {
            segment.setsize(48); //inital segment buffer size (about 277 blocks)
            // Expand filenames to read later.  Write their base names and sizes
            // to archive.
            int i;
            for (i=1; i<argc; ++i) {
                String name(argv[i]);
                int len=name.size()-1;
                for (int j=0; j<=len; ++j)  // change \ to /
                if (name[j]=='\\') name[j]='/';
                while (len>0 && name[len-1]=='/')  // remove trailing /
                name[--len]=0;
                int base=len-1;
                while (base>=0 && name[base]!='/') --base;  // find last /
                ++base;
                if (base==0 && len>=2 && name[1]==':') base=2;  // chop "C:"
                int expanded=expand(header_string, filenames, name.c_str(), base);
                if (!expanded && (i>1||argc==2))
                printf("%s: not found, skipping...\n", name.c_str());
                files+=expanded;
            }

            // If there is at least one file to compress
            // then create the archive header.
            if (files<1) quit("Nothing to compress\n");
            archive=new FileDisk();
            archive->create(archiveName.c_str());
            archive->append(PROGNAME);
            archive->putc(0);
            archive->putc(level);
            archive->putc(streamCount);
            // store small model uncompressed to archive, used when decompressing
            int modsize=strlen(pp);
            archive->put32(modsize);
            //printf("Small model: %d bytes.\n",modsize);
            for (int k=0;k<modsize;++k) archive->putc(pp[k]);
            segment.hpos= archive->curpos();
            
            for (int i=0; i<12+4+2; i++) archive->putc(0); //space for segment size in header +streams info
            
            printf("Creating archive %s with %d file(s)...\n",
            archiveName.c_str(), files);
        }

        // Decompress: open archive for reading and store file names and sizes
        if (mode==DECOMPRESS) {
            archive= new FileDisk();
            archive->open(archiveName.c_str(),true);
            // Check for proper format and get option
            String header;
            int len=strlen(PROGNAME)+1, c, i=0;
            header.resize(len+1);
            while (i<len && (c=archive->getc())!=EOF) {
                header[i]=c;
                i++;
            }
            header[i]=0;
            if (strncmp(header.c_str(), PROGNAME "\0", strlen(PROGNAME)+1))
            printf("%s: not a %s file\n", archiveName.c_str(), PROGNAME), quit();
            level=archive->getc();
            
            level=level&0xf;
            streamCount=archive->getc();
            //read small model.
            int modsize=archive->get32();
            dmodel = (char *)calloc(modsize+1,1);
            for (int k=0;k<modsize;++k) dmodel[k]=archive->getc(); 
            
            filestreams = new File*[streamCount];
            for (int i=0;i<streamCount;i++) filestreams[i]= new FileTmp();
            filestreamsize.resize(streamCount);
            // Read segment data from archive end
            U64 currentpos,datapos=0L;
            for (int i=0; i<8; i++) datapos=datapos<<8,datapos+=archive->getc();
            segment.hpos=datapos;
            U32 segpos=archive->get32();  //read segment data size
            segment.pos=archive->get32(); //read segment data size
            streambit=archive->getc()<<8; //get bitinfo of streams present
            streambit+=archive->getc();
            
            if (segment.hpos==0 || segment.pos==0) quit("Segment data not found.");
            segment.setsize(segment.pos);
            currentpos= archive->curpos();
             archive->setpos( segment.hpos); 
            if (archive->blockread( &segment[0],   segment.pos  )<segment.pos) quit("Segment data corrupted.");
            // Decompress segment data 
            Encoder* segencode;
            FileTmp  tmp;
            tmp.blockwrite(&segment[0],   segment.pos  ); 
            tmp.setpos(0); 
            segencode=new Encoder (DECOMPRESS, &tmp ,dmodel); 
            segment.pos=0;
            for (U32 k=0; k<segpos; ++k) {
                 segment.put1( segencode->decompress());
            }
            delete segencode;
            tmp.close();
            //read stream sizes if stream bit is set
            for (int i=0;i<streamCount;i++){
                if ((streambit>>(streamCount-i))&1){
                   for (int j=0; j<8; j++) filestreamsize[i]<<=8,filestreamsize[i]+=archive->getc();
                }
            }
            archive->setpos(currentpos); 
            segment.pos=0; //reset to offset 0
        }
        Encoder* en;
        
       // en->predictor->setdebug(1);
        // Compress header
        if (mode==COMPRESS) {
            en=new Encoder(mode, archive,pp);
            int len=header_string.size();
            printf("\nFile list (%d bytes)\n", len);
            assert(en->getMode()==COMPRESS);
            U64 start=en->size();
            en->compress(0); // block type 0
            en->compress(len>>24); en->compress(len>>16); en->compress(len>>8); en->compress(len); // block length
            for (int i=0; i<len; i++) en->compress(header_string[i]);
            printf("Compressed from %d to %d bytes.\n",len,(U32)en->size()-start);
        }

        // Deompress header
        if (mode==DECOMPRESS) {
            en=new Encoder(mode, archive,dmodel);
            if (en->decompress()!=0) printf("%s: header corrupted\n", archiveName.c_str()), quit();
            int len=0;
            len+=en->decompress()<<24;
            len+=en->decompress()<<16;
            len+=en->decompress()<<8;
            len+=en->decompress();
            header_string.resize(len);
            for (int i=0; i<len; i++) {
                header_string[i]=en->decompress();
                if (header_string[i]=='\n') files++;
            }
            if (doList) printf("File list of %s archive:\n%s", archiveName.c_str(), header_string.c_str());
        }
        
        // Fill fname[files], fsize[files] with input filenames and sizes
        fname.resize(files);
        fsize.resize(files);
        char *p=&header_string[0];
        char* q=&filenames[0];
        for (int i=0; i<files; ++i) {
            assert(p);
            fsize[i]=atoll(p);
            assert(fsize[i]>=0);
            while (*p!='\t') ++p; *(p++)='\0';
            fname[i]=mode==COMPRESS?q:p;
            while (*p!='\n') ++p; *(p++)='\0';
            if (mode==COMPRESS) { while (*q!='\n') ++q; *(q++)='\0'; }
        }
        // Compress or decompress files
        assert(fname.size()==files);
        assert(fsize.size()==files);
        U64 total_size=0;  // sum of file sizes
        for (int i=0; i<files; ++i) total_size+=fsize[i];
        if (mode==COMPRESS) {
            en->flush();
            delete en;
            for (int i=0; i<files; ++i) {
                printf("\n%d/%d  Filename: %s (%d bytes)\n", i+1, files, fname[i], (U32)fsize[i]);
                DetectStreams(fname[i], fsize[i]);
            }
            segment.put1(0xff); //end marker
            printf("\n Segment data size: %d bytes\n",segment.pos);
            // delete detect vm
            for (int i=0;i<vTypes.size();i++){
                if ( vTypes[i].dsize!=-1)      delete vmDetect[i];
                if ( vTypes[i].ensize!=-1)      delete vmEncode[i];
                 if ( vTypes[i].desize!=-1)      delete vmDecode[i];
            }
            delete[] vmDetect;
            delete[] vmEncode;
            delete[] vmDecode;
            //Display Level statistics
            U32 ttc;
            U64 tts;
            for (int j=0; j<=itcount; ++j) {
                printf("\n %-2s |%-9s |%-10s |%-10s\n","TN","Type name", "Count","Total size");
                printf("-----------------------------------------\n");
                ttc=0,tts=0;
                for (int i=0; i<datatypecount; ++i)   if (typenamess[i][j]) printf(" %2d |%-9s |%10d |%10d\n",i,i==defaultType?"default":vTypes[i].detect, typenamesc[i][j],(U32)typenamess[i][j]),ttc+=typenamesc[i][j],tts+=typenamess[i][j];
                printf("-----------------------------------------\n");
                printf("%-13s%1d |%10d |%10d\n\n","Total level",j, ttc,(U32)tts);
            }
            CompressType(archive);
            
#ifdef MT
            File **filesmt;
            filesmt = new File*[streamCount];
            for (int i=0;i<streamCount;i++) filesmt[i]= new FileTmp();
            std::vector<Job> jobs;
#endif
            for (int i=0; i<streamCount; ++i) {
                U64 datasegmentsize;
                datasegmentsize= filestreams[i]->curpos();    //get segment data offset
                filestreamsize[i]=datasegmentsize;
                 filestreams[i]->setpos( 0);
                streambit=(streambit+(datasegmentsize>0))<<1; //set stream bit if streamsize >0
                if (datasegmentsize>0){                       //if segment contains data
                printf("%s   stream(%d).  Total %d\n",vStreams[i].model,i,(U32)datasegmentsize);  

#ifdef MT
                                                              // add streams to job list
                    filesmt[i]=new FileTmp();                 //open tmp file for stream output
                    Job job;
                    job.out=filesmt[i];
                    job.in=filestreams[i];
                    job.streamid=i;
                    job.command=0; //0 compress
                    job.datasegmentsize=datasegmentsize;
                    jobs.push_back(job);
#else
                    compressStream(i,datasegmentsize,filestreams[i],archive);
#endif
                }
            }

#ifdef MT
  // Loop until all jobs return OK or ERR: start a job whenever one
  // is eligible. If none is eligible then wait for one to finish and
  // try again. If none are eligible and none are running then it is
  // an error.
  int thread_count=0;  // number RUNNING, not to exceed topt
  U32 job_count=0;     // number of jobs with state OK or ERR

  // Aquire lock on jobs[i].state.
  // Threads can access only while waiting on a FINISHED signal.
#ifdef PTHREAD
  pthread_attr_t attr; // thread joinable attribute
  check(pthread_attr_init(&attr));
  check(pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE));
  check(pthread_mutex_lock(&mutex));  // locked
#else
  mutex=CreateMutex(NULL, FALSE, NULL);  // not locked
#endif

  while(job_count<jobs.size()) {

    // If there is more than 1 thread then run the biggest jobs first
    // that satisfies the memory bound. If 1 then take the next ready job
    // that satisfies the bound. If no threads are running, then ignore
    // the memory bound.
    int bi=-1;  // find a job to start
    if (thread_count<topt) {
      for (U32 i=0; i<jobs.size(); ++i) {
        if (jobs[i].state==READY  && bi<0 ) {
          bi=i;
          if (topt==1) break;
        }
      }
    }

    // If found then run it
    if (bi>=0) {
      jobs[bi].state=RUNNING;
      ++thread_count;
#ifdef PTHREAD
      check(pthread_create(&jobs[bi].tid, &attr, thread, &jobs[bi]));
#else
      jobs[bi].tid=CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)thread,
          &jobs[bi], 0, NULL);
#endif
    }

    // If no jobs can start then wait for one to finish
    else {
#ifdef PTHREAD
      check(pthread_cond_wait(&cv, &mutex));  // wait on cv

      // Join any finished threads. Usually that is the one
      // that signaled it, but there may be others.
      for (U32 i=0; i<jobs.size(); ++i) {
        if (jobs[i].state==FINISHED || jobs[i].state==FINISHED_ERR) {
          void* status=0;
          check(pthread_join(jobs[i].tid, &status));
          if (jobs[i].state==FINISHED) jobs[i].state=OK;
          if (jobs[i].state==FINISHED_ERR) quit(" thread error"); //exit program on thread error 
          ++job_count;
          --thread_count;
        }
      }
#else
      // Make a list of running jobs and wait on one to finish
      HANDLE joblist[MAXIMUM_WAIT_OBJECTS];
      int jobptr[MAXIMUM_WAIT_OBJECTS];
      DWORD njobs=0;
      WaitForSingleObject(mutex, INFINITE);
      for (U32 i=0; i<jobs.size() && njobs<MAXIMUM_WAIT_OBJECTS; ++i) {
        if (jobs[i].state==RUNNING || jobs[i].state==FINISHED
            || jobs[i].state==FINISHED_ERR) {
          jobptr[njobs]=i;
          joblist[njobs++]=jobs[i].tid;
        }
      }
      ReleaseMutex(mutex);
      DWORD id=WaitForMultipleObjects(njobs, joblist, FALSE, INFINITE);
      if (id>=WAIT_OBJECT_0 && id<WAIT_OBJECT_0+njobs) {
        id-=WAIT_OBJECT_0;
        id=jobptr[id];
        if (jobs[id].state==FINISHED) jobs[id].state=OK;
        if (jobs[id].state==FINISHED_ERR) quit(" thread error"); //exit program on thread error 
        ++job_count;
        --thread_count;
      }
#endif
    }
  }
#ifdef PTHREAD
  check(pthread_mutex_unlock(&mutex));
#endif

    // Append temporary files to archive if OK.
    for (U32 i=0; i<jobs.size(); ++i) {
        if (jobs[i].state==OK) {
            filesmt[jobs[i].streamid]->setpos( 0);
            //append streams to archive
            const int BLOCK=4096;
            U8 blk[BLOCK];
            bool readdone=false; 
            for (;;) { 
                if (readdone) break;
                int bytesread=filesmt[jobs[i].streamid]->blockread(&blk[0], BLOCK);
                if (bytesread!=BLOCK) {
                    readdone=true;                   
                    archive->blockwrite(&blk[0],  bytesread  );
                } else      
                    archive->blockwrite(&blk[0],  BLOCK  );
            }
            filesmt[jobs[i].streamid]->close();
        }
    }

             #endif
            for (int i=0; i<streamCount; ++i) {
                filestreams[i]->close();
            }
            
            // Write out segment data
            U64 segmentpos;
            segmentpos= archive->curpos();  //get segment data offset
            archive->setpos( segment.hpos);
            archive->put64(segmentpos);     //write segment data offset
            //compress segment data
            Encoder* segencode;
            FileTmp tmp;                    // temporary encoded file
            segencode=new Encoder (COMPRESS, &tmp ,pp); 
            for (U64 k=0; k<segment.pos; ++k) {
                segencode->compress(segment[k]);
            }
            segencode->flush();
            delete segencode;
            archive->put32(segment.pos);     // write segment data size
            printf(" Segment data compressed from %d",segment.pos);
            segment.pos=tmp.curpos();
            segment.setsize(segment.pos);
            printf(" to %d bytes\n ",segment.pos);
            tmp.setpos( 0); 
            if (tmp.blockread(&segment[0], segment.pos)<segment.pos) quit("Segment data corrupted.");
            tmp.close();
            archive->put32(segment.pos);      // write  compressed segment data size
            archive->putc(streambit>>8&0xff); // write stream bit info
            archive->putc(streambit&0xff); 
            archive->setpos(segmentpos); 
            archive->blockwrite(&segment[0], segment.pos); //write out segment data
            //write stream size if present
            for (int i=0;i<streamCount;i++){
                if (filestreamsize[i]>0) archive->put64(filestreamsize[i]);
            }
            printf("Total %d bytes compressed to %d bytes.\n", (U32)total_size,  (U32)archive->curpos()); 
            
        }
        // Decompress files to dir2: paq8pxv -d dir1/archive.paq8pxv dir2
        // If there is no dir2, then extract to dir1
        // If there is no dir1, then extract to .
        else if (!doList) {
            assert(argc>=2);
            String dir(argc>2?argv[2]:argv[1]);
            if (argc==2) {  // chop "/archive.paq8pxv"
                int i;
                for (i=dir.size()-2; i>=0; --i) {
                    if (dir[i]=='/' || dir[i]=='\\') {
                        dir[i]=0;
                        break;
                    }
                    if (i==1 && dir[i]==':') {  // leave "C:"
                        dir[i+1]=0;
                        break;
                    }
                }
                if (i==-1) dir=".";  // "/" not found
            }
            dir=dir.c_str();
            if (dir[0] && (dir.size()!=3 || dir[1]!=':')) dir+="/";
            /////
            
            delete en;
            DecompressType(archive);
            U64 datasegmentsize;
            U64 datasegmentlen;
            int datasegmentpos;
            int datasegmentinfo;
            int datasegmenttype;

            Encoder *defaultencoder;
            defaultencoder=0;
            char *app;
            for (int i=0; i<streamCount; ++i) {
                datasegmentsize=(filestreamsize[i]); // get segment data offset
                if (datasegmentsize>0){              // if segment contains data
                    filestreams[i]->setpos( 0);
                    U64 total=datasegmentsize;
                    datasegmentpos=0;
                    datasegmentinfo=0;
                    datasegmentlen=0;
                    //if (predictord) delete predictord,predictord=0;
                    if (defaultencoder) delete defaultencoder,defaultencoder=0,free(app);
                    //load config file from archive stream
                    //read compressed file header and data
                    app=0;
                    if (level>0) {                  
                       //int fsz=0;  
                       Encoder* enm;
                       enm=new Encoder(DECOMPRESS, archive,dmodel);
                       enm->predictor->set();
                       int len=0;
                       len+=enm->decompress()<<24; //decompress compressed model lenght
                       len+=enm->decompress()<<16;
                       len+=enm->decompress()<<8;
                       len+=enm->decompress();
                       app = (char *)calloc(len+1,1); //alloc mem for decompressed buf
                       // decompress model into buf pp
                       for (int k=0;k<len;++k) app[k]=enm->decompress();
                       //printf("%s",app); //print model 
                       delete enm; //delete encoder and predictor
                    }
                    printf("DeCompressing ");
                    printf("%s   stream(%d).\n",vStreams[i].model,i); 
                    //init encoder with decompressed model app
                    defaultencoder=new Encoder (mode, archive,app); 
                    while (datasegmentsize>0) {
                        while (datasegmentlen==0){
                                datasegmenttype=segment(datasegmentpos++);
                                for (int ii=0; ii<8; ii++) datasegmentlen=datasegmentlen<<8,datasegmentlen+=segment(datasegmentpos++);
                                for (int ii=0; ii<4; ii++) datasegmentinfo=(datasegmentinfo<<8)+segment(datasegmentpos++);
                                //skip if type is recursive or not in current stream
                                if (vTypes[datasegmenttype].type<defaultType || !(isstreamtype(datasegmenttype,i)))datasegmentlen=0;
                                if (level>0) {
                                defaultencoder->predictor->x.filetype=datasegmenttype;
                                defaultencoder->predictor->x.blpos=0;
                                defaultencoder->predictor->x.finfo=datasegmentinfo; 
                                defaultencoder->predictor->set();
                                defaultencoder->predictor->setdebug(0);
                                }
                        }
                        for (U64 k=0; k<datasegmentlen; ++k) {
                            if (!(datasegmentsize&0x1fff)) printStatus(total-datasegmentsize, total,i);
                            filestreams[i]->putc(defaultencoder->decompress());
                            datasegmentsize--;
                        }
                        datasegmentlen=0;
                    }
                }
            } 
            // set datastream file pointers to beginning
            for (int i=0; i<streamCount; ++i)         
            filestreams[i]->setpos( 0);
            /////
            segment.pos=0;
            for (int i=0; i<files; ++i) {
                String out(dir.c_str());
                out+=fname[i];
                DecodeStreams(out.c_str(), fsize[i]);
            } 
            int d=segment(segment.pos++);
            if (d!=0xff) printf("Segmend end marker not found\n");
            for (int i=0; i<streamCount; ++i) {
                filestreams[i]->close();
            }
        }
        archive->close();
        if (!doList) programChecker.print();
    }
    catch(const char* s) {
        if (s) printf("%s\n", s);
    }
    if (pause) {
        printf("\nClose this window or press ENTER to continue...\n");
        getchar();
    }
    return 0;
}

