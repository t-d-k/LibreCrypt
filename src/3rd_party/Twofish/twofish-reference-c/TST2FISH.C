/***************************************************************************
	TST2FISH.C	-- Test code for Twofish encryption

	Submitters:
		Bruce Schneier, Counterpane Systems
		Doug Whiting,	Hi/fn
		John Kelsey,	Counterpane Systems
		Chris Hall,		Counterpane Systems
		David Wagner,	UC Berkeley
			
	Code Author:		Doug Whiting,	Hi/fn
		
	Version  1.00		April 1998
		
	Copyright 1998, Hi/fn and Counterpane Systems.  All rights reserved.
		
	Notes:
		*	Tab size is set to 4 characters in this file
		*	A random number generator is generated and used here, so that 
			the same results can be generated on different platforms/compilers.
		*	Command line arguments:
				-h or ?	==>	give help message
				-lNN	==> set sanity count test loop count to NN
				-m		==> do full MCT generation
				-pPath	==> set file base path
				-r      ==> set initial random seed based on time
				-tNN	==> perform timings with iteration count NN
				-rNN	==> set initial random seed to NN
				-v		==> read & verify files instead of creating them

***************************************************************************/

#include	"aes.h"
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<time.h>
#include	<ctype.h>

extern	CONST char *moduleDescription;	/* which module is running */
extern	CONST char *modeString;			/* which key schedule mode */
extern	CONST int  debugCompile;		/* is external module compiled with debug? */

char CompilerName[8]=
	#if   defined(__BORLANDC__)
		"BCC";
	#elif defined(_MSC_VER)
		"MSC";
	#elif defined(__WATCOMC__)
		"WAT";
	#else
		"???";
	#endif

#if defined(__WATCOMC__) && defined(_M_IX86) && !defined(NO_TIMER)
    DWORD ReadTimeStampCounter(void);
	#pragma aux ReadTimeStampCounter = " db 0Fh,031h" value [eax] modify exact [eax edx] // RDTSC opcode
#endif


/*
+*****************************************************************************
*			Constants/Macros/Tables
-****************************************************************************/

typedef struct
	{
	FILE *f;				/* the file being written/read */
	int  I;					/* test number */
	int	 keySize;			/* key size in bits */
	int	 gotDebugIO;		/* got any debug IO? */
	BYTE pt[BLOCK_SIZE/8];	/* plaintext */
	BYTE ct[BLOCK_SIZE/8];	/* ciphertext */

	keyInstance    ki;		/* use ki.keyDwords as key bits */
	cipherInstance ci;		/* use ci.iv as iv bits */
	} testData;


static char hexTab[]	=	"0123456789ABCDEF";
char		filePath[80]=	"";

int			useAsm		=	0;	/* use assembly language */
int			mctInner	=	MCT_INNER/100;
int			mctOuter	=	MCT_OUTER/10;
int			verify		=	0;	/* set to nonzero to read&verify files */
int			debug		=	0;	/* debugging mode */
int			verbose		=	0;	/* verbose output */
int			quietVerify	=	0;	/* quiet during verify */
int			timeIterCnt	=	0;	/* how many times to iterate for timing */
DWORD		randBits[64]= {1};	/* use Knuth's additive generator */
int			randPtr;
testData *	debugTD		= NULL;	/* for use with debugIO */
int			CLKS_BYTE	=	0;	/* use clks/byte? (vs. clks/block) */
int			FMT_LOG		=	0;	/* format for log file */
int			CLK_MHZ		=	200;/* default clock speed */

#define		KEY_BITS_0			128			/* first key bit setting to test */
#define		STEP_KEY_BITS		((MAX_KEY_BITS-KEY_BITS_0)/2)

static char  hexString[]=
		"0123456789ABCDEFFEDCBA987654321000112233445566778899AABBCCDDEEFF";

/*
+*****************************************************************************
*			Functions
-****************************************************************************/
DWORD Here(DWORD x)
	{
	unsigned int mask=~0U;

	return (* (((DWORD *)&x)-1)) & mask;
	}
extern DWORD TwofishCodeSize(void);

#ifdef USE_ASM
int cdecl get_cpu_type(void);			/* return CPU type */
#endif

/*
+*****************************************************************************
*
* Function Name:	Rand
*
* Function:			Generate random number
*
* Arguments:		None.
*
* Return:			New random number.
*
* Notes:			Uses Knuth's additive generator, other magic
*
-****************************************************************************/
DWORD Rand(void)
	{
	if (randPtr >= 57)
		randPtr = 0;			/* handle the ptr wrap */

	randBits[randPtr] += randBits[(randPtr < 7) ? randPtr-7+57 : randPtr-7];

	randBits[62]+= randBits[61];
	randBits[63] = ROL(randBits[63],9) + 0x6F4ED7D0;	/* very long period! */
	
	return (randBits[randPtr++] ^ randBits[63]) + randBits[62];
	}


/*
+*****************************************************************************
*
* Function Name:	SetRand
*
* Function:			Initialize random number seed
*
* Arguments:		seed	=	new seed value
*
* Return:			None.
*
* Notes:			
*
-****************************************************************************/
void SetRand(DWORD seed)
	{
	int i;
	DWORD x;

	randPtr=0;
	for (i=x=0;i<64;i++)
		{
		randBits[i]=seed;
		x |= seed;		/* keep track of lsb of all entries */
		seed = ROL(seed,11) + 0x12345678;
		}

	if ((x & 1) == 0)	/* insure maximal period by having at least one odd value */
		randBits[0]++;

	for (i=0;i<1000;i++)
		Rand();			/* run it for a while */

	randBits[63] = Rand();
	randBits[62] = Rand();
	randBits[61] = Rand() | 1;	/* make it odd */
	}


/*
+*****************************************************************************
*
* Function Name:	ClearTestData
*
* Function:			Initialize test data to all zeroes
*
* Arguments:		t		=	pointer to testData structure
*
* Return:			None.
*
* Notes:			
*
-****************************************************************************/
void ClearTestData(testData *t)
	{
	t->gotDebugIO=0;
	memset(t->pt,0,BLOCK_SIZE/8);
	memset(t->ct,0,BLOCK_SIZE/8);
	memset(t->ci.iv32,0,BLOCK_SIZE/8);
	memset(t->ki.key32,0,MAX_KEY_BITS/8);
	memset(t->ki.keyMaterial,'0',sizeof(t->ki.keyMaterial));
#if defined(COMPILE_KEY) && defined(USE_ASM)
	t->ki.cSig1=t->ki.cSig2=0;
#endif
	}

/*
+*****************************************************************************
*
* Function Name:	FatalError
*
* Function:			Output a fatal error message and exit
*
* Arguments:		msg		=	fatal error description (printf string)
*					msg2	=	2nd parameter to printf msg
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void FatalError(CONST char *msg,CONST char *msg2)
	{
	printf("\nFATAL ERROR: ");
	printf(msg,msg2);
	exit(2);
	}


/*
+*****************************************************************************
*
* Function Name:	GetTimer
*
* Function:			Return a hi-frequency timer value
*
* Arguments:		None
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
DWORD GetTimer(void)
	{
	DWORD x;

#if defined(__BORLANDC__) && defined(__WIN32__) && !defined(NO_TIMER)
#define	HI_RES_CLK	1
	x=0;
#pragma option -Od					/* disable optimizations (it's a REAL hack!) */
	__emit__(0x0F);	__emit__(0x31);	/* RDTSC opcode */
#pragma option -O.					/* restore optimization setting */
#elif defined(_MSC_VER) && defined(_M_IX86) && !defined(NO_TIMER)
#define	HI_RES_CLK	1
	_asm
		{
		_emit 0x0F
		_emit 0x31
		mov	x,eax
		};
#elif defined(__WATCOMC__) && defined(_M_IX86) && !defined(NO_TIMER)
	#define	HI_RES_CLK	1
	x = ReadTimeStampCounter();
#elif defined(CLOCKS_PER_SEC)
	x=clock();
#else
#define CLOCKS_PER_SEC	1			/* very low resolution timer */
	x=time(NULL);
#endif

	return x;
	}


/*
+*****************************************************************************
*
* Function Name:	TimeOps
*
* Function:			Time encryption/decryption and print results
*
* Arguments:		iterCnt	= how many calls to make
*
* Return:			None.
*
* Notes:			None.
*
-****************************************************************************/
void TimeOps(int iterCnt)
	{
	enum { TEST_CNT	= 3, BLOCK_CNT=64 };
	int   i,j,k,n,q;
	DWORD t0,t1,dt[8],minT;
	DWORD testTime[3][TEST_CNT];
	testData t;
	BYTE text[BLOCK_CNT*(BLOCK_SIZE/8)];
	static char *testName[TEST_CNT]={"BlockEncrypt:","BlockDecrypt:","reKeyEncrypt:"};
	static char *atomName[TEST_CNT]={"block","block","call "};
	static char *format  [TEST_CNT]={"%10.1f/%s ","%10.1f/%s ","%10.1f/%s "};
	static int	 denom   [TEST_CNT]={BLOCK_CNT,BLOCK_CNT,1};
	static int	 needSet [TEST_CNT]={1,1,0};

	ClearTestData(&t);
	for (i=0;i<TEST_CNT;i++)
		{
		if (needSet[i] & 1)
			{
			denom[i]=sizeof(text)/((CLKS_BYTE) ? 1 : (BLOCK_SIZE/8) );
			atomName[i] = (CLKS_BYTE) ? "byte "      : "block";
			}
		format  [i] = (CLKS_BYTE) ? "%10.1f/%s " : "%10.0f/%s ";
		}

	for (i=0;i<MAX_KEY_SIZE;i++)		/* generate random key material */
		t.ki.keyMaterial[i]=hexTab[Rand() & 0xF];
	for (j=0;j<sizeof(text);j++)
		text[j]=(BYTE) Rand();
	memset(dt,0,sizeof(dt));
	dt[0]++;							/* make sure it's in the cache */

	/* calibrate our timing code */
	t0=GetTimer();	t1=GetTimer();	t0++; t1++;	/* force cache line fill */
	for (i=0;i<sizeof(dt)/sizeof(dt[0]);i++)
		{
		t0=GetTimer();
		t1=GetTimer();
		dt[i]=t1-t0;
		}

	for (n=0;n<TEST_CNT;n++)			/* gather all data into testTime[][] */
		{
		for (t.keySize=KEY_BITS_0,q=0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS,q++)
			{
			cipherInit(&t.ci,MODE_ECB,NULL);
			makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial);

#if defined(HI_RES_CLK)
#define		CALL_N						/* just call once */
#define		ICNT	1.0
#define		TSCALE	1.0
			for (k=0,minT=~0lu;k<iterCnt;k++)
#else
#define		CALL_N	for (j=0;j<iterCnt;j++)
#define		ICNT	((double)iterCnt)
#define		TSCALE	((1.0E6*CLK_MHZ)/CLOCKS_PER_SEC)
			for (k=0,minT=~0lu;k<4;k++)
#endif
				{	/* run a few times to get "best" time */
				switch (n)
					{
					case 0:
						blockEncrypt(&t.ci,&t.ki,text,sizeof(text)*8,text);
						GetTimer();
						t0=GetTimer();
						CALL_N blockEncrypt(&t.ci,&t.ki,text,sizeof(text)*8,text);
						t1=GetTimer();
						break;
					case 1:
						blockDecrypt(&t.ci,&t.ki,text,sizeof(text)*8,text);
						GetTimer();
						t0=GetTimer();
						CALL_N blockDecrypt(&t.ci,&t.ki,text,sizeof(text)*8,text);
						t1=GetTimer();
						break;
					case 2:
						reKey(&t.ki);
						GetTimer();
						t0=GetTimer();
						CALL_N { t.ki.key32[0]+=0x87654321;	/* change key bytes to force cache misses */
								 t.ki.key32[1]+=0x9ABCDEF3;
								 reKey(&t.ki); }
						t1=GetTimer();
						break;
					default:
						FatalError("Unknown test","");
						break;
					}
				if (minT > t1-t0)
					minT = t1-t0;
				}
			testTime[q][n]=minT;
			}
		}
	/* now print all the results */
#ifdef HI_RES_CLK
	if (!FMT_LOG)
		{
		printf("\nCalibrate GetTimer(): ",t1-t0);
		for (i=0;i<sizeof(dt)/sizeof(dt[0]);i++)
			printf("%6ld",dt[i]);
		printf("\n\n");
		}
#else
	printf("All times in clks, assuming %d MHz CPU (use -fmNN switch to set)\n",CLK_MHZ);
	printf("CLOCKS_PER_SEC = %8.1f\n",(double)CLOCKS_PER_SEC);
#endif

	printf("%-13s","keySize=");
	for (t.keySize=KEY_BITS_0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		printf("%10d bits  ",t.keySize);
	printf("\n");
		
	for (n=0;n<TEST_CNT;n++)
		{
		printf("%-13s",testName[n]);
		for (q=0;q<3;q++)
			{
			printf(format[n],TSCALE*testTime[q][n]/(ICNT*(double)denom[n]),atomName[n]);
			}
		printf("\n");
		}
	if (FMT_LOG)
		printf(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
	}


/*
+*****************************************************************************
*
* Function Name:	AES_Sanity_Check
*
* Function:			Make sure things work to the interface spec and
*					that encryption and decryption are inverse functions
*
* Arguments:		None.
*
* Return:			None.
*
* Notes:			Will FatalError if any problems found
*
-****************************************************************************/
void AES_Sanity_Check(int testCnt)
	{
	static DWORD hexVal[] =
			{0x67452301,0xEFCDAB89,0x98BADCFE,0x10325476,
			 0x33221100,0x77665544,0xBBAA9988,0xFFEEDDCC};
	static char *modeNames[]={"(null)","MODE_ECB","MODE_CBC","MODE_CFB1"};

	int i,j,q,n,testNum,lim,saveDebug=debug;
	testData t;
	keyInstance k2;
	BYTE pt[128];
	BYTE ct[128];
	char ivString[BLOCK_SIZE/4];
	char *mName;
	BYTE mode;
#if ALIGN32
	BYTE alignDummy[3];	/* keep dword alignment on stack after BYTE mode */
#endif

	
	if (!quietVerify) printf("\nTwofish code sanity check...");
#if (MODE_CFB1 != MODE_ECB  + 2)
#error Need to change mode loop constants
#endif
	if (testCnt)
	for (mode=MODE_ECB;mode<=MODE_CFB1;mode++)
		{
		debug=(mode == saveDebug);
		mName=modeNames[mode];
		if (cipherInit(&t.ci,mode,hexString) != TRUE)
			FatalError("cipherInit error during sanity check %s",mName);
		if (t.ci.mode != mode)
			FatalError("Cipher mode not set properly during sanity check %s",mName);
		if (mode != MODE_ECB)
			for (i=0;i<BLOCK_SIZE/32;i++)
				if (t.ci.iv32[i] != hexVal[i])
					FatalError("Invalid IV parse during sanity check %s",mName);
		lim = (mode == MODE_CFB1) ? (testCnt+31)/32 : testCnt;
		for (t.keySize=KEY_BITS_0;t.keySize <= MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
			{
			/* printf("Running %-9s sanity check on keySize = %3d.\n",mName,t.keySize); */
			if (!quietVerify) printf(".");	/* show some progress */
			ClearTestData(&t);
			debug=0;			/* don't show debug info in this makeKey call */
			if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,hexString) != TRUE)
				FatalError("Error parsing key during sanity check %s",mName);
			debug=(mode == saveDebug);
			for (i=0;i<t.keySize/32;i++)
				if (t.ki.key32[i]!=hexVal[i])
					FatalError("Invalid key parse during sanity check %s",mName);
			for (testNum=0;testNum<lim;testNum++)
				{						/* run a bunch of encode/decode tests */
				if ((testNum&0x1F)==0)	/* periodic re-key time? */
					{
					for (j=0;j<t.keySize/4;j++)
						t.ki.keyMaterial[j]=hexTab[Rand() & 0xF];
					if (testNum==0)
						ClearTestData(&t);	/* give "easy" test data the first time */
					if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
						FatalError("Encrypt makeKey during sanity check %s",mName);
					debug=0;
					if (makeKey(&k2  ,DIR_DECRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
						FatalError("Decrypt makeKey during sanity check %s",mName);
					debug=(mode == saveDebug);
					}
				if (mode != MODE_ECB)				/* set IV  if needed*/
					for (j=0;j<BLOCK_SIZE/4;j++)
						ivString[j]=hexTab[(testNum)? Rand() & 0xF : 0];
				if ((debug) || (testNum == 0))
					n = (BLOCK_SIZE/8);				/* do only one block if debugging */
				else
					n = (BLOCK_SIZE/8)*(1 + (Rand() % (sizeof(pt)/(BLOCK_SIZE/8))));

				for (j=0;j<n;j++)					/* set random plaintext */
					pt[j]=(testNum) ? (BYTE) Rand() : 0;
				if (mode == MODE_CBC)
					{	/* check that CBC works as advertised */
					cipherInit(&t.ci,mode,ivString);
					t.ci.mode=MODE_ECB;
					for (q=0;q<BLOCK_SIZE/8;q++)	/* copy over the iv */
						t.pt[q] = (BYTE) (t.ci.iv32[q/4] >> (8*(q&3)));	/* auto-Bswap! */
					for (j=0;j<n;j+=BLOCK_SIZE/8)
						{
						for (q=0;q<BLOCK_SIZE/8;q++)	/* xor in next block */
							t.pt[q] ^= pt[j+q];
						debug=0;
						if (BLOCK_SIZE != blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.pt))
							FatalError("blockEncrypt return value during sanity check %s",mName);
						debug=(mode == saveDebug);
						}
					t.ci.mode=MODE_CBC;			/* restore mode */
					}
				/* encrypt */
				cipherInit(&t.ci,mode,ivString);
				if (n*8 != blockEncrypt(&t.ci,&t.ki,pt,n*8,ct))
					FatalError("blockEncrypt return value during sanity check %s",mName);
				if (mode == MODE_CBC)			/* validate CBC "hash" */
					for (q=0;q<BLOCK_SIZE/8;q++)
						if (t.pt[q] != ct[n-BLOCK_SIZE/8+q])
							FatalError("CBC doesn't work during sanity check %s",mName);
				/* decrypt */
				cipherInit(&t.ci,mode,ivString);
				if (n*8 != blockDecrypt(&t.ci,&t.ki,ct,n*8,ct))
					FatalError("blockDecrypt return value during sanity check %s",mName);
				/* compare */
				for (j=0;j<n;j++)
					if (pt[j] != ct[j])
						{
						char s[128];
						sprintf(s,"Sanity check: encrypt/decrypt miscompare (mode=%s,keySize=%d)",
								mName,t.keySize);
						FatalError(s,"");
						}
				if (debug)
					{
					if (testNum >= debug)
						exit(1);
					printf(";-------------------------------------------------\n");
					}
				}
			}
		}
	debug=saveDebug;
	if (!quietVerify) printf("  OK\n");
	}


/*
+*****************************************************************************
*
* Function Name:	AES_FileIO
*
* Function:			Output to file or verify file contents vs. string
*
* Arguments:		f		=	opened file
*					s		=	string to output/compare (NULL-->reset, return)
*					errOK	=	do not fatalError on miscompare
*
* Return:			Zero --> compare ok
*
* Notes:			On miscompare, FatalError (unless errOK)
*
-****************************************************************************/
int AES_FileIO(FILE *f,CONST char *s,int errOK)
	{
	int  i;
	static int  lineNum=0;
	static int  j=0;
	static char line[516]="";

	if (s == NULL)	/* starting new file */
		{
		line[0]=j=lineNum=0;
		return 0;
		}

	if (!verify)
		{
		fprintf(f,s);
		return 0;
		}
				
	/* here to verify the file against the string */
	for (i=0;s[i];i++)
		{
		while (line[j] == 0)
			{
			lineNum++;
			if (fgets(line,sizeof(line)-4,f) == NULL)
				{
				if ((s[i]=='\n') && (s[i+1]==0))
					{
					line[0]=j=0;	/* missing final eol is ok */
					return 0;
					}
				FatalError("Unexpected EOF looking for %s",s);
				}
			if (verbose) printf(line);
			j=0;
			}
		if (s[i] != line[j])
			{
			if ((s[i] == '\n') && ((i==0) || (s[i-1] == '\n'))) continue; /* blank line skip */
			if (line[j] == '\n') {j++; continue; }
			if (!errOK)
				{
				char tmp[1024];
				sprintf(tmp,"Miscompare at line #%d:\n%s\nlooking for\n\n%%s",lineNum,line);
				FatalError(tmp,s);
				}
			line[0]=j=0;	/* let caller re-synch if desired */
			return 1;		/* return error flag */
			}
		j++;
		}

	return 0;
	}



/*
+*****************************************************************************
*
* Function Name:	AES_PutFileHeader
*
* Function:			Output a text header for AES test file
*
* Arguments:		fileName	=	name of file to create
*					testName	=	name of the specific test
*
* Return:			Open FILE pointer
*
* Notes:			If unable to create, gives FatalError
*
-****************************************************************************/
FILE *AES_PutFileHeader(CONST char *fileName,CONST char *testName)
	{
	char s[512];
	FILE *f;

	sprintf(s,"%s%s",filePath,fileName);
	if (verify)
		{
		if (!quietVerify) printf("Verifying file %s",s);
		f=fopen(s,"rt");
		AES_FileIO(NULL,NULL,0);		/* reset file read state */
		}
	else
		{
		printf("Creating file %s.\n",s);
		f=fopen(s,"wt");
		}
	if (f == NULL) FatalError("Unable to open file '%s'",s);

	sprintf(s,
			"\n=========================\n"
			"\n"
			"FILENAME:  \"%s\"\n"
			"\n"
			"%s\n"
			"\n"
			"Algorithm Name:       TWOFISH\n"
			"Principal Submitter:  Bruce Schneier, Counterpane Systems\n"
			"\n"
			"==========\n"
			"\n",
			fileName,testName);

	if (AES_FileIO(f,s,1))		
		{						/* header mismatch */
		if (!verify)
			FatalError("Miscompare while not verifying??","");
		printf("  \tWARNING:  header mismatch!");
		fgets(s,sizeof(s)-4,f);
		do	{					/* skip rest of "bad" header */
			if (fgets(s,sizeof(s)-4,f) == NULL)
				break;			/* end of file? */
			}
		while ((s[0] != '=') || (s[1] != '='));
		fgets(s,sizeof(s)-4,f);	/* skip trailing blank line */
		}

	if (verify)
		if (!quietVerify) printf("\n");

	return f;
	}

/*
+*****************************************************************************
*
* Function Name:	AES_PutTestResult
*
* Function:			Output a test result
*
* Arguments:		f		=	output file
*					name	=	name of field
*					p		=	pointer to bytes/dwords
*					cnt		=	# bytes to output
*					fmt32	=	nonzero --> p points to dwords, else bytes
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_PutBytes(FILE *f,CONST char *name,CONST void *p,int cnt,int fmt32)
	{
	char s[128];
	int i,j,a;
	if (p == NULL) return;

	a = (fmt32) ? ADDR_XOR : 0;	/* handle big/little endian on dword i/o */

	sprintf(s,"%s=",name);
	for (j=0;s[j];j++) ;
	for (i=0;i<cnt;i++)
		{
		s[j++]=hexTab[((BYTE *)p)[i ^ a] >> 4 ];
		s[j++]=hexTab[((BYTE *)p)[i ^ a] & 0xF];
		}
	s[j++]='\n';
	s[j  ]=0;	/* terminate the string */

	AES_FileIO(f,s,0);
	}


/*
+*****************************************************************************
*
* Function Name:	AES_printf
*
* Function:			Output a test result
*
* Arguments:		t		=	testData (includes output file)
*					fmt		=	format list (string of chars, see notes)
*
* Return:			None.
*
* Notes:
*	The fmt string specifies what is output. The following characters are
*	treated specially (S,K,P,C,v,V,I).  See the code in the switch statement
*	to see how they are handled.  All other characters (e.g., '\n') are
*	simply output to the file.
*
-****************************************************************************/
void AES_printf(testData *t,CONST char *fmt)
	{
	char s[40];

	for (s[1]=0;*fmt;fmt++)
		switch (*fmt)
			{
			case 'I': sprintf(s,"I=%d\n",t->I);				AES_FileIO(t->f,s,0);	break;
			case 'S': sprintf(s,"KEYSIZE=%d\n",t->keySize); AES_FileIO(t->f,s,0);	break;
			case 'P': AES_PutBytes(t->f,"PT" ,t->pt		 ,BLOCK_SIZE/8,0);	break;
			case 'C': AES_PutBytes(t->f,"CT" ,t->ct	     ,BLOCK_SIZE/8,0);	break;
			case 'v': AES_PutBytes(t->f,"IV" ,t->ci.IV   ,BLOCK_SIZE/8,0);	break;
			case 'V': AES_PutBytes(t->f,"IV" ,t->ci.iv32 ,BLOCK_SIZE/8,1);	break;
			case 'K': AES_PutBytes(t->f,"KEY",t->ki.key32,t->keySize/8,1);	break;
			default:  s[0]=*fmt; s[1]=0; AES_FileIO(t->f,s,0);				break;
			}
	}

/*
+*****************************************************************************
*
* Function Name:	AES_EndSection
*
* Function:			Insert a separator between sections
*
* Arguments:		t		=	ptr to testData, contains file
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_EndSection(testData *t)
	{
	AES_FileIO(t->f,"==========\n\n",0);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Close
*
* Function:			Close an AES text file
*
* Arguments:		t	=	testData ptr (contains f)
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Close(testData *t)
	{
	fclose(t->f);
	}

/*
+*****************************************************************************
*
* Function Name:	DebugIO
*
* Function:			Output debug string
*
* Arguments:		s	=	string to output
*
* Return:			None.
*
* Notes:			
*
-****************************************************************************/
void DebugIO(CONST char *s)
	{
	if (debugTD)
		{
		AES_FileIO(debugTD->f,s,0);
		debugTD->gotDebugIO=1;
		}
	else
		printf(s);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_VK
*
* Function:			Run variable key test
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_VK(CONST char *fname)
	{
	testData t;

	memset(t.ki.keyMaterial,'0',MAX_KEY_SIZE);

	t.f=AES_PutFileHeader(fname,
			  "Electronic Codebook (ECB) Mode\nVariable Key Known Answer Tests");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		{
		ClearTestData(&t);
		AES_printf(&t,"S\nP\n");	/* output key size, plaintext */
		for (t.I=1;t.I<=t.keySize;t.I++)
			{
			t.ki.keyMaterial[(t.I-1)/4]='0' + (8 >> ((t.I-1) & 3));
			if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
				FatalError("Error parsing key during %s test",fname);
			if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
				FatalError("blockEncrypt return during %s test",fname);
			AES_printf(&t,"IKC\n");	/* output I, KEY, CT, newline */

			t.ki.keyMaterial[(t.I-1)/4]='0';	/* rezero the key bit */
			}
		AES_EndSection(&t);
		}

	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_Intermediate
*
* Function:			Run intermediate value test
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_Intermediate(CONST char *fname)
	{
	testData t;

	if ((useAsm) || (!debugCompile))
		{
		if (!quietVerify) printf("WARNING: Skipping %s test\n",fname);
		return;
		}

	memset(t.ki.keyMaterial,'0',MAX_KEY_SIZE);

	t.f=AES_PutFileHeader(fname,
			  "Electronic Codebook (ECB) Mode\nIntermediate Value Tests");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		{
		ClearTestData(&t);
		debugTD=&t;
		debug=1;

		if (t.keySize > KEY_BITS_0)
			memcpy(t.ki.keyMaterial,hexString,sizeof(t.ki.keyMaterial));
		debug=0;
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during %s test",fname);
		debug=1;
		AES_printf(&t,"S\nK\n");	/* output key size, key */
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during %s test",fname);

		AES_printf(&t,"P\n");		/* output plaintext */
		AES_FileIO(t.f,"Encrypt()\n",0);

		if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
			FatalError("blockEncrypt return during %s test",fname);
		AES_printf(&t,"\nC\n");	/* output CT, newline */

		AES_FileIO(t.f,"Decrypt()\n",0);
		AES_printf(&t,"\nC\n");	/* output CT, newline */
		if (blockDecrypt(&t.ci,&t.ki,t.ct,BLOCK_SIZE,t.pt) != BLOCK_SIZE)
			FatalError("blockDecrypt return during %s test",fname);
		AES_printf(&t,"\nP\n");	/* output PT, newline */

		AES_EndSection(&t);
		if (!t.gotDebugIO)
			FatalError("Need to run DEBUG version to test %s",fname);
		debug=0;
		debugTD=NULL;
		}

	AES_Close(&t);
	}


/*
+*****************************************************************************
*
* Function Name:	AES_Test_VT
*
* Function:			Run variable text test
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_VT(CONST char *fname)
	{
	testData t;

	memset(	t.ki.keyMaterial,'0',MAX_KEY_SIZE);

	t.f=AES_PutFileHeader(fname,
			  "Electronic Codebook (ECB) Mode\nVariable Text Known Answer Tests");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		{
		ClearTestData(&t);
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during %s test",fname);

		AES_printf(&t,"S\nK\n");	/* output key size, key */
		for (t.I=1;t.I<=BLOCK_SIZE;t.I++)
			{
			t.pt[(t.I-1)/8] = 0x80 >> ((t.I-1) & 7);
			if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
				FatalError("blockEncrypt return during %s test",fname);
			AES_printf(&t,"IPC\n");	/* output I, PT, CT, newline */
			t.pt[(t.I-1)/8] = 0;
			}
		AES_EndSection(&t);
		}
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_TBL
*
* Function:			Run tabl test
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_TBL(CONST char *fname)
	{
	int i;
	testData t;

	t.f=AES_PutFileHeader(fname,
			"Electronic Codebook (ECB) Mode\nTables Known Answer Test\n"
			"Tests permutation tables and MDS matrix multiply tables.");

	for (t.keySize=KEY_BITS_0;t.keySize <= MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		{
		AES_printf(&t,"S\n");	/* output key size */
		TableOp(TAB_ENABLE);
		TableOp(TAB_RESET);

		ClearTestData(&t);
		if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
			FatalError("Error cipherInit() during %s test",fname);

		for (t.I=1;TableOp(TAB_QUERY) == FALSE;t.I++)
			{
			if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
				FatalError("Error parsing key during %s test",fname);
			if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
				FatalError("blockEncrypt during %s test",fname);
			AES_printf(&t,"IKPC\n");		/* show the 'vector' */
			memcpy(t.ki.keyMaterial+MAX_KEY_SIZE/2,t.ki.keyMaterial,MAX_KEY_SIZE/2);
			for (i=0;i<MAX_KEY_SIZE/2;i+=2)	/* make new key from old paintext */
				{
				t.ki.keyMaterial[i  ]=hexTab[t.pt[i/2] >> 4];
				t.ki.keyMaterial[i+1]=hexTab[t.pt[i/2] &0xF];
				}
			memcpy(t.pt,t.ct,BLOCK_SIZE/8);	/* use ciphertext as new plaintext */
			}
		TableOp(TAB_DISABLE);
		AES_EndSection(&t);		/* output separator */
		if (!quietVerify) printf("  [%d,%3d]",t.keySize,t.I);
		}
	if (!quietVerify) printf("\n");
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_ECB_E_MCT
*
* Function:			Run ECB Monte Carlo test for ECB encryption
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_ECB_E_MCT(CONST char *fname)
	{
	int i,j,q;
	testData t;

	t.f=AES_PutFileHeader(fname,
				"Electronic Codebook (ECB) Mode - ENCRYPTION\nMonte Carlo Test");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0,q=0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS,q+=2)
		{
		AES_printf(&t,"S\n");			/* output key size */
		if (!quietVerify) printf("  keyLen = %3d. ",t.keySize);

		ClearTestData(&t);				/* start with all zeroes */
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during %s test",fname);

		for (t.I=0;t.I<mctOuter;t.I++)
			{
			AES_printf(&t,"IKP");
			if (!quietVerify) printf("%3d\b\b\b",t.I);
			for (j=0;j<mctInner;j++)
				{
				if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
					FatalError("blockEncrypt return during %s test",fname);
				if (j == mctInner-1)	/* xor the key for next outer loop */
					for (i=0;i<t.keySize/32;i++)
						t.ki.key32[i] ^=
							Bswap((i>=q) ? ((DWORD *)t.ct)[i-q] :
										   ((DWORD *)t.pt)[BLOCK_SIZE/32-q+i]);
				BlockCopy(t.pt,t.ct);
				}
			AES_printf(&t,"C\n");
			if (reKey(&t.ki) != TRUE)
				FatalError("reKey return during %s test",fname);
			}
		AES_EndSection(&t);
		}
	if (!quietVerify) printf("   \n");
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_ECB_D_MCT
*
* Function:			Run ECB Monte Carlo test for ECB decryption
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_ECB_D_MCT(CONST char *fname)
	{
	int i,j,q;
	testData t;

	t.f=AES_PutFileHeader(fname,
				"Electronic Codebook (ECB) Mode - DECRYPTION\nMonte Carlo Test");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0,q=0;t.keySize <= MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS,q+=2)
		{
		AES_printf(&t,"S\n");			/* output key size */
		if (!quietVerify) printf("  keyLen = %3d. ",t.keySize);

		ClearTestData(&t);				/* start with all zeroes */
		if (makeKey(&t.ki,DIR_DECRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during %s test",fname);

		for (t.I=0;t.I<mctOuter;t.I++)
			{
			AES_printf(&t,"IKC");
			if (!quietVerify) printf("%3d\b\b\b",t.I);
			for (j=0;j<mctInner;j++)
				{
				if (blockDecrypt(&t.ci,&t.ki,t.ct,BLOCK_SIZE,t.pt) != BLOCK_SIZE)
					FatalError("blockDecrypt return during %s test",fname);
				if (j == mctInner-1)	/* xor the key for next outer loop */
					for (i=0;i<t.keySize/32;i++)
						t.ki.key32[i] ^=
							Bswap((i>=q) ? ((DWORD *)t.pt)[i-q] :
									       ((DWORD *)t.ct)[BLOCK_SIZE/32-q+i]);
				BlockCopy(t.ct,t.pt);
				}
			AES_printf(&t,"P\n");
			if (reKey(&t.ki) != TRUE)
				FatalError("reKey return during %s test",fname);
			}
		AES_EndSection(&t);
		}
	if (!quietVerify) printf("   \n");
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_CBC_E_MCT
*
* Function:			Run ECB Monte Carlo test for CBC encryption
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_CBC_E_MCT(CONST char *fname)
	{
	int i,j,q;
	testData t;
	BYTE ctPrev[BLOCK_SIZE/8];
	BYTE IV[BLOCK_SIZE/8];
#define	CV	t.ci.IV						/* use t.ci.IV as CV */

	t.f=AES_PutFileHeader(fname,
			"Cipher Block Chaining (CBC) Mode - ENCRYPTION\nMonte Carlo Test");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0,q=0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS,q+=2)
		{
		AES_printf(&t,"S\n");			/* output key size */
		if (!quietVerify) printf("  keyLen = %3d. ",t.keySize);

		ClearTestData(&t);				/* start with all zeroes */
		memset(IV,0,sizeof(IV));
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial)	 != TRUE)
			FatalError("Error parsing key during %s test",fname);

		BlockCopy(CV,IV);				
		for (t.I=0;t.I<mctOuter;t.I++)
			{
			AES_printf(&t,"IKvP");
			if (!quietVerify) printf("%3d\b\b\b",t.I);
			for (j=0;j<mctInner;j++)
				{
				for (i=0;i<BLOCK_SIZE/8;i++)
					t.pt[i]  ^= CV[i];		/* IB = PT ^ CV */
				BlockCopy(ctPrev,t.ct);		/* save previous ct */

				if (blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct) != BLOCK_SIZE)
					FatalError("blockEncrypt return during %s test",fname);
				BlockCopy(t.pt,(j)? ctPrev : CV);
				BlockCopy(CV,t.ct);
				}
			AES_printf(&t,"C\n");

			for (i=0;i<t.keySize/32;i++)
				t.ki.key32[i] ^=
					Bswap((i>=q) ? ((DWORD *)t.ct  )[i-q] :
								   ((DWORD *)ctPrev)[BLOCK_SIZE/32-q+i]);
			BlockCopy(t.pt,ctPrev);
			BlockCopy(CV,t.ct);

			if (reKey(&t.ki) != TRUE)
				FatalError("reKey return during %s test",fname);
			}
		AES_EndSection(&t);
		}
	if (!quietVerify) printf("   \n");
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	AES_Test_CBC_D_MCT
*
* Function:			Run ECB Monte Carlo test for CBC decryption
*
* Arguments:		fname	=	file name to produce
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void AES_Test_CBC_D_MCT(CONST char *fname)
	{
	int i,j,q;
	testData t;
	BYTE ptPrev[BLOCK_SIZE/8];
	BYTE IV[BLOCK_SIZE/8];
#define	CV	t.ci.IV						/* use t.ci.IV as CV */

	t.f=AES_PutFileHeader(fname,
			"Cipher Block Chaining (CBC) Mode - DECRYPTION\nMonte Carlo Test");

	if (cipherInit(&t.ci,MODE_ECB,NULL) != TRUE)
		FatalError("cipherInit error during %s test",fname);

	for (t.keySize=KEY_BITS_0,q=0;t.keySize <= MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS,q+=2)
		{
		AES_printf(&t,"S\n");			/* output key size */
		if (!quietVerify) printf("  keyLen = %3d. ",t.keySize);

		ClearTestData(&t);				/* start with all zeroes */
		memset(IV,0,sizeof(IV));
		if (makeKey(&t.ki,DIR_DECRYPT,t.keySize,t.ki.keyMaterial)	 != TRUE)
			FatalError("Error parsing key during %s test",fname);

		BlockCopy(CV,IV);				
		for (t.I=0;t.I<mctOuter;t.I++)
			{
			AES_printf(&t,"IKvC");
			if (!quietVerify) printf("%3d\b\b\b",t.I);
			for (j=0;j<mctInner;j++)
				{
				BlockCopy(ptPrev,t.pt);
				if (blockDecrypt(&t.ci,&t.ki,t.ct,BLOCK_SIZE,t.pt) != BLOCK_SIZE)
					FatalError("blockDecrypt return during %s test",fname);
				for (i=0;i<BLOCK_SIZE/8;i++)
					t.pt[i]  ^= CV[i];		/* PT = OB ^ CV */
				BlockCopy(CV,t.ct);			/* CV = CT */
				BlockCopy(t.ct,t.pt);		/* CT = PT */
				}
			AES_printf(&t,"P\n");

			for (i=0;i<t.keySize/32;i++)
				t.ki.key32[i] ^=
					Bswap((i>=q) ? ((DWORD *)t.pt  )[i-q] :
								   ((DWORD *)ptPrev)[BLOCK_SIZE/32-q+i]);
			if (reKey(&t.ki) != TRUE)
				FatalError("reKey return during %s test",fname);
			}
		AES_EndSection(&t);
		}
	if (!quietVerify) printf("   \n");
	AES_Close(&t);
	}

/*
+*****************************************************************************
*
* Function Name:	ShowParams
*
* Function:			Print out the settings of settable parameters
*
* Arguments:		None.
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void ShowParams(void)
	{
	int saveDebug=debug;
	testData t;

	debug=0;	/* turn off debug output */
	memset(t.ki.keyMaterial,'0',sizeof(t.ki.keyMaterial));

	printf("(keyLen,numRounds): ");
	for (t.keySize=KEY_BITS_0;t.keySize<=MAX_KEY_BITS;t.keySize+=STEP_KEY_BITS)
		{
		if (makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial) != TRUE)
			FatalError("Error parsing key during ShowParam","");
		printf("  (%d,%d)",t.keySize,t.ki.numRounds);
		}
	printf("\n");
	debug=saveDebug;
	}

/*
+*****************************************************************************
*
* Function Name:	ParseArgFile
*
* Function:			Parse commands from argument file
*
* Arguments:		fName		=	name of file to read
*					argList		=	list of ptrs to fill in
*					maxArgCnt	=	size of argList
*
* Return:			None.
*
* Notes:	'/' and ';' are comment to end of line characters in the file
*			This function is used to allow a "custom" set of switches to
*			be automatically read from a file at startup.
*
-****************************************************************************/
int ParseArgFile(CONST char *fName,char *argList[],int maxArgCnt)
	{
	static char buf[1024];
	static int  bufCnt=0;		/* current # chars in buf */

	int i,j,k,argCnt;
	char line[256];
	FILE *f=fopen(fName,"rt");

	if (f == NULL) return 0;
	if (debug) printf("Reading args from file %s: ",fName);

	for (argCnt=0;argCnt<maxArgCnt;)
		{	/* read in arg file one line at a time */
		memset(line,0,sizeof(line));
		if (fgets(line,sizeof(line)-4,f) == NULL)
			break;
		for (i=0;line[i];i++)	/* ignore comments to end of line */
			if ((line[i]=='/') || (line[i]==';'))
				{ line[i]=line[i+1]=0; break; }
		for (i=0;line[i];)		/* parse out tokens */
			{
			for (j=i;line[j];j++)	/* skip leading whitespace */
				if (line[j] > ' ') break;
			if (line[j]==0) break;
			for (k=j;line[k];k++)
				if (line[k] <= ' ') break;
			/* now j..k-1 defines a token */
			if (k-j+1 > (int)(sizeof(buf) - bufCnt))
				FatalError("Arg file too large: %s",line);
			if (argCnt >= maxArgCnt)
				break;
			memcpy(buf+bufCnt,line+j,k-j);
			buf[bufCnt+k-j]=0;	/* terminate the token */
			if (debug) printf(" %s",buf+bufCnt);
			argList[argCnt++]=buf+bufCnt;
			bufCnt+=k-j+1;
			i=k;				/* skip to next token */
			}
		}
	fclose(f);
	if (debug) printf("\n");
	return argCnt;
	}

/*
+*****************************************************************************
*
* Function Name:	GiveHelp
*
* Function:			Print out list of command line switches
*
* Arguments:		None.
*
* Return:			None.
*
* Notes:
*
-****************************************************************************/
void GiveHelp(void)
	{
	printf("Syntax:   TST2FISH [options]\n"
		   "Purpose:  Generate/validate AES Twofish code and files\n"
		   "Options:  -lNN    ==> set sanity check loop to NN\n"
		   "          -m      ==> do full MCT generation\n"
		   "          -pPath  ==> set file path\n"
		   "          -s      ==> set initial random seed based on time\n"
		   "          -sNN    ==> set initial random seed to NN\n"
		   "          -tNN    ==> time performance using NN iterations\n"
		   "          -v      ==> validate files, don't generate them\n",
		   MAX_ROUNDS
		  );
	exit(1);
	}

#ifdef TEST_EXTERN
void	Test_Extern(void);
#endif

void ShowHex(FILE *f,CONST void *p,int bCnt,CONST char *name)
	{
	int i;

	fprintf(f,"    ;%s:",name);
	for (i=0;i<bCnt;i++)
		{
		if ((i % 8) == 0)
			fprintf(f,"\n\t.byte\t");
		else
			fprintf(f,",");
		fprintf(f,"0%02Xh",((BYTE *)p)[i]);
		}
	fprintf(f,"\n");
	}

/* output a formatted 6805 test vector include file */
void Debug6805(void)
	{
	int i,j;
	testData t;
	FILE *f;

	ClearTestData(&t);
	t.keySize=128;
	
	f=stdout;
	cipherInit(&t.ci,MODE_ECB,NULL);
	makeKey(&t.ki,DIR_ENCRYPT,t.keySize,t.ki.keyMaterial);

	for (i=0;i<4;i++)	/* make sure it all fits in 256 bytes */
		{
		reKey(&t.ki);
		blockEncrypt(&t.ci,&t.ki,t.pt,BLOCK_SIZE,t.ct);
		fprintf(f,"; Twofish vector #%d\n",i+1);
		ShowHex(f,&t.keySize,1,"Key Size");
		ShowHex(f,t.ki.key32,16,"Key");
		ShowHex(f,t.pt,BLOCK_SIZE/8,"Plaintext");
		ShowHex(f,t.ct,BLOCK_SIZE/8,"Ciphertext");
		for (j=0;j<16;j++)
			((BYTE *)t.ki.key32)[j] = t.pt[j] ^ t.ct[j];
		memcpy(t.pt,t.ct,sizeof(t.pt));
		fprintf(f,";-------------------------------------------------------\n");
		}
	fprintf(f,"\n\t.byte 0\t;end of list\n");
	fclose(f);
	}

/*
+*****************************************************************************
*	Main AES test function
-****************************************************************************/
int main(int argc,char *argv[])
	{
#define	MAX_ARGS	40
	int i,j,k,argCnt,testCnt=32;
	int doTableTest=1;
	int doIntermediate=0;
	DWORD randSeed=0x12345678;
	char *argList[MAX_ARGS];
	char *moduleName=moduleDescription;

	i=1;			/* make sure LittleEndian is defined correctly */
	if (b0(i) != 1)
		FatalError("LittleEndian defined incorrectly","");
	if ((ALIGN32) && (k == 2))
		FatalError("Cannot enable ALIGN32 in 16-bit mode\n","");

#if ((MCT_INNER != 10000) || (MCT_OUTER != 400))
#error  MCT loop counts incorrect!
#endif

	argCnt=ParseArgFile("TST2FISH.CFG",argList,MAX_ARGS);	/* read parameter file */
	for (i=1;(i<argc) && (argCnt<MAX_ARGS);i++)				/* append command line */
		argList[argCnt++]=argv[i];

	for (i=0;i<argCnt;i++)	/* parse command line arguments */
		{
		if (argList[i][0] == '-')
			switch (toupper(argList[i][1]))
				{
				case 'D':
					if (argList[i][2])
						debug=atoi(argList[i]+2);
					else
						debug=1;
					break;
				case 'F':
					switch (toupper(argList[i][2]))
						{
						case 'L':	FMT_LOG		=	1;
									testCnt		=	0;	break;
						case 'B':	CLKS_BYTE	=	~CLKS_BYTE;	break;
						case 'M':	CLK_MHZ		=	atoi(argList[i]+3);	break;
						}
					break;
				case '?':
				case 'H':
					GiveHelp();
					break;	
				case 'A':
					if (argList[i][2])
						useAsm=atoi(argList[i]+2);
					else
						useAsm=7;		/* enable everything in ASM */
					break;
				case 'I':
					doIntermediate=1;
					break;
				case 'L':
					testCnt = atoi(argList[i]+2);
					break;
				case 'M':				/* do FULL MCT generation */
					mctInner = MCT_INNER;
					mctOuter = MCT_OUTER;
					break;
				case 'P':
					for (j=0;j<sizeof(filePath)-4;j++)
						if ((filePath[j]=argList[i][j+2]) == 0)
							break;
					filePath[j]=filePath[j+1]=0;
#ifdef _M_IX86	/* DOS/Win specific filePath stuff */
					if ((j) && (filePath[j-1] != ':') && (filePath[j-1] != '\\'))
						filePath[j]='\\';	/* append backslash to filePath */
#endif
					break;
				case 'S':
					if (argList[i][2])
						randSeed = atol(argList[i]+2);
					else
						randSeed=(DWORD) time(NULL);	/* randomize */
					break;
				case 'T':
					if (argList[i][2])
						timeIterCnt = atoi(argList[i]+2);
					else
						timeIterCnt = 32;
					break;
				case 'V':
					verify=1;	/* don't generate files.  Read&verify them */
					if (argList[i][2]=='+')
						verbose=1;
					if (argList[i][2]=='-')
						doTableTest=0;
					if (toupper(argList[i][2])=='Q')
						quietVerify=1;
					break;
				case '6':
					Debug6805();
					exit(1);
					break;
				}
		else
			GiveHelp();
		}

#ifdef USE_ASM
	if (useAsm & 7)	moduleName="Assembler ";
#endif

	printf("%s%d.%s: %s%s [%ld bytes,%s].\n",CompilerName,8*sizeof(int),
#ifdef USE_ASM
		   (get_cpu_type() == 5) ? "Pentium" : "Pro/II",
#else
			(sizeof(int) == 2) ? "x86" : "CPU???",
#endif
		   moduleName,modeString,TwofishCodeSize(),
#if LittleEndian
		   "little-endian"
#else
		   "big-endian"
#endif
		  );

	SetRand(randSeed);					/* init pseudorandom generator for testing */

	if (testCnt)
		AES_Sanity_Check(testCnt);		/* test API compliance, self-consistency */

	if ((timeIterCnt) && (!verify))
		{
		TimeOps(timeIterCnt);
		exit(0);
		}

#ifdef TEST_EXTERN
	Test_Extern();
	exit(0);
#endif

	if (doIntermediate)
		{
		AES_Test_Intermediate("ecb_ival.txt"); /* intermediate value test */
		return 0;
		}
		

	AES_Test_VK("ecb_vk.txt");			/* Variable key  KAT */
	AES_Test_VT("ecb_vt.txt");			/* Variable text KAT */
	AES_Test_Intermediate("ecb_ival.txt"); /* intermediate value test */

	if (!quietVerify)
	printf("%s MCT Generation : %d,%d.\n",
		   ((MCT_INNER == mctInner) && (MCT_OUTER == mctOuter)) ? "Full" : " *** Partial",
		   mctOuter,mctInner);
	AES_Test_CBC_E_MCT("cbc_e_m.txt");	/* Monte Carlo test for CBC encryption */
	AES_Test_CBC_D_MCT("cbc_d_m.txt");	/* Monte Carlo test for CBC decryption */
	AES_Test_ECB_E_MCT("ecb_e_m.txt");	/* Monte Carlo test for ECB encryption */
	AES_Test_ECB_D_MCT("ecb_d_m.txt");	/* Monte Carlo test for ECB decryption */

	if (doTableTest)
		AES_Test_TBL("ecb_tbl.txt");		/* Table test */
	else
		if (!quietVerify) printf("WARNING: Skipping ecb_tbl.txt verification\n");

	if (verify)
		printf("*** All files verified OK ***\n");
	
	if (timeIterCnt)
		TimeOps(timeIterCnt);

	return 0;
	}
