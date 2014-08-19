
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: Building the Software</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:tdk@doxbox.eu">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://doxbox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_Open-Source disk encryption for Windows_
</SPAN>

## Technical Details: Building the Software

DoxBox/DoxBox Explorer come in a number of parts:

* DoxBox:
	1. A front-end GUI, written in Delphi
	2. A number of kernel drivers, written in C
* DoxBox Explorer:
	1. A front-end GUI, written in Delphi
	2. A number of kernel drivers, written in C, built for Win32

* A number of command line decryption utilities, also written in C.


1. [DoxBox](#level_3_heading_1) 
1. [DoxBox Explorer](#level_3_heading_3)
1. [Building the Command Line Decryption Utilities](#level_3_heading_4)
1. [Signing the Binaries](#level_3_heading_5)
1. [Additional Notes](#level_3_heading_6)




* * *
 
<A NAME="level_3_heading_1">
### DoxBox
</A>

<A NAME="level_4_heading_1">
#### Building the GUI
</A>


This is a description for Delphi newbies of the basic steps involved in compiling the DoxBox GUI.

To build the GUI, the following software is required:

*   Delphi (Embarcadero Delphi XE2 or later, is ecommended. Previous versions can probably be used with minimal changes, though wouldn't look
as nice under Windows Vista)

* The SDeanComponents package (v2.00.00 or later)  
* (Optional) GNU gettext for Delphi (dxgettext), available (free) from: [http://dybdahl.dk/dxgettext/](http://dybdahl.dk/dxgettext/) (This package adds support for language translations)

The binary release of this software was built with Embarcadero Delphi XE2.

  1.  Open the SDeanUtilsXE package
    1.  Build the package
    2.  Install the package
    3.  for each component, ensure that the correct path to the component is added to your Delphi environment ("Tools | Environment Options...", "Library" tab)
  2.  Add the path to the modified Delphi files included in SDeanComponents to fix various bugs relating to Delphi's Windows Vista support to the top of Delphi's standard library paths. (This step probably won't be needed with later versions of Delphi, and shouldn't be carried out with older versions of Delphi, which will have different source)
  * Open the DoxBox project ("FreeOTFE.dpr")

  3. If you have the dxgettext software installed (see above), ensure that the compiler directive "\_DXGETTEXT" is set. Otherwise, make sure that this compiler directive is _not_ set.

  * Build the application.

* You should now find a file called "DoxBox.exe" in the directory
above the "src" directory

You have now successfully built the GUI frontend!

If required, the compiler definition "FREEOTFE\_TIME_CDB\_DUMP" may be set, in which case the time taken to dump a CDB ("Tools | Critical data block | Dump to human readable file...") will be shown after the dump completes.


<A NAME="level_4_heading_2">
#### Building the Kernel Drivers
</A>

The kernel mode drivers implement the actual hash, encryption/decryption and main FreeOTFE drivers.

To build these drivers, the following software is required:

<UL>
  
* Microsoft Visual Studio 2008 (older versions may well be used, changing "vcvarsall" to "vcvars32", and similar changes)
<UL>
  
* If using an older version of MS Visual Studio, the MS Windows SDK (February 2003 version) is also needed
</UL>
  
* The MS Windows WDK (WDK for Server 2008 v6001.18001)
</UL>

The binary release of this software was built with Microsoft Visual Studio 2010 Professional Edition.

At time of writing, the MS Windows SDK (also called the 'WDK') can be downloaded from the Microsoft WWW site. The MS Windows DDK (also called the 'WDK') is available as a download cd image, and can be ordered from the Microsoft WWW site as a free CD, for the cost of delivery.

It should be noted that if you are unable to source the exact versions listed above, earlier versions may well be substituted, although I cannot guarantee success. Later versions should operate correctly. The above list describes the development environment as used to build the binary release of DoxBox.
versions used: 
<TABLE>
<TBODY>
<TR><TD>Visual Studio          </TD><TD>2010 Professional</TD>    </TR>
<TR><TD>Windows Driver Development Kit (WinDDK) </TD><TD>7600.16385.1</TD>    </TR>
<TR><TD>lib tomcrypt           </TD><TD>1.17</TD>    </TR>
<TR><TD>Gladman library          </TD><TD>downloaded on 04/12/05</TD>    </TR>
<TR><TD>Twofish library          </TD><TD>Version  1.00		April 1998</TD>    </TR>
</TBODY>
</TABLE>

<A NAME="level_5_heading_1">
##### Setting up the Build Environment
</A>

<A NAME="level_6_heading_1">
###### Installation and Configuration of MS Build Environment
</A>

The following list comprehensively describes the configuration used to build the binary release of DoxBox. Feel free to adjust according to taste - a number of the options listed are not necessary, and are only included for completeness...

1.  Install VC++
1. Put a copy of "vcvarsall.bat" into one of the directories in your path

2.  Configure the VC++ editor:

	*   To use spaces, not tabs	
	*   To indent braces

3.  Install the MS Windows SDK with the following options:
	
	* Install in C:\MSSDK	
	* Install the "Core SDK"	
	* **Then** install the debugging tools for windows
	  
	* Do **not** register environment variables (we'll use "Setenv.bat" from the command line)
	* Install the MS Windows DDK with the following options:
	

    * Install in C:\WINDDK\3790
    * Include the "Illustrative Driver Samples"</li>
    * Include the "Input Samples"</li>
    * Include the "Storage Samples"</li>
    * Include the "Virtual Device Driver Samples"</li>
    * Include the "WDM Samples"</li>
    * Build Environment\Windows Driver Development Kit AMD64 Additional Build Tools</li>
    * Build Environment\Windows Server 2003 AMD64 Libraries</li>
    * Build Environment\Windows XP Headers</li>
    * Build Environment\Windows XP x86 Libraries</li>
    * Build Environment\Windows XP IA86 Libraries</li>
   		+ Needed if the build .BAT files (see later) use "chk WXP" - if that's skipped it'll default to WNET (windows .NET)</li>
    * Build Environment\Windows 2000 Headers</li>
    * Build Environment\Windows 2000 Build Environment</li>
			+ Needed if the build .BAT files (see later) use "chk &lt;something&gt;" - if that's skipped it'll default to WNET (windows
.net)
    

<A NAME="level_6_heading_2">

###### DoxBox Build Configuration
</A>

1.  Edit "setup\_env\_common.bat" (located under src\drivers\Common\bin), and ensure that the following variables are set appropriately:
  

  <TABLE>
    <TBODY>
      <TR> <TH>Variable<br> </TH> <TH>Description </TH> <TH>Default value </TH> </TR>
      <TR> <TD>FREEOTFE\_DEBUG</TD> 	<TD>Build type flag; set to 1 for debug build, or 0 for release</TD> <TD>0</TD> </TR>
      <TR> <TD>FREEOTFE\_TARGET</TD> 	<TD>Target OS to build for; e.g. WXP/W2K/WNET; note that W2K builds will not operate correctly under Windows XP (e.g. when formatting a volume)</TD> <TD>WXP</TD> </TR>
      <TR> <TD>PROJECT\_DRIVE</TD> 		<TD>The drive on which you have stored the FreeOTFE source</TD> <TD>E:</TD> </TR>
      <TR> <TD>PROJECT\_DIR</TD> 			<TD>The full drive and path where the "drivers" directory is located</TD> <TD>_&lt;see file&gt;_</TD> </TR>
      <TR> <TD>MSSDK\_DIR</TD> 				<TD>The directory in which you installed the MS SDK</TD> <TD>C:\MSSDK</TD> </TR>
      <TR> <TD>MSDDK\_DIR</TD> 				<TD>The directory in which you installed the MS DDK</TD> <TD>C:\WINDDK\3790</TD> </TR>
    </TBODY>
  </TABLE>

2. Edit "setup\_env\_driver.bat" (in the same directory), and ensure that "SETENV.BAT" is called with the parameters appropriate to the type of build you wish to create, and that "FREEOTFE\_OUTPUT\_DIR" is set to the appropriate directory under the source directories where the build executable places the files it creates (this shouldn't be needed as it will happen automatically if the above are configured correctly)
  

<A NAME="level_6_heading_3">
###### 3rd Party Source Code
</A>

Some of the FreeOTFE drivers (the hash/encryptions drivers in particular) are dependant on certain 3rd party software being installed. DoxBox's source code comes complete with 3rd party included in the"src\3rd\_party" directory and should be preconfigured, ready for use.

_Alternatively_, you may wish to download this 3rd party source from the original authors in order to verify the integrity of this software. For this reason, details of where this software was obtained from are included in the above directory.

Please note that should choose the latter option, it is important that you review the individual driver notes (see separate driver directories; "\_notes.txt" files) to ensure that this software is configured correctly. Additionally, you may well have to modify the "my\_build\_sys.bat" files, directing them to the location where you installed said 3rd party source code, as the build process requires that certain files are copied over into the DoxBox src directories. (Annoying, but this is a requirement of the MS "build.exe" command)

The LibTomCrypt source in particular had minor configuration changes to tomcrypt\_cfg.h and tomcrypt\_custom.h; please compare the original source (a copy of its release ZIP file is stored under src\3rd\_party\libtomcrypt) with the modified version (uncompressed in a directory under this one)

<A NAME="level_5_heading_2">
##### Building the DoxBox Drivers
</A>

Either:

  1.  Open "FreeOTFE.sln" using Visual C++
  2.  Rightclick on each project in turn, and select "Build"
or:

  1.  Enter each of the separate driver directories in turn and launch each project's "my\_build\_sys.bat" In either case, a copy of the binary which is built will be copied into the directory above your "src" directory. 
  After reaching this stage, you should have successfully built your own version of the FreeOTFE drivers!
<P>
Notes:

  1.  If FREEOTFE\_TARGET is set to W2K, the resulting binary may not operate correctly under MS Windows XP as a number of functions what are only needed under Windows XP and later are #ifdef'd out. As a result, a "W2K" binary may not operate correctly under Windows XP (e.g. trying to format a volume may result in... Nothing happening). If you want a binary which will operate under _both_ Windows 2000 and Windows XP, set this to WXP. 
  2.  Windows XP migrated a couple of the previous Windows 2000 macros to be functions. In order to allow the above "WXP" builds to work under Windows 2000, "IFSRelated.h" includes a copy of these macros, and uses them regardless - see comments in code for an explanation.


* * *
 
<A NAME="level_3_heading_3">
### DoxBox Explorer
</A>

<A NAME="level_4_heading_3">
#### Building the GUI
</A>


This is a description for Delphi newbies of the basic steps involved in compiling the DoxBox Explorer GUI.

To build the GUI, the following software is required:

*   Delphi (Embarcadero Delphi XE2 or later, though previous versions can probably be used with minimal changes, though wouldn't look as nice under Windows Vista)
*   The SDeanComponents package (v2.00.00 or later)
* (Optional) GNU gettext for Delphi (dxgettext), available (free) from: [http://dybdahl.dk/dxgettext/](http://dybdahl.dk/dxgettext/) (This package adds support for language translations)

The binary release of this software was built with Embarcadero Delphi XE2.

  1.  With the package SDeanUtilsXE

    1.  Build the package
    2.  Install the package
    3.  Ensure that the correct path to each component is added to your Delphi environment ("Tools | Environment Options...", "Library" tab)
  2.  Add the path to the modified Delphi files included in SDeanComponents to fix various bugs relating to Delphi's Windows Vista support to the top of Delphi's standard library paths. (This step probably won't be needed with later versions of Delphi, and shouldn't be carried out with older versions of Delphi, which will have different source)* Open the DoxBox Explorer project ("FreeOTFEExplorer.dpr")
  3.  If you have the dxgettext software installed (see above), ensure that the compiler directive "\_DXGETTEXT" is set. Otherwise, make sure that this compiler directive is _not_ set.
  4 Build the application.
  You should now find a file called "DoxBox.exe" in the directory above the "src" directory
You have now successfully built the GUI frontend!

<A NAME="level_4_heading_4">
#### Building the DLL drivers
</A>

To build the DLLs used by DoxBox Explorer:

1. Open "FreeOTFE4PDA.sln" using Visual C++
1. Set the build configuration within Visual C++ to "Release" - "Win32"
1. Right-click on each project in turn, and select "Rebuild". Note: Don't bother building the "GUI" project; at present, this can only be built for the Windows Mobile platform.



A copy of the binary which is built will be copied into the directory above your "src" directory.

* * *
 
<A NAME="level_3_heading_4">
### Building the Command Line Decryption Utilities
</A>

_Note: The development of the command line decryption utilities has ceased. This functionality has been superceded with the development of DoxBox Explorer_

To build the command line decryption utilities, the following software is required:

  *   A C compiler (Visual C++ .NET was used to write and test this software)
Please follow the following steps:

  1.  Install and configure up the build environment, as described as per building the backend drivers, _you may omit the SDK and DDK_.
		* Modify the software as appropriate for your test
		* Please see the command line decryption utility documentation

  2.  Launch the relevant "my\_build\_exe.bat" file 
  
The executable should be built in the same directory.



* * * 
<A NAME="level_3_heading_5">
### Signing the Binaries
</A>

To sign the DoxBox binary files (.exe, .dll and .sys files), the procedure is pretty much as described at: [Pantaray Research](http://www.pantaray.com/signcode.html#create_SPC) web site

At present, DoxBox is signed using a self-signed certificate; the full procedure used is as follows:

1. Install Visual Studio
2. From a command prompt, run "vcvarsall" (all commands detailed below should be executed from this command prompt)
3. Create a private certificate:

		makecert.exe -sv tdk.pvk -n "E=tdk@doxbox.eu,CN=Sarah Dean" tdk.cer

this should create two files: tdk.pvk and tdk.cer

4. Create a test software publisher certificate (SPC):

				cert2spc.exe tdk.cer tdk.spc

	to create tdk.spc. (This file would normally be supplied by a CA, if purchased)

5. Create a personal information file

        pvk2pfx -pvk tdk.pvk -spc tdk.spc -pfx tdk.pfx -f /pi <pvk password> /po <pfx password>

Where:

+ _&lt;pvk password&gt;_ is the password used when generating the .pvk file with makecert.txt
+ _&lt;pfx password&gt;_ is the password you wish to use for securing the new .pfx file


6. Sign each of binary files:

	       signtool.exe sign /f tdk.pfx /p <pfx password> /v /t http://timestamp.verisign.com/scripts/timstamp.dll <filename> 


Where:
	_&lt;pfx password&gt;_ is the password used when generating the .pfx file with pvk2pfx


The URL specified is a time stamping service (Verisign's in this case).


* * * 
<A NAME="level_3_heading_6">
### Additional Notes
</A>

When building the C code, FreeOTFEPlatform.h automatically #defines one of the following:

* FreeOTFE\_PC\_DRIVER
* FreeOTFE\_PC\_DLL
* FreeOTFE\_PDA

depending on what is being built.

This header file should be #included at the start of _every_ file which uses any of these defines. (Yes, this is obvious - but easily overlooked!)



