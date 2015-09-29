
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Technical Details: Building the Software</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:tdk@doxbox.eu">
<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

## Technical Details: Building the Software

LibreCrypt/LibreCrypt Explorer comes in a number of parts:

* LibreCrypt:
	1. A front-end GUI, written in Delphi
	2. [A number of kernel drivers, written in C](#building_kernel_drivers)
* LibreCrypt Explorer:
	1. A front-end GUI, written in Delphi
	2. A number of DLLs, written in C, built for Win32 and Win64
* Test Apps
	1. Some command line apps mostly for testing of drivers

* A number of command line decryption utilities, also written in C.

1. [LibreCrypt](#level_3_heading_1) 
1. [LibreCrypt Explorer](#level_3_heading_3)
1. [Building the Command Line Decryption Utilities](#building_command_line_decryption_utilities)
1. [Building the Command Line Test Apps](#building_command_line_test_apps)
1. [Building the Kernel Drivers](#building_kernel_drivers)
1. [Signing the Binaries](#signing_binaries)
1. [Additional Notes](#level_3_heading_6)


* * *
 
<A NAME="level_3_heading_1">
### LibreCrypt
</A>

<A NAME="level_4_heading_1">
#### Building the GUI
</A>

	
  This is a description for Delphi newbies of the basic steps involved in compiling the LibreCrypt GUI.

  To build the GUI, the following software is required:

  *   Delphi (Embarcadero Delphi XE2 or later, is recommended. Earlier/later versions may possibly be used, but are untested)

  * The SDeanUtilsXE package, included in the project source.
  * (Optional) GNU gettext for Delphi (dxgettext), available (free) from: [http://dybdahl.dk/dxgettext/](http://dybdahl.dk/dxgettext/) (This package adds support for language translations)
  *	The FastMM memory manager, from (http://sourceforge.net/projects/fastmm/](http://sourceforge.net/projects/fastmm/). This wipes memory after use.
  The binary release of this software was built with Embarcadero Delphi XE2.

  1.  Open the SDeanUtilsXE package
  1.  Build the package
  2.  Install the package
  3.  for each component, ensure that the correct path to the component is added to your Delphi environment ("Tools | Environment Options...", "Library" tab)
  2.  Add the path to the modified Delphi files included in SDeanComponents to fix various bugs relating to Delphi's Windows Vista support to the top of Delphi's standard library paths. (This step probably won't be needed with later versions of Delphi, and shouldn't be carried out with older versions of Delphi, which will have different source)
  * Open the LibreCrypt project, "LibreCrypt.dproj" under .\src\PC\gui\main

  3. If you have the dxgettext software installed (see above), ensure that the compiler directive "\_DXGETTEXT" is set. Otherwise, make sure that this compiler directive is _not_ set.

  * Build the project.

  * You should now find a file called "LibreCrypt.exe" in the directory '.\bin\PC' 
  
  You have now successfully built the GUI frontend

  If required, the compiler definition "FREEOTFE\_TIME_CDB\_DUMP" may be set, in which case the time taken to dump a CDB ("Tools | Critical data block | Dump to human readable file...") will be shown after the dump completes.
  <A NAME="level_4_heading_2">
##### translation
</A>
It is not necessary to do a full install of dxgettext to build with i18n support. Instead you can just add the file gnugettext.pas to the project.
In order to update any translations, however dxgettext must be installed.

After any changes to the strings in the GUI, the translation files (.po) should be updated, and compiled into .mo files.
The process for updating translations is:
	*	Extract the default.po file from the source or executable.
	* Remove any strings that need not be translated  
	*	Update or create each locale .po file with the translations
	*	Compile the .po files into .mo files.
	
Extracting the strings from the sources is preferred. 
Updating or creating the .po files is done by the translators.
For the convenience of translators, the English .po file is distributed with the executable, although it is not used.
The ignore.po file should also be regenerated if there are a lot of changes to the source - see the msgmkignore command

######	Extract the .po file from the source
* Install dxgettext if not already installed. There is no need to integrate with Delphi.
* Open a command prompt at the root LibreCrypt directory
* run the following command `"c:\Program Files (x86)\dxgettext\dxgettext.exe" -r -b src/PC/gui/ --delphi -o bin/PC/locale/en/LC_MESSAGES`
	This assumes that dxgettext is installed at `c:\Program Files (x86)\dxgettext\`
	Note that the relative paths use a forward slash as a path separater
	This command will give warnings for code like : `_(self.Caption)` - however these can be ignored as these strings will be found in the .dfm

######	Extract the .po file from the executable
* Install dxgettext if not already installed. 
* Open a command prompt at the root LibreCrypt directory
* run the following command `"c:\Program Files (x86)\dxgettext\dxgettext.exe" bin/PC/LibreCrypt.exe -o bin/PC/locale/en/LC_MESSAGES`
	This assumes that dxgettext is installed at `c:\Program Files (x86)\dxgettext\`
	Note that the relative paths use a forward slash as a path separater
	
##### Remove ignoreable strings	
* Open a command prompt at the root LibreCrypt directory
* run the following commands:
	+ move bin\PC\locale\en\LC_MESSAGES\default.po bin\PC\locale\en\LC_MESSAGES\full.po
	+ `"c:\Program Files (x86)\dxgettext\msgremove.exe" bin/PC/locale/en/LC_MESSAGES/full.po -i src/PC/gui/translation/ignore.po -o bin/PC/locale/en/LC_MESSAGES/default.po`	

###### Compile the .po files into .mo files.
* Install dxgettext if not already installed. 
* Open a command prompt at the root LibreCrypt directory
* run the following command, replacing 'de' with the language code, for each language `"c:\Program Files (x86)\dxgettext\msgfmt.exe" 	  src/PC/gui/translation/de/default.po -o bin/PC/locale/de/LC_MESSAGES/default.mo`
	This assumes that dxgettext is installed at `c:\Program Files (x86)\dxgettext\`
	Note that the relative paths use a forward slash as a path separater
It is also possible to do this from Windows Explorer - see (translations)[translations.md].
There is a bat file under 'tools' - `update_mo_files.bat` -  that compiles all the .mo files. 

<A NAME="building_kernel_drivers">
#### Building the Kernel Drivers
</A>

The kernel mode drivers implement the actual hash, encryption/decryption and main FreeOTFE drivers.

To build these drivers, the following software is required:

* Microsoft Visual Studio 2010 (older versions may well be used, changing "vcvarsall" to "vcvars32", and similar changes)
* If using an older version of MS Visual Studio, the MS Windows SDK (February 2003 version) is also needed 
* The MS Windows DDK (WDK 7600.16385.1)

The binary release of this software was built with Microsoft Visual Studio 2010 Professional Edition.

At time of writing, the MS Windows SDK can be downloaded from the Microsoft WWW site. The MS Windows DDK (also called the 'WDK') is available as a download cd image, and can be ordered from the Microsoft WWW site as a free CD, for the cost of delivery.

If you are unable to source the exact versions listed above, earlier versions may well be substituted, although I cannot guarantee success. Later versions should operate correctly. This list describes the environment used to build the release version of LibreCrypt.
The versions used are: 
<TABLE>
<TBODY>
<TR><TD>Visual Studio          </TD>									<TD>2010 Professional</TD>    </TR>
<TR><TD>Windows Driver Development Kit (WinDDK) </TD>	<TD>7600.16385.1</TD>    </TR>
<TR><TD>lib tomcrypt           </TD>									<TD>1.17</TD>    </TR>
<TR><TD>Gladman library          </TD>								<TD>downloaded on 04/12/05</TD>    </TR>
<TR><TD>Twofish library          </TD>								<TD>Version  1.00		April 1998</TD>    </TR>
<TR><TD>dxgettext          </TD>											<TD>GNU gettext for Delphi, C++ Builder and Kylix 1.2 beta</TD>    </TR>
</TBODY>
</TABLE>

<A NAME="level_5_heading_1">
##### Setting up the Build Environment
</A>

<A NAME="level_6_heading_1">
###### Installation and Configuration of MS Build Environment
</A>

The following list comprehensively describes the configuration used to build the binary release of LibreCrypt. Feel free to adjust according to taste - a number of the options listed are not necessary, and are only included for completeness...

1.  Install Visual Studio 2010
1. Put a copy of "vcvarsall.bat" into one of the directories in your path

2.  Configure the VS editor:

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
   		+ Needed if the build .BAT files (see later) use "chk WXP" - if that's skipped it'll default to WNET (windows .NET)
    * Build Environment\Windows 2000 Headers</li>
    * Build Environment\Windows 2000 Build Environment</li>
			+ Needed if the build .BAT files (see later) use "chk &lt;something&gt;" - if that's skipped it'll default to WNET (windows .net)
    

<A NAME="level_6_heading_2">

###### LibreCrypt Build Configuration
</A>

1.  Edit "setup\_env\_common.bat" (located under src\drivers\Common\bin), and ensure that the following variables are set appropriately:
 		Variables in bold will probably need to be manually changed, depending on the user's setup  

  <TABLE>
    <TBODY>
      <TR> <TH>Variable  </TH> <TH>Description </TH> <TH>Default value </TH> </TR>      
      <TR> <TD>FREEOTFE_CPU</TD>  <TD>The target platform to build the drivers for. Set to either x86 or amd64, only necesary if build_all_amd and build_all_x86 are not used</TD>  <TD>amd64</TD> </TR>      
      <TR> <TD>FREEOTFE\_DEBUG</TD> 	<TD>Build type flag; set to 1 for debug build, or 0 for release</TD> <TD>0</TD> </TR>
      <TR> <TD>FREEOTFE\_TARGET</TD> 	<TD>Target OS to build for; e.g. WXP/W2K/WNET; note that W2K builds will not operate correctly under Windows XP (e.g. when formatting a volume)</TD> <TD>WXP</TD> </TR>
      <TR> <TD>PROJECT\_DRIVE</TD> 		<TD>The drive on which you have stored the LibreCrypt source </TD> <TD>&lt;The drive the config batch file is stored on&gt;</TD> </TR>
      <TR> <TD>PROJECT\_DIR</TD> 			<TD>The full drive and path where the "drivers" directory is located</TD> <TD>_&lt;see file&gt;_</TD> </TR>      
      <TR> <TD>BIN_OUTPUT_DIR</TD> <TD>The path where the built drivers will be copied to. This directory will automatically be created if it does not already exist.</TD> <TD>/&lt;"bin" directory at the same level as the main "src" directory&gt;/</TD> </TR> 
      <TR>  <TD>VCVARSALL</TD> <TD>The full path and filename to Visual Studio's VCVARSALL.BAT (or vcvar32.bat, if building with an old version)</TD> <TD>"C:\PROGRA~2\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"</B></TD> </TR> 
      <TR> <TD>MSSDK\_DIR</TD> 				<TD>The directory in which you installed the Microsoft SDK. Set to 0 if not needed (e.b. Visual Studio 8.0 and later)</TD> <TD>C:\MSSDK</TD> </TR>
      <TR> <TD>MSDDK\_DIR</TD> 				<TD>The directory in which you installed the MS DDK</TD> <TD>C:\Apps\WinDDK\7600.16385.1</TD> </TR>
    </TBODY>
  </TABLE>

2. Edit "setup\_env\_driver.bat" (in the same directory), and ensure that "SETENV.BAT" is called with the parameters appropriate to the type of build you wish to create, and that "FREEOTFE\_OUTPUT\_DIR" is set to the appropriate directory under the source directories where the build executable places the files it creates (this shouldn't be needed as it will happen automatically if the above are configured correctly)
  

<A NAME="level_6_heading_3">
###### 3rd Party Source Code
</A>

Some of the FreeOTFE drivers (the hash/encryptions drivers in particular) are dependant on certain 3rd party software being installed. LibreCrypt's source code comes complete with 3rd party included in GitHUb under the"src\3rd\_party" directory and should be preconfigured, ready for use.

_Alternatively_, you may wish to download this 3rd party source from the original authors in order to verify the integrity of this software. For this reason, details of where this software was obtained from are included in the above directory.

Please note that should choose the latter option, it is important that you review the individual driver notes (see separate driver directories; "\_notes.txt" files) to ensure that this software is configured correctly. Additionally, you may well have to modify the "my\_build\_sys.bat" files, directing them to the location where you installed said 3rd party source code, as the build process requires that certain files are copied over into the LibreCrypt src directories. (Annoying, but this is a requirement of the MS "build.exe" command)

The LibTomCrypt source in particular had minor configuration changes to tomcrypt\_cfg.h and tomcrypt\_custom.h; please compare the original source (a copy of its release ZIP file is stored under src\3rd\_party\libtomcrypt) with the modified version (uncompressed in a directory under this one)

<A NAME="level_5_heading_2">
##### Building the LibreCrypt Drivers (.sys files)
</A>

Either:

  1.  Open ".\src\PC\drivers\FreeOTFE.sln" using Visual Studio
  2.  Rightclick on each project in turn, and select "Build"

or:

	1.	Run: ...\src\PC\drivers\build_ALL.bat

or:

	1. Edit the file `.\src\PC\drivers\Common\bin\setup_env_common.bat`, you will need to update these lines:
	
		* `set VCVARSALL="C:\PROGRA~2\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"`
			this should be set to point to 'vcvarsall.bat', in '8.3' filename form
		*	`set PROJECT_DRIVE=P:`
			The drive where the source code is, alternatively a 'subst' command can be used to point p: the project directory (e.g. the source should be under P:\src\)
		* `set PROJECT_BASE_DIR=%PROJECT_DRIVE%\`
			set this to the project directory
		* `set MSDDK_DIR=C:\Apps\WinDDK\7600.16385.1`
			The DDK dir
	1. and either
		2. 
			
			3. Run: `.\src\PC\drivers\build_all_amd.bat` 
			3. open a new DOS command box and run `.\src\PC\drivers\build_all_x86.bat`
		or
		2. 
			
			3.  Enter each of the separate driver directories in turn and launch each project's "my\_build\_sys.bat" 
			
	
	In either case, the binaries are built into the `.bin\PC\<platform>\` directory. 
  After reaching this stage, you should have successfully built your own version of the LibreCrypt drivers

Notes:

  1.  If FREEOTFE\_TARGET is set to W2K, the resulting binary may not operate correctly under MS Windows XP as a number of functions what are only needed under Windows XP and later are #ifdef'd out. As a result, a "W2K" binary may not operate correctly under Windows XP (e.g. trying to format a volume may result in... Nothing happening). If you want a binary which will operate under _both_ Windows 2000 and Windows XP, set this to WXP. 
  2.  Windows XP migrated a couple of the previous Windows 2000 macros to be functions. In order to allow the above "WXP" builds to work under Windows 2000, "IFSRelated.h" includes a copy of these macros, and uses them regardless - see comments in code for an explanation.


* * *
 
<A NAME="level_3_heading_3">
### LibreCrypt Explorer
</A>

<A NAME="level_4_heading_3">
#### Building the GUI
</A>


This is a description for Delphi newbies of how to compile the LibreCrypt Explorer GUI.

To build the GUI, the following software is required:

*   Delphi (Embarcadero Delphi XE2 or later, though previous versions can probably be used with minimal changes, though wouldn't look as nice under Windows Vista and above)
*   The SDeanComponents package (v2.00.00 or later)
* (Optional) GNU gettext for Delphi (dxgettext), available (free) from: [http://dybdahl.dk/dxgettext/](http://dybdahl.dk/dxgettext/) (This package adds support for language translations) (see below). It is not necessary to install this to simply rebuild if no strings have changed.

The binary release of this software was built with Embarcadero Delphi XE2.

  1.  With the package SDeanUtilsXE

    1.  Build the package
    2.  Install the package
    3.  Ensure that the correct path to each component is added to your Delphi environment ("Tools | Environment Options...", "Library" tab)
    
    Note: Some components in this package are forms containing others in the same package. So, if you open a form in the package before installing it, you may see a message saying 'Field X does not have a corresponding component. Remove the declaration?'. If you do, click 'Cancel', clicking 'yes' will result in the component being deleted from the '.pas' file. 
  3. Open the LibreCrypt Explorer project ("LibreCryptExplorer.dproj" under .\src\PC\gui\explorer)
  3.  If you have the dxgettext software installed (see above), ensure that the compiler directive "\_DXGETTEXT" is set. Otherwise, make sure that this compiler directive is _not_ set.
  4 Build the application.
  You should now find a file called "LibreCryptExplorer.exe" in the directory above the "src" directory
  
You have now successfully built the GUI frontend

##### compiler directives

* FREEOTFE\_MAIN 		defined for main GUI, not explorer
* FREEOTFE\_EXPLORER defined for doxbox explorer, not main gui
* \_DXGETTEXT 				see above

##### gettext
i18n is done using dxgettext, compatible with GNU gettext.
Unfortunately the project was hosted on Berlios.de, which is now closed as a hosting site, and the project appears abondoned.
An older version of the project was hosted on sourceforge. Fortunately a patch was submited to the sourceforge forum, containing the latest code.
In order to build the source with i18n support, only a file gnugettext.pas is needed. For convenience this is part of the github project.
To retrive this, download dxgettext.7z from TODO and extract the file from the .\dxgettext\dxgettext\sample\ directory.
To run the other functions of dxgettext, viz extracting srings from the project and building .mo files:

* install dxgettext from sourceforge, choosing 'no' to 'integate with IDE'
* apply the patch from dxgettext.7z
* run the tools from the command line

<A NAME="level_4_heading_4">
#### Building the DLL drivers
</A>

To build the DLLs used by LibreCrypt Explorer:

1. Open "LibreCryptDLLs.sln" under `.\src\PDA\` using Visual Studio 2010
1. Set the build configuration within Visual Studio to "Release" - "Win32" or "Release" - "Win64"
1. Right-click on each project in turn, and select "Rebuild". Note: Don't bother building the "GUI" project; at present, this can only be built for the Windows Mobile platform.

The binaries built are put into the directories `.\bin\PC\DLLs\<config>\<platform>\`.

* * *
 
<A NAME="building_command_line_decryption_utilities">
### Building the Command Line Decryption Utilities
</A>

_Note: The development of the command line decryption utilities has ceased. This functionality has been superceded with the development of LibreCrypt Explorer and the test projects_


To build the command line decryption utilities, the following software is required:

  *   A C compiler (Visual Studio 2010 was used to write and test this software)
Please follow the following steps:

  1.  Install and configure up the build environment, as described as per building the backend drivers, _you may omit the SDK and DDK_.
		* Modify the software as appropriate for your test
		* Please see the command line decryption utility documentation

  2.  Launch the relevant "my\_build\_exe.bat" file 
  
The executable should be built in the same directory.


* * *
 
<A NAME="building_command_line_test_apps">
### Building the Command Line Test Apps
</A>

_Note: These are not included in the release and are for testing purposes_
_Note: These are work in progress_

To build the command line Test Apps, the following software is required:

  *   A C compiler (Visual Studio 2010 was used to write and test this software)
Please follow the following steps:

  1.  Install and configure up the build environment, as described as per building the backend drivers, _you may omit the SDK and DDK_.
		* Open the test project(s) under .\src\PDA\TEST_PROJS\*
		* Please see the command line test app documentation
	or
  2.  Launch the relevant "my\_build\_exe.bat" file 
  
The executable should be built in the same directory.
These executables are built using the same code as the DLLs but with different preprocessor directives, they are used for testing the drivers and DLLs. 

***

All the projects have been built under a directory "P:\", but whereever possible relative paths have been used. In case of errors run the command `subst p: <path to project directory>` and retry
  
* * * 
<A NAME="signing_binaries">
### Signing the Binaries
</A>

To sign the LibreCrypt binary files (.exe, .dll and .sys files), the procedure is pretty much as described at: [Pantaray Research](http://www.pantaray.com/signcode.html#create_SPC) web site

At present, LibreCrypt is signed using a self-signed certificate; the full procedure used is as follows:

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
* FreeOTFE\_TEST\_APP

depending on what is being built.

This header file should be #included at the start of _every_ file which uses any of these defines. (Yes, this is obvious - but easily overlooked!)



