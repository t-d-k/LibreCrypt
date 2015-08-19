<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">

<TITLE>Command Line Interface</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>

<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

<SPAN class="tip">
The latest version of this document can be found at the [LibreCrypt project site](https://github.com/t-d-k/librecrypt)
</SPAN>

## Command Line Interface

LibreCrypt supports passing parameters via a command line interface.



* * *

<A NAME="level_4_heading_1">

#### Synopsis
</A>

> <pre>
> LibreCrypt.exe /mount /volume <span style="font-style: italic;">&lt;volume&gt;</span> [/freeotfe | /linux] [/readonly] [/drive <span style="font-style: italic;">&lt;drive letter&gt;</span>]
>              [/offset <span style="font-style: italic;">&lt;offset&gt;</span>] [/nocdbatoffset]
>              [/keyfile <span style="font-style: italic;">&lt;keyfile&gt;</span>] [/keyfileisascii] [/keyfilenewline {CRLF|LF|CR}]
>              [/lesfile <span style="font-style: italic;">&lt;LES file&gt;</span>]
>              [/saltlength <span style="font-style: italic;">&lt;saltlength&gt;</span>] [/keyiterations <span style="font-style: italic;">&lt;keyiterations&gt;</span>]
>              [/password <span style="font-style: italic;">&lt;password&gt;</span>] [/silent] [/noexit] [/minimize]
>              [/settings <span style="font-style: italic;">&lt;settings file&gt;</span>]
> 
> LibreCrypt.exe /dismount {<span style="font-style: italic;">&lt;drive letter&gt;</span>[:] | all} [/force]
> 
> LibreCrypt.exe /count
> 
> LibreCrypt.exe /create
> 
> LibreCrypt.exe /portable {start | on | 1 | stop | off | 0 | toggle} [/silent]
> 
> LibreCrypt.exe /drivercontrol gui
> LibreCrypt.exe /drivercontrol count [/type {total | portable | installed}]
> LibreCrypt.exe /drivercontrol install /filename {<span style="font-style: italic;">&lt;driver filename&gt;</span> | all} [/silent]
> LibreCrypt.exe /drivercontrol uninstall /drivername {<span style="font-style: italic;">&lt;driver name&gt;</span> | all} [/silent]
> </pre>

<A NAME="level_4_heading_2">

#### Description
</A>

LibreCrypt has command line parameters so most operations can be run from the command line or MS Windows shortcuts.
#### Note on terminology
In the LibreCrypt GUI and documentation aimed at new users, easy to understand words like 'open', 'container' and 'lock' are used. In documentation aimed at more advanced users (like this page) standard file system terms are used, as follows:
<TABLE>
  <TBODY>    
		<TR> <TD>LibreCrypt Volume</TD>				<TD> A container                    </TD>  </TR>
		<TR> <TD>Volume</TD>							<TD> A 'Container', perhaps for a legacy app </TD>  </TR>
		<TR> <TD>Mount a volume</TD>			<TD> Open a Container     </TD>  </TR>
		<TR> <TD>Dismount a volume</TD>		<TD> Close a Container </TD>  </TR>
		<TR> <TD>Flush keys for a volume</TD><TD> Lock a Container   </TD>  </TR>    
  </TBODY>
</TABLE>

LibreCrypt was previously known as 'FreeOTFE'. The driver that handles native container volumes ('containers') is still known as the 'FreeOTFE' driver and the name is preserved in some filenames and command line parameters.
<A NAME="level_4_heading_3">

#### Options
</A>
            
 "-" may be used instead of "/", if required.

Note: Although there is currently no option to specify which drive letter a volume should be mounted as, this may be set on any given container by selecting "Tools | Change volume/keyfile password/details..."


<TABLE>
  <TBODY>
    <TR>
      <TD style="vertical-align: top;">
      	<pre>/count</pre>
      </TD>
      <TD style="vertical-align: top;">Count the number of open containers. Exit code indicates the appropriate number (or error code; see below)</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      	<pre>/create</pre>
      </TD>
      <TD style="vertical-align: top;">Create new container</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      	<pre>/dismount {<span style="font-style: italic;">&lt;drive letter&gt;</span>[:] | all}</pre>
      </TD>
      <TD style="vertical-align: top;">Dismount indicated drive/all drives</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      	<pre>/drive <span style="font-style: italic;">&lt;drive letter&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">Drive letter to be used when mounting the volume</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/drivercontrol count [/type {total | portable | installed}]</pre>
      </TD>
      <TD style="vertical-align: top;">Return count of drivers as exit code; total drivers present/count installed in portable mode/count formally installed. If "/type" is not specified, "/type total" is assumed</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/drivercontrol gui</pre>
      </TD>
      <TD style="vertical-align: top;">Display driver control dialog</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/drivercontrol install /filename {<span style="font-style: italic;">&lt;driver filename&gt;</span> | all}</pre>
      </TD>
      <TD style="vertical-align: top;">Install specified driver, or all drivers in the current working directory</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/drivercontrol uninstall /drivername {<span style="font-style: italic;">&lt;driver name&gt;</span> | all}<span style="font-style: italic;"></span></pre>
      </TD>
      <TD style="vertical-align: top;">Uninstall specified driver, or all installed drivers. Note that this takes the <span style="font-style: italic;">driver name</span>, not the driver's filename (Important: Don't attempt to uninstall drivers which are currently in use; e.g. by a mounted volume)</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/force</pre>
      </TD>
      <TD style="vertical-align: top;">When dismounting, force dismount (emergency dismount)</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/freeotfe</pre>
      </TD>
      <TD style="vertical-align: top;">The specified volume is a container. If a volume type (LibreCrypt/Linux) isn't specified, the user will be prompted for this information.</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/keyfile <span style="font-style: italic;">&lt;keyfile&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">Keyfile to be used when opening a container. Only valid when opening containers</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/keyfileisascii</pre>
      </TD>
      <TD style="vertical-align: top;">Only valid when mounting Linux LUKS volumes. If this flag is specified, the keyfile will be treated as containing ASCII password. Otherwise the keyfile will be treated as containing binary data. Default is to treat as binary.</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/keyfilenewline {CRLF|LF|CR}</pre>
      </TD>
      <TD style="vertical-align: top;">Only valid when mounting Linux LUKS volumes. If /keyfileisascii is specified, the first newline found in the keyfile will be treated as an EOF. This option specifies what constitutes a newline. Default is "LF".</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/keyiterations <span style="font-style: italic;">&lt;keyiterations&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">The number of key iterations</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/lesfile <span style="font-style: italic;">&lt;LES file&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">File from which to read Linux encryption settings from. Only valid when mounting non-LUKS Linux volumes</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/linux</pre>
      </TD>
      <TD style="vertical-align: top;">The specified volume is a Linux volume. If a volume type (LibreCrypt/Linux) isn't specified, the user will be prompted for this information.</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/mount</pre>
      </TD>
      <TD style="vertical-align: top;">Mount indicated file/partition</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/minimize</pre>
      </TD>
      <TD style="vertical-align: top;">Minimise main window when run</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/nocdbatoffset</pre>
      </TD>
      <TD style="vertical-align: top;">Indicates there is no CDB stored from the offset specified by "/offset". Only meaningful when both "/offset", "/freeotfe" and "/keyfile" are used.
      </TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/noexit</pre>
      </TD>
      <TD style="vertical-align: top;">Normally, when run with command line parameters, LibreCrypt carries out the requested action and exits. Specifying this parameter will cause LibreCrypt to continue running</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/offset <span style="font-style: italic;">&lt;offset&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">Use specified offset when mounting. This may be specified using GB/MB/KB units, or with no units for an offset in bytes (e.g. 5GB or 5368709120 for a 5GB offset). Note: If the value used for the offset has has a space in it, it should be surrounded by quotes (e.g. "10 GB" or 10GB are both valid)</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/password <span style="font-style: italic;">&lt;password&gt;</span></pre>
      </TD>
      <TD style="vertical-align: top;">The password to be used. It is _not_ recommended that this is used; particularly in shortcuts, as this would involve storing your password in plaintext such that anyone can read it!
      </TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/portable {start | on | 1 | stop | off | 0 | toggle}</pre>
      </TD>
      <TD style="vertical-align: top;">Turn on/off portable mode</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/readonly</pre>
      </TD>
      <TD style="vertical-align: top;">Mount the volume readonly</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/settings</pre>
      </TD>
      <TD style="vertical-align: top;">Use specified file for program settings, instead of normal settings location 
      </TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/silent<span style="font-style: italic;"></span></pre>
      </TD>
      <TD style="vertical-align: top;">Can be used when installing/uninstalling drivers and changing portable mode to suppress dialogs, and also when mounting a volume to prevent the password entry dialog showing
      </TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/saltlength _&lt;saltlength&gt;_</pre>
      </TD>
      <TD style="vertical-align: top;">The length of the salt (in bits)</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/volume _&lt;volume&gt;_</pre>
      </TD>
      <TD style="vertical-align: top;">The volume filename/partition to be used. When using a volume file, this should be set to the full path and filename of a volume file. Partitions should be specified in the form: "\Device\Harddisk<span style="font-style: italic;">X</span>\Partition<span style="font-style: italic;">Y</span>", where <span style="font-style: italic;">X</span> is 0 based, while <span style="font-style: italic;">Y</span> starts from 1 to indicate a particular partition, or 0 to indicate the entire disk.
      Important: If the volume filename has any spaces in it, then it should be enclosed in double quotes (").</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/SetTestMode _&lt;on|off&gt;_</pre>
      </TD>
      <TD style="vertical-align: top;">Set or unset Windows 'test mode' to allow unsigned drivers in 64 bit Windows 7 or above</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/SetInstalled</pre>
      </TD>
      <TD style="vertical-align: top;">Sets a flag in the ini file to indicate the app has been installed. Without this flag, LibreCrypt will attempt to install the drivers at startup</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">
      <pre>/dev_menu</pre>
      </TD>
      <TD style="vertical-align: top;">Enables an extra 'developer' menu, used for testing</TD>
    </TR>    
  </TBODY>
</TABLE>

Note: Under Windows Vista, the "install" and "uninstall" options require LibreCrypt.exe to be explicitly "runas" an account with administrator privileges, if not being executed from a process which has already been escalated; see examples below. The "/portable" will automatically escalate to administrator privileges, if required



<A NAME="level_4_heading_4">

#### Exit Codes
</A>

When run at the command line, LibreCrypt will terminate with one of the following exit codes:



<TABLE style="text-align: left;" border="1" cellpadding="2" cellspacing="2">

  <TBODY>
    <TR>
      <TH style="vertical-align: top;">Exit code</TH>
      <TH style="vertical-align: top;">Meaning   </TH>
    </TR>
    <TR>
      <TD style="vertical-align: top;">0</TD>
      <TD style="vertical-align: top;">Success</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">100</TD>
      <TD style="vertical-align: top;">Invalid/unrecognised command line</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">101</TD>
      <TD style="vertical-align: top;">Unable to connect to FreeOTFE driver; check driver is installed or portable mode is started</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">102</TD>
      <TD style="vertical-align: top;">Unable to mount volume</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">103</TD>
      <TD style="vertical-align: top;">Unable to dismount volume </TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">104</TD>
      <TD style="vertical-align: top;">Unable to start portable mode</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">105</TD>
      <TD style="vertical-align: top;">Unable to stop portable mode</TD>
    </TR><TR>
      <TD style="vertical-align: top;">106</TD>
      <TD style="vertical-align: top;">The requested operation requires administrative privileges</TD>
    </TR>
    <TR>
      <TD style="vertical-align: top;">999</TD>
      <TD style="vertical-align: top;">Other error</TD>
    </TR>
  </TBODY>
</TABLE>

<A NAME="level_4_heading_5">

#### Examples
</A>

Opening a container:

> <pre>LibreCrypt.exe /mount /volume "C:\My Private Data\my encrypted volume.vol" /freeotfe</pre>

Mounting a Linux volume readonly:

> <pre>LibreCrypt.exe /mount /volume "C:\My Private Data\my encrypted volume.vol" /linux /readonly</pre>

Mounting a container partition::

> <pre>LibreCrypt.exe /mount /volume "\Device\Harddisk0\Partition1" /freeotfe /readonly</pre>

Dismount drive "S:":

> <pre>LibreCrypt.exe /dismount S:</pre>

Dismount all volumes, emergency:

> <pre>LibreCrypt.exe /dismount all /force</pre>

Start and stop portable mode:

> <pre>LibreCrypt.exe /portable start
> LibreCrypt.exe /portable stop
> </pre>

Installing drivers (except Windows Vista):

> <pre>
> LibreCrypt.exe /drivercontrol install /filename FreeOTFE.sys
> LibreCrypt.exe /drivercontrol install /filename FreeOTFEHashSHA.sys
> LibreCrypt.exe /drivercontrol install /filename FreeOTFECypherAES_ltc.sys
> LibreCrypt.exe /drivercontrol install /filename all
> </pre>

Installing drivers (Windows Vista):

> <pre>
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol install /filename FreeOTFE.sys
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol install /filename FreeOTFEHashSHA.sys
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol install /filename FreeOTFECypherAES_ltc.sys
> </pre>

Uninstalling drivers (except Windows Vista):

> <pre>
> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFE
> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFEHashSHA
> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFECypherAES
> LibreCrypt.exe /drivercontrol uninstall /drivername all
</pre>

Uninstalling drivers (except Windows Vista):

> <pre>
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFE
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFEHashSHA
> runas /user:<span style="font-style: italic;">myAdminAccount</span> LibreCrypt.exe /drivercontrol uninstall /drivername FreeOTFECypherAES
</pre>






