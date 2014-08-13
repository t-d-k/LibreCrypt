

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: Filename Extensions</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:sdean12@sdean12.org">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://DoxBox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_OpenSource disk encryption for Windows_
</SPAN>
</TABLE>
\n
    </TD>
  </TR>
  <TR>
    <TD width=20%>

      
            

### Technical Details: Filename Extensions

Volume files, keyfiles, and all other files created and used by FreeOTFE can have
_*any*_ file extension you wish to give them (if any).

By default, FreeOTFE uses the following:

<TABLE>
<TR>
<TH>Extension</TH>
<TH>Description</TH>
</TR>
<TR>
<TD>.vol</TD>
<TD>Volume file</TD>
</TR>
<TR>
<TD>.cdb</TD>
<TD>Keyfile (aka Critical Data Block)</TD>
</TR>
<TR>
<TD>.cdbBackup</TD>
<TD>Critical Data Block Backup</TD>
</TR>
</TABLE>



