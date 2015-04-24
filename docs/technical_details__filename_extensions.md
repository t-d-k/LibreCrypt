

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Technical Details: Filename Extensions</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***
    
            

### Technical Details: Filename Extensions

LibreCrypt container files, keyfiles, and all other files created and used by LibreCrypt can have _*any*_ file extension you wish to give them (if any).

By default, LibreCrypt uses the following:

<TABLE>
<TR> <TH>Extension</TH> <TH>Description</TH> </TR>
<TR> <TD>.vol</TD> <TD>Volume file (container)</TD> </TR> 
<TR> <TD>.cdb</TD> <TD>Keyfile (aka Critical Data Block)</TD> </TR>
<TR> <TD>.cdbBackup</TD> <TD>Critical Data Block Backup</TD> </TR>
</TABLE>



