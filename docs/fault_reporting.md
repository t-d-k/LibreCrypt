<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Fault/Bug Reporting</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">

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
            
## Fault/Bug Reporting

Although LibreCrypt should be pretty stable and have no faults, it's always possible that you may find *something* not quite right. In these cases, it would be *very much appreciated* if you could report it as shown in the [contact details](contact_details.html).

If you have recently upgraded to a newer version of LibreCrypt, please could you ensure that you have followed the upgrade procedure *exactly* before reporting a fault. (See [Installation and Upgrading from a Previous Version](installation_and_upgrading__PC.html) section)

When reporting a fault with LibreCrypt, please include as much detail as possible, preferably including as much of the following as possible (Note: Not all of the following may apply):


  1. What OS you're running under.
  1. Any service packs which have been applied to your OS.
  1. The LibreCrypt executable version (See the "Help | About..." dialog).
  1. The main FreeOTFE driver version (See the "Help | About..." dialog).
  1. Details of how the volume in question was created (e.g. the summary shown on the last stage of creating a container).
  1. The size of any volume file/partition involved.
  1. If using a volume file, the filesystem used on the drive the volume file is stored on (e.g. NTFS/FAT/FAT32).
  1. The filesystem the volume is formatted as (e.g. NTFS/FAT/FAT32).
  1. A copy of the CDB dump taken from any volume file involved (See "Tools | Critical Data Block | Dump to human readable file...").
  1. If a keyfile is being used, a copy of the keyfile and password used.
  1. A **small** test volume (e.g. 1MB) which can replicate the problem found (Note: _Please do *not* email volume files, unless asked to!_ An FTP site is available for uploading these)

Some of the items listed above may include potentially sensitive data. In which case, feel free to omit that information - or better still, create a simple test case which replicates the problem, but doesn't include any such data.

