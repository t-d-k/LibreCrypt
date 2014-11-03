

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: DoxBox Volumes and Keyfiles</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

### Technical Details: DoxBox Volumes and Keyfiles

A DoxBox (regardless of whether its stored in a file or partition) consists of two things:

<OL>
  * A critical data block (CDB)
  * An encrypted partition image
</OL>

The CDB may either form part of the volume, in which case it is
prepended to the encrypted partition image, or it may be stored as a
separate file, in which case it is referred to as a "keyfile".

Users may create any number of keyfiles for any given volume. To create a new keyfile, the user must supply either:

<OL>
* An existing keyfile, and its password, etc
* A volume file which has a CDB
</OL>

together with its password, salt length, etc. The keyfile or volume
CDB supplied will then be read in, decrypted, and re-encrypted with a new
password, salt length, etc (all supplied by the user) before being written out as the new keyfile.

A full definition of the contents of a CDB/keyfile is supplied in this documentation.

<A NAME="level_4_heading_1">
#### Notes
</A>

<UL>

  *  A DoxBox keyfile is nothing more than a CDB, the "volume details block" of which contains the encryption details used for securing the volume it relates to
  *  A volume may have one or more keyfiles, in which case they all share the same data stored within their respective "volume details block", but each one is encrypted with a different user password, salt, random padding, etc - making each keyfile unique. 
  *  Keyfiles are encrypted with the same cypher/hash that the encrypted partition image they relate to is encrypted with.

</UL>



