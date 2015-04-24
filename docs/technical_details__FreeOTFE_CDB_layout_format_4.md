

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Technical Details: LibreCrypt Critical Data Block (CDB) Layout (CDB Format ID 4)</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

### Technical Details: LibreCrypt Critical Data Block (CDB) Layout (CDB Format ID 4)

CDB layout format 4 is identical to CDB layout format 3, with the one
exception that the Master key stored within the Volume details block
may include multiple keys, one after the other

For example:

<UL>
  * An LRW encrypted volume's "Master key" will be the encryption key, followed immediately by the tweak key.

  * An XTS encrypted volume's "Master key" will be the two XTS encryption keys, one after the other

</UL>
In all cases, the Master key length will indicate the total length of
all keys. It is the responsibility of the cypher driver to determine
how the key material is used.

FreeOTFE v3.0 (and later) create volumes using this layout.



