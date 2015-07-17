

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Miscellaneous Notes</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Miscellaneous Notes

* Please, do _read the documentation_ (the [FAQ section](FAQ.md), in particular) _before_ emailing questions! The [FAQ section](FAQ.md) in particular may well have the answer you're looking for.
* Both versions of LibreCrypt, and LibreCrypt Explorer, are fully compatible with one another.
* From the main window, doubleclicking on a mounted drive will launch explorer on that drive. Rightclicking brings up a context menu.
* After creating a new container it is recommended that you make a backup of the volume's CDB.

	* In the case of volume files which have their CDB stored as part of the volume file, this can be achieved by selecting "Tools | Critical
data block | Backup..."
	* In the case of volume files where the CDB is stored in a separate keyfile, simply make a backup copy of this keyfile.
	
* A number of LibreCrypt properties can be changed via the "Tools | Change volume/keyfile password/details..." menu-item. Note that volumes must be _dismounted_ first before they can be modified in this way.
* An option is included to dump out a human readable version of the volume's critical data block/keyfile's contents (select "Tools | Critical data block | Dump to human readable file..."). This option is primarily intended to assist developers, and to future-proof volumes file by giving you access to the actual master encryption/decryption key used by the volume it dumps out. It should be noted that the inclusion of this option does **not** present a security risk as it **requires** that the user to enter the volume/keyfile's password immediately before it can operate (obviously, the volume/keyfile's password is needed in order to decrypt the critical data block). If an attacker has your volume/keyfile's password, clearly this option will give no further information away.
* A "Revert timestamps" option is available from the "Options" dialog. If selected, on mounting a volume file its timestamps will be noted. When the volume is subsequently dismounted, these timestamps will be restored. By default, the PC version of LibreCrypt, and LibreCrypt Explorer, have this option switched **on**.
* A password is **not** needed when backing up a volume's CDB as the backup copy is not stored in plaintext; it is a literal backup copy of a volume's (encrypted) CDB.
* A password **is** needed when creating a keyfile as this requires that the volume's CDB is decrypted, before being re-encrypted with the keyfile's password and written out to the keyfile.


