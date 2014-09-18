

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Linux Volumes</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="../src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](../src/Common/Common/images/DoxBox128.png)](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.squte.com/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Linux Volumes

 
_IMPORTANT_: This is obvious, _but_... If you are using FTP to transfer your Linux volumes between your Linux and MS Windows systems, _make sure you transfer the volume file in binary mode!_

<UL>
  * [Creating Linux Volumes](#level_3_heading_1)
  * [Hiding a Linux Volume Within Another Volume](#level_3_heading_2)
  * [Mounting Volumes Created under Linux](#level_3_heading_3)
  * [Cryptoloop Specific Information](#level_3_heading_4)
  * [dm-crypt Specific Information](#level_3_heading_5)
  * [LUKS Specific Information](#level_3_heading_6)
  * [Additional Notes on Linux Volumes](#level_3_heading_7)
</UL>

* * * 
<A NAME="level_3_heading_1">
### Creating Linux Volumes
</A>

_*IMPORTANT: *Ifyou select the wrong options when creating a Linux volume usingDoxBox, you will not be able to read it under Linux! (Although this is patently obvious, there are some people who...!)*NOTE:* At time of writing (17th July 2005), although DoxBox can read and write LUKS volumes, it cannot create them itself._

To create a new encrypted Linux-compatible volume:

  1. Launch DoxBox
  1. If you are creating a file-based volume (as opposed to an encrypted partition):
  
    2. Select "File | Linux volume | New..."
    2. Enter the filename and size of the volume required.
    2. Click the "OK" button
    	This will create a file of the appropriate size, and is the equivalent of the Linux command:

				dd if=/dev/zero of=./vol_none bs=1M count=**x**

		Where "_x_" is the size of the volume in MB.	
		
		
		This step is unnecessary in case of using an existing partition on your HDD, and is only required in order to create the file which is to store the encrypted data.

  1.  Select "File | Linux volume | Mount..."
  1.  Enter the appropriate parameters into the mount dialog.
  		See the "Linux Examples" section in the documentation for example configurations; the choices you make here must reflect those options that are supported by the version of Linux you wish to subsequently use the volume with.
  1. Click the "OK" button This will mount the volume using the encryption system(s) specified, and is analogous to executing the Linux commands:	
			
				losetup /dev/loop0 &lt;volume file&gt; &lt;various options&gt;
				mkdir ./mountpoint
				mount /dev/loop0 ./mountpoint

		At this point a new drive should appear, although at this stage it is unformatted and you will be given an error if you attempt to access it (e.g. using Windows Explorer) 
  1. Select the new drive 
  1. Select "Tools | Format..."
  	You will be shown the standard MS Windows format dialog. It is suggested that you select either the FAT or FAT32 filesystem.
  1. Click "OK" button to format the volume.
		This is analogous to the Linux command:
		
		
			mkdosfs /dev/loop0
		
		Your volume file is now fully ready for use, although for security reasons it is highly recommended that you now initialize the volume by writing data to all its sectors before making any further use of it.	
		
		This process is recommended because if omitted it may be possible for an attacker to determine the amount of data you have stored on your volume file (as stated above, the actual process of creating the volume file consists of creating a suitably large file, filled with zeros) 
		Note: It is important that this step be carried out if you intend using the volume file just created as a "host" file for storing a second, hidden, encrypted volume. 
			
		
	1. Switch back to DoxBox, and select "Tools | Overwrite free space..."
	2. Click "Yes" when prompted if you wish to proceed.
			DoxBox will then write pseudorandom data to the drive, which will then be encrypted before being written to the volume file.

* * * 
<A NAME="level_3_heading_2">
### Hiding a Linux Volume Within Another Volume
</A>

To create a Linux volume within another volume file:

  1. Create a DoxBox/Linux volume as normal, **ensuring that you initialize the volume by mounting it, formatting it, and then overwriting all its free space**.
  1. Unmount the "outer" volume
  1. Remount the "outer" volume, but specify a reasonable offset on the "File options" tab of the Linux mount dialog.

* * * 
<A NAME="level_3_heading_3">
### Mounting Volumes Created under Linux
</A>

Select "File | Linux volume | Mount file.../Mount partition".

Enter the volume's password, and set all appropriate options

Click "OK".

Note that if you do not:

  1. Set the same options as used when the volume is mounted and used while under Linux
  1. Format the volume using a filesystem MS Windows understands (i.e. NTFS/FAT/FAT32)

then although your Linux volume may well be mounted, its contents will probably be unreadable.

* * * 
<A NAME="level_3_heading_4">
### Cryptoloop Specific Information
</A>

<A NAME="level_4_heading_1">
#### Hash Selection
</A>

Cryptoloop ("losetup") Linux volumes use the hash of the user's key as the key used for reading/writing to the encrypted volume.

<A NAME="level_4_heading_2">
#### Cryptoloop and RIPEMD-160
</A>

If you pass "-H rmd160" to losetup in order to use RIPEMD-160 to
process your password, losetup's behaviour changes slightly. The user's
password is **not** simply hashed with RIPEMD-160 - instead, the following procedure is used:

  1. The user's password is hashed once using RIPEMD-160
  1. A copy of the first 129 characters of the user's password is made
  1. The capital letter "A" is prepended to the start of the copy
  1. The resulting string is then hashed with RIPEMD-160
  1. This hash is then appended to the first hash to produce 320 bits of data
  1. The appropriate number of bits is taken from the result, and used as the encryption/decryption key.

For this reason, DoxBox includes a RIPEMD-160 driver specifically modified ("RIPEMD-160 (Linux; Twice, with A)") to carry out this form of hashing.

(This does not appear to be documented; the above logic was derived from examining "util-linux-2.12p.diff" - one of the files included with loop-AES)

<A NAME="level_4_heading_3">
#### Cypher Selection
</A>

If the cypher selected ("-e" parameter passed as losetup) can support different keysizes (e.g. AES supports 128/192/256 bit keysizes), and the user doesn't specify the keysize to be used (i.e. you specify "-e AES" as opposed to "-e AES128"), then the cypher will default to using 128 bit keys.

(From: [http://loop-aes.sourceforge.net/loop-AES.README](http://loop-aes.sourceforge.net/loop-AES.README))

* * * 
<A NAME="level_3_heading_5">
### dm-crypt Specific Information
</A>

<h4>/dev/loop1 Usage in the Examples Included in this Documentation</H4>
The examples shown in this documentation include the slightly odd step of:

	losetup /dev/loop1 /dev/mapper/myMapper
	mkdosfs /dev/loop1
	...

as opposed to just straight:

	>mkdosfs /dev/mapper/myMapper
	...

This is carried out as (in my tests) the latter appears to result in failure:

	# mkdosfs /dev/mapper/myMapper
	mkdosfs 2.8 (28 Feb 2001)
	mkdosfs: unable to get drive geometry for '/dev/mapper/myMapper'

<A NAME="level_4_heading_4">
#### Hash Selection
</A>

If an attempt is made to mount a volume using a cypher with a larger keysize than the hash algorithm used to processes the user's password, dm-crypt appears to use the following algorithm to generate the actual encryption/decryption key used by the cypher:

  1. The user's password is hashed.
  1. If the hash output contains fewer bits than the cypher's keysize, the capital letter "A" is prepended to the user's password, and a new hash is generated.
  1. This second hash is appended to the previous one
  1. If the total hash output contains fewer bits than the cypher's keysize, **another** capital letter "A" is prepended to the user's password, and a new hash is generated.
  1. This third hash is appended to the previous hashes
  1. If the total hash output contains fewer bits than the cypher's keysize, **another** capital letter "A" is prepended to the user's password, and a new hash is generated.
  1. ...and so on, until the required keylength is reached.

i.e. This is the same as Cryptoloop uses for its RIPEMD-160 hashing, but is extended to produce a key of arbitrary length, by adding multiple "A" characters to the password and hashing, until a key of the required length is obtained.

DoxBox supports this form of key processing, which can be invoked by selecting the option "Hash with "A"s, if hash output is too short" on the Linux mount dialog.

Note that, under linux, the actual encryption/decryption key can be shown in its hex representation by running "dmsetup table".

For example, if the volume's password is "password1234567890ABC", then:

If AES (256 bit key) is used for encryption/decryption, and the user's password is processed with RIPEMD-160, the actual encryption/decryption key will be:

	FAFE56C3BAB4CD216BA02474AC157EA555FA5711
	D539285C28A6D8122D9464EE

This is made up as follows:

<TABLE style="text-align: left;">

<TBODY>
<TR> <TD> <pre>FAFE56C3BAB4CD216BA02474AC157EA555FA5711</pre> </TD> <TD>The first 160 bits are the RIPEMD-160 hash of "password1234567890ABC"</TD> </TR>
<TR> <TD> <pre>D539285C28A6D8122D9464EE**0AA3C4811DE0D846**</pre> </TD> <TD>The remaining bits are the first 96 bits taken from the RIPEMD-160 hash of "Apassword1234567890ABC"</TD> </TR>
</TBODY>
</TABLE>

If Blowfish (448 bit key) is used for encryption/decryption, and the user's password is processed with MD5, the actual encryption/decryption key will be:

	4EAB90A0D00CE0086EB59DA838CC888D
	D1270498F52EFFA562872664BB514F8E
	2FA054980C9D92542F5801FDF82ADFEA
	121E587A4EEBDF3B

This is made up as follows:

<TABLE style="text-align: left;">

<TBODY>
<TR>
<TD> <pre>4EAB90A0D00CE0086EB59DA838CC888D</pre> </TD> <TD>The first 128 bits are the MD5 hash of "password1234567890ABC"</TD>
</TR>
<TR>
<TD> <pre>D1270498F52EFFA562872664BB514F8E</pre> </TD> <TD>The next 128 bits are the MD5 hash of "Apassword1234567890ABC"</TD>
</TR>
<TR>
<TD> <pre>2FA054980C9D92542F5801FDF82ADFEA</pre> </TD> <TD>The next 128 bits are the MD5 hash of "AApassword1234567890ABC"</TD>
</TR>
<TR>
<TD> <pre>121E587A4EEBDF3B**D6CD437A1B2C32A**</pre> </TD> <TD>The remaining bits are the first 64 bits taken from the MD5 hash of "AAApassword1234567890ABC"</TD>
</TR>
</TBODY>
</TABLE>

<A NAME="level_4_heading_5">
#### ESSIV
</A>

dm-crypt's ESSIV functionality is available with v2.6.10 and later Linux kernels.

The manner in which Linux uses ESSIV differs from DoxBox volumes in how the ESSIV encryption key is generated. Both hash the master encryption/decryption key to generate the key used for ESSIV, however dm-crypt uses the full hash output as the ESSIV key. This means that if you have a dm-crypt volume which is encrypted using 256 bit AES, and specify MD5 as the ESSIV hash, the ESSIV process will actually use AES-128 for creating the "salt" for ESSIV IVs (MD5 generates 128 bit hashes).

It is for this reason, you cannot create a dm-crypt volume under Linux using 256 bit Twofish, and specify SHA-512 as the ESSIV hash; Twofish doesn't support 512 bit keys, and so dm-crypt fails.

* * * 
<A NAME="level_3_heading_6">
### LUKS Specific Information
</A>
As LUKS is based on dm-crypt, please see also the section above relating to dm-setup.

DoxBox supports LUKS to v1.1 of the LUKS specification. This is the latest version at time of writing (2nd December 2007)

 
<SPAN class="tip">
As well as using the "File | Linux | Mount file/partition..." menu items, LUKS volumes may also be mounted using the main "File | Mount file/partition..." menu items and toolbar buttons. (DoxBox detects LUKS volumes by their signature and offers to mount them appropriately)   
</SPAN>

<A NAME="level_4_heading_6">
#### ESSIV
</A>

DoxBox supports LUKS with ESSIV, subject to the condition that the ESSIV hash used generates hashes with the same, or less, bits than the cypher's block size.

Also at time of writing (25th February 2007), the current LUKS implementation of "cryptsetup" only supports the SHA1 hash algorithm, although other hashes may be used for ESSIV.

Because of the way in which dm-crypt operates (see also the "dm-crypt" section on ESSIV, above), LUKS ESSIV doesn't do what you'd probably expect it to do. Specifically, if you have (for example) a Blowfish-448 encrypted volume, and specify cbc-essiv:sha256 for use as IVs - LUKS (dm-crypt) will actually use Blowfish-256 as the ESSIV cypher, and **not **Blowfish-448. In other words, the ESSIV cypher used will be from the same "family" of cypher (AES, Blowfish, Serpent, etc) - but will use the keylength which matches the ESSIV hash output length.

As a result of this, another option appears on the LUKS password entry dialog; "Base IV cypher on hash length". If this is checked, then when mounting an ESSIV volume, the keylength of the cypher used for ESSIV generation will be that of the ESSIV hash. If this is unchecked, the ESSIV cypher used will have the same keylength as the main bulk encryption cypher used for securing the encrypted disk image.

Most users will want this option **checked**.

<A NAME="level_4_heading_7">
#### DoxBox Supported LUKS Cyphers
</A>
The following table lists compatibility with LUKS cyphers:

<TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Cypher </TH> <TH>Compatibility </TH>
    </TR>
    <TR>
      <TD>aes</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>twofish</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>serpent</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>cast5</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>cast6</TD> <TD>Supported by DoxBox.</TD>
    </TR>
  </TBODY>
</TABLE>

<A NAME="level_4_heading_8">
#### DoxBox Supported LUKS Cypher modes
</A>
The following table lists compatibility with LUKS cypher modes:

<TABLE style="text-align: left;">
  <TBODY>
    <TR> <TH>Mode </TH> <TH>Compatibility </TH> </TR>
    <TR> <TD>ecb</TD> <TD>Not supported by DoxBox.  Note: This is a pretty insecure mode - the use of ECB is **highly discouraged**, and DoxBox is unlikely to ever support this mode.</TD> </TR>
    <TR> <TD>cbc-plain</TD> <TD>Supported by DoxBox.</TD> </TR>
    <TR> <TD>cbc-essiv:**&lt;hash&gt;**</TD> <TD>Supported by DoxBox</TD> </TR>
		<TR> <TD>lrw-benbi </TD> <TD>Supported by DoxBox</TD> </TR>
		<TR> <TD>xts-plain       </TD> <TD>Supported by DoxBox</TD> </TR> 
		<TR> <TD>xts-plain64 </TD> <TD>Supported by DoxBox</TD> </TR>
  </TBODY>
</TABLE>

<A NAME="level_4_heading_9">
#### DoxBox Supported LUKS hashes
</A>
The following table lists compatibility with LUKS hashes:

<TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Hash </TH> <TH>Compatibility </TH>
    </TR>
    <TR>
      <TD>sha1</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>sha256</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>sha512</TD> <TD>Supported by DoxBox.</TD>
    </TR>
    <TR>
      <TD>ripemd160</TD> <TD>Supported by DoxBox.</TD>
    </TR>
  </TBODY>
</TABLE>

* * * 
<A NAME="level_3_heading_7">
### Additional Notes on Linux Volumes
</A>

Linux volumes should be formatted as FAT/FAT32/NTFS in order for them to be recognised by MS Windows. Although it should be possible for MS Windows can to understand other filesystems (e.g. ext2/ext3/riserFS), this does require 3rd party filesystem drivers to be installed.

If you do wish to read an ext2/ext3 formatted volume from MS Windows, the filesystem drivers listed below are suggested. There may well be others, though at time of writing (23rd December 2005) these are the only ones that I have checked:

<TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Package </TH> <TH>URL </TH> <TH>Description</TH>
    </TR>
    <TR>
      <TD>Ext2 Installable File System For Windows</TD> <TD>http://www.fs-driver.org/</TD> <TD>Supports both read and write operations with ext2/ext3. Freeware, but closed source.</TD>

    </TR>
    <TR>
      <TD>EXT2 IFS for Windows</TD> <TD>http://uranus.it.swin.edu.au/~jn/linux/ext2ifs.htm</TD> <TD>Supports ext2, readonly. Open source.</TD>
 </TR>
  </TBODY>
</TABLE>

Further information on Linux volumes may be obtained from:

<TABLE style="text-align: left;">

  <TBODY>
    <TR>
      <TD>Cryptoloop</TD> <TD>[Cryptoloop HOWTO](http://tldp.org/HOWTO/Cryptoloop-HOWTO/)</TD>
    </TR>
    <TR>
 <TD>loop-AES</TD> <TD>[loop-AES README](http://loop-aes.sourceforge.net/loop-AES.README)</TD>
    </TR>
    <TR>
      <TD>dm-crypt</TD> <TD>[dm-crypt web site](http://www.saout.de/misc/dm-crypt/)  [dm-crypt Wiki](http://www.saout.de/tikiwiki/tiki-index.php)       </TD>
    </TR><TR>
      <TD>LUKS</TD> <TD>[LUKS - Linux Unified Key Setup](http://luks.endorphin.org/)</TD>
    </TR>

  </TBODY>
</TABLE>

Note that for many of the controls on DoxBox's Linux mount volume dialog, the equivalent Cryptoloop ("losetup") parameter for that control is displayed in brackets.





