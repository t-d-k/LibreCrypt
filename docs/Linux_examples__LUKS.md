

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. Using this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Linux Examples: LUKS</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="../src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](../src/Common/Common/images/DoxBox128.png)](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.squte.com/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Linux Examples: LUKS

This section gives a series of examples of how to create Linux LUKS volumes, and then mount them using DoxBox.

These examples have been
tested using Fedora Core 3, with a v2.6.20.1 kernel installed and using cryptsetup-luks v1.0; though
they should work for all compatible Linux distributions.

Note: The executable name in the following examples is "cryptsetup-luks"; most systems use "cryptsetup".

  * [Initial Setup](#level_3_heading_1)
  * [Defaults](#level_3_heading_2)
  * [Example #1: Mounting a LUKS Volume Encrypted Using LUKS's Default Encryption](#level_3_heading_3)
  * [Example #2: Mounting a LUKS Volume Encrypted Using 256 bit AES](#level_3_heading_4)
  * [Example #3: Mounting a LUKS Volume Encrypted Using 128 bit Twofish](#level_3_heading_5)
  * [Example #4: Mounting a LUKS Volume Encrypted Using 256 bit AES-XTS](#level_3_heading_6)
  * [Example #5: Mounting a LUKS Volume Encrypted Using 256 bit Serpent XTS](#level_3_heading_7)


* * * 
<A NAME="level_3_heading_1">
### Initial Setup
</A>

To begin using LUKS under Linux, ensure that the various kernel modules are installed:

<blockquote>
<pre>
modprobe cryptoloop

modprobe aes
modprobe anubis
modprobe arc4
modprobe blkcipher
modprobe blowfish
modprobe cast5
modprobe cast6
modprobe cbc
modprobe crc32c
modprobe crypto_algapi
modprobe crypto_hash
modprobe cryptomgr
modprobe crypto_null
modprobe deflate
modprobe des
modprobe ecb
modprobe gf128mul
modprobe hmac
modprobe khazad
modprobe lrw
modprobe md4
modprobe md5
modprobe michael_mic
modprobe serpent
modprobe sha1
modprobe sha256
modprobe sha512
modprobe tea
modprobe tgr192
modprobe twofish_common
modprobe twofish
modprobe wp512
modprobe xcbc

**# dm_mod should give you dm_snapshot, dm_zero and dm_mirror?**
modprobe dm_mod
modprobe dm_crypt
</pre>
</blockquote>

At this point, typing "dmsetup targets" should give you something along the lines of:

<pre>
crypt            v1.0.0
striped          v1.0.1
linear           v1.0.1
error            v1.0.1
</pre>

Typing "lsmod" will show you which modules are currently installed.

* * * 
<A NAME="level_3_heading_2">
### Defaults
</A>

If not overridden by the user, LUKS defaults to encrypting with:

<TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Cypher:</TH>
      <TD>AES</TD>
    </TR>
    <TR>
      <TH>Cypher keysize:</TH>
      <TD>128 bit</TD>
    </TR>
    <TR>
      <TH>Cypher mode:</TH>
      <TD>cbc-plain       </TD>
    </TR>
    <TR>
      <TH>Hash:</TH>
      <TD>SHA-1</TD>
    </TR>
  </TBODY>
</TABLE>

* * * 
<A NAME="level_3_heading_3">
### Example #1: Mounting a LUKS Volume Encrypted Using LUKS's Default Encryption
</A>

This example demonstrates use of a LUKS volume using the LUKS's
default encryption system: AES128 with the user's password hashed with SHA1, using 32 bit sector IDs as encryption IVs

Creating the volume file under Linux:

	dd if=/dev/zero of=./volumes/vol_default.box bs=1M count=1
	losetup /dev/loop0 ./volumes/vol_default.box
	echo password1234567890ABC | cryptsetup-luks luksFormat /dev/loop0
	cryptsetup-luks luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup-luks luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup-luks status myMapper
	losetup /dev/loop1 /dev/mapper/myMapper
	mkdosfs /dev/loop1
	mkdir ./test_mountpoint
	mount /dev/loop1 ./test_mountpoint
	cp ./test_files/SHORT_TEXT.txt        ./test_mountpoint
	cp ./test_files/BINARY_ZEROS.dat      ./test_mountpoint
	cp ./test_files/BINARY_ABC_RPTD.dat   ./test_mountpoint
	cp ./test_files/BINARY_00_FF_RPTD.dat ./test_mountpoint
	umount ./test_mountpoint
	losetup -d /dev/loop1
	cryptsetup-luks luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint

Mounting the volume under DoxBox:

  1. Select "Linux | Mount..."
  1. Select the volume file
  1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
  1. Click the "OK" button

* * * 
<A NAME="level_3_heading_4">
### Example #2: Mounting a LUKS Volume Encrypted Using 256 bit AES
</A>

This example demonstrates use of a LUKS AES256 volume.

Creating the volume file under Linux:

<blockquote>
<pre>
dd if=/dev/zero of=./volumes/vol_aes_256.box bs=1M count=1
losetup /dev/loop0 ./volumes/vol_aes_256.box
echo password1234567890ABC | cryptsetup-luks -c aes -s 256 luksFormat /dev/loop0
cryptsetup-luks luksDump /dev/loop0 
echo password1234567890ABC | cryptsetup-luks luksOpen /dev/loop0 myMapper
dmsetup ls
dmsetup table
dmsetup status
cryptsetup-luks status myMapper
losetup /dev/loop1 /dev/mapper/myMapper
mkdosfs /dev/loop1
mkdir ./test_mountpoint
mount /dev/loop1 ./test_mountpoint
cp ./test_files/SHORT_TEXT.txt        ./test_mountpoint
cp ./test_files/BINARY_ZEROS.dat      ./test_mountpoint
cp ./test_files/BINARY_ABC_RPTD.dat   ./test_mountpoint
cp ./test_files/BINARY_00_FF_RPTD.dat ./test_mountpoint
umount ./test_mountpoint
losetup -d /dev/loop1
cryptsetup-luks luksClose myMapper
losetup -d /dev/loop0
rm -rf ./test_mountpoint
</pre>
</blockquote>

Mounting the volume under DoxBox:


1. Select "Linux | Mount..."
1. Select the losetup volume file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button

* * * 
<A NAME="level_3_heading_5">
### Example #3: Mounting a LUKS Volume Encrypted Using 128 bit Twofish
</A>

This example demonstrates use of a LUKS Twofish 128 volume.

Creating the volume file under Linux:

	dd if=/dev/zero of=./volumes/vol_twofish.box bs=1M count=1
	losetup /dev/loop0 ./volumes/vol_twofish.box
	echo password1234567890ABC | cryptsetup-luks -c twofish luksFormat /dev/loop0
	cryptsetup-luks luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup-luks luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup-luks status myMapper
	losetup /dev/loop1 /dev/mapper/myMapper
	#cat ./test_files/2MB_Z.dat > /dev/loop1
	#cat ./test_files/2MB_0x00.dat > /dev/loop1
	mkdosfs /dev/loop1
	mkdir ./test_mountpoint
	mount /dev/loop1 ./test_mountpoint
	cp ./test_files/SHORT_TEXT.txt        ./test_mountpoint
	cp ./test_files/BINARY_ZEROS.dat      ./test_mountpoint
	cp ./test_files/BINARY_ABC_RPTD.dat   ./test_mountpoint
	cp ./test_files/BINARY_00_FF_RPTD.dat ./test_mountpoint
	umount ./test_mountpoint
	losetup -d /dev/loop1
	cryptsetup-luks luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint


Mounting the volume under DoxBox:

1. Select "Linux | Mount..."
1. Select the losetup volume file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button



* * * 
<A NAME="level_3_heading_6">
### Example #4: Mounting a LUKS Volume Encrypted Using 256 bit AES-XTS
</A>

This example demonstrates use of a LUKS AES 256 volume in XTS mode.

Creating the volume file under Linux:


	dd if=/dev/zero of=./volumes/vol_aes_xts.box bs=5M count=1
	losetup /dev/loop0 ./volumes/vol_aes_xts.box
	echo password1234567890ABC | cryptsetup-luks -c aes-xts-plain -s 512 luksFormat /dev/loop0
	cryptsetup-luks luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup-luks luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup-luks status myMapper
	losetup /dev/loop1 /dev/mapper/myMapper
	#cat ./test_files/2MB_Z.dat > /dev/loop1
	#cat ./test_files/2MB_0x00.dat > /dev/loop1
	mkdosfs /dev/loop1
	mkdir ./test_mountpoint
	mount /dev/loop1 ./test_mountpoint
	cp ./test_files/SHORT_TEXT.txt        ./test_mountpoint
	cp ./test_files/BINARY_ZEROS.dat      ./test_mountpoint
	cp ./test_files/BINARY_ABC_RPTD.dat   ./test_mountpoint
	cp ./test_files/BINARY_00_FF_RPTD.dat ./test_mountpoint
	umount ./test_mountpoint
	losetup -d /dev/loop1
	cryptsetup-luks luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint


Mounting the volume under DoxBox:

1. Select "Linux | Mount..."
1. Select the losetup volume file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button


* * * 
<A NAME="level_3_heading_7">
### Example #5: Mounting a LUKS Volume Encrypted Using 256 bit Serpent XTS
</A>

This example demonstrates use of a LUKS Serpent 256 volume in XTS mode.

Creating the volume file under Linux:


		dd if=/dev/zero of=./volumes/vol_serpent_xts.box bs=5M count=1
		losetup /dev/loop0 ./volumes/vol_serpent_xts.box
		echo password1234567890ABC | cryptsetup-luks -c serpent-xts-plain -s 512 luksFormat /dev/loop0
		cryptsetup-luks luksDump /dev/loop0 
		echo password1234567890ABC | cryptsetup-luks luksOpen /dev/loop0 myMapper
		dmsetup ls
		dmsetup table
		dmsetup status
		cryptsetup-luks status myMapper
		losetup /dev/loop1 /dev/mapper/myMapper
		#cat ./test_files/2MB_Z.dat > /dev/loop1
		#cat ./test_files/2MB_0x00.dat > /dev/loop1
		mkdosfs /dev/loop1
		mkdir ./test_mountpoint
		mount /dev/loop1 ./test_mountpoint
		cp ./test_files/SHORT_TEXT.txt        ./test_mountpoint
		cp ./test_files/BINARY_ZEROS.dat      ./test_mountpoint
		cp ./test_files/BINARY_ABC_RPTD.dat   ./test_mountpoint
		cp ./test_files/BINARY_00_FF_RPTD.dat ./test_mountpoint
		umount ./test_mountpoint
		losetup -d /dev/loop1
		cryptsetup-luks luksClose myMapper
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint


Mounting the volume under DoxBox:



1. Select "Linux | Mount..."
1. Select the losetup volume file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button





