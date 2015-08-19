

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Linux Examples: LUKS</TITLE>

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
            

## Linux Examples: LUKS

This section gives a series of examples of how to create Linux LUKS containers, and then open them using LibreCrypt.

These examples have been tested using Fedora Core 3, with a v2.6.20.1 kernel installed and using cryptsetup v1.0; though they should work for all compatible Linux distributions.


  * [Initial Setup](#level_3_heading_1)
  * [Defaults](#level_3_heading_2)
  * [Example #1: Opening a LUKS Container Encrypted Using LUKS's Default Encryption](#level_3_heading_3)
  * [Example #2: Opening a LUKS Container Encrypted Using 256 bit AES](#level_3_heading_4)
  * [Example #3: Opening a LUKS Container Encrypted Using 128 bit Twofish](#level_3_heading_5)
  * [Example #4: Opening a LUKS Container Encrypted Using 256 bit AES-XTS](#level_3_heading_6)
  * [Example #5: Opening a LUKS Container Encrypted Using 256 bit Serpent XTS](#level_3_heading_7)


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
### Example #1: Opening a LUKS Container Encrypted Using LUKS's Default Encryption
</A>

This example demonstrates use of a LUKS container using the LUKS's
default encryption system: AES128 with the user's password hashed with SHA1, using 32 bit sector IDs as encryption IVs

Creating the container file under Linux:

	dd if=/dev/zero of=./containers/vol_default.vol bs=1M count=1
	losetup /dev/loop0 ./containers/vol_default.vol
	echo password1234567890ABC | cryptsetup luksFormat /dev/loop0
	cryptsetup luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup status myMapper
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
	cryptsetup luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

  1. Select "Linux | Open..."
  1. Select the container file
  1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
  1. Click the "OK" button

* * * 
<A NAME="level_3_heading_4">
### Example #2: Opening a LUKS Container Encrypted Using 256 bit AES
</A>

This example demonstrates use of a LUKS AES256 container.

Creating the container file under Linux:

<blockquote>
<pre>
dd if=/dev/zero of=./containers/vol_aes_256.vol bs=1M count=1
losetup /dev/loop0 ./containers/vol_aes_256.vol
echo password1234567890ABC | cryptsetup -c aes -s 256 luksFormat /dev/loop0
cryptsetup luksDump /dev/loop0 
echo password1234567890ABC | cryptsetup luksOpen /dev/loop0 myMapper
dmsetup ls
dmsetup table
dmsetup status
cryptsetup status myMapper
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
cryptsetup luksClose myMapper
losetup -d /dev/loop0
rm -rf ./test_mountpoint
</pre>
</blockquote>

Opening the container under LibreCrypt:


1. Select "Linux | Open..."
1. Select the losetup container file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button

* * * 
<A NAME="level_3_heading_5">
### Example #3: Opening a LUKS Container Encrypted Using 128 bit Twofish
</A>

This example demonstrates use of a LUKS Twofish 128 container.

Creating the container file under Linux:

	dd if=/dev/zero of=./containers/vol_twofish.vol bs=1M count=1
	losetup /dev/loop0 ./containers/vol_twofish.vol
	echo password1234567890ABC | cryptsetup -c twofish luksFormat /dev/loop0
	cryptsetup luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup status myMapper
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
	cryptsetup luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint


Opening the container under LibreCrypt:

1. Select "Linux | Open..."
1. Select the losetup container file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button



* * * 
<A NAME="level_3_heading_6">
### Example #4: Opening a LUKS Container Encrypted Using 256 bit AES-XTS
</A>

This example demonstrates use of a LUKS AES 256 container in XTS mode.

Creating the container file under Linux:


	dd if=/dev/zero of=./containers/vol_aes_xts.vol bs=5M count=1
	losetup /dev/loop0 ./containers/vol_aes_xts.vol
	echo password1234567890ABC | cryptsetup -c aes-xts-plain64 -s 512 luksFormat /dev/loop0
	cryptsetup luksDump /dev/loop0 
	echo password1234567890ABC | cryptsetup luksOpen /dev/loop0 myMapper
	dmsetup ls
	dmsetup table
	dmsetup status
	cryptsetup status myMapper
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
	cryptsetup luksClose myMapper
	losetup -d /dev/loop0
	rm -rf ./test_mountpoint


Opening the container under LibreCrypt:

1. Select "Linux | Open..."
1. Select the losetup container file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button


* * * 
<A NAME="level_3_heading_7">
### Example #5: Opening a LUKS Container Encrypted Using 256 bit Serpent XTS
</A>

This example demonstrates use of a LUKS Serpent 256 container in XTS mode.

Creating the container file under Linux:


		dd if=/dev/zero of=./containers/vol_serpent_xts.vol bs=5M count=1
		losetup /dev/loop0 ./containers/vol_serpent_xts.vol
		echo password1234567890ABC | cryptsetup -c serpent-xts-plain64 -s 512 luksFormat /dev/loop0
		cryptsetup luksDump /dev/loop0 
		echo password1234567890ABC | cryptsetup luksOpen /dev/loop0 myMapper
		dmsetup ls
		dmsetup table
		dmsetup status
		cryptsetup status myMapper
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
		cryptsetup luksClose myMapper
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint


Opening the container under LibreCrypt:

1. Select "Linux | Open..."
1. Select the losetup container file
1. In the dialog shown, enter "password1234567890ABC" as the key, and set any of the options wanted.
1. Click the "OK" button





