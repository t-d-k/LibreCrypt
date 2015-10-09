

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Linux Examples: Cryptoloop</TITLE>

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
      
            

## Linux Examples: Cryptoloop

This section gives a series of examples of how to create Linux Cryptoloop (losetup) containers, and then open them using LibreCrypt.

These examples have been tested using SuSE 9.2; though they should work for all compatible Linux distributions.

  * [Initial Setup](#level_3_heading_1)
  * [Defaults](#level_3_heading_2)
  * [Example #1: Opening a Cryptoloop Container Without Encryption](#level_3_heading_3)
  * [Example #2: Opening a Cryptoloop Container Encrypted Using XOR](#level_3_heading_4)
  * [Example #3: Opening a Cryptoloop Container Encrypted Using 128 bit AES](#level_3_heading_5)
  * [Example #4: Opening a Cryptoloop Container Encrypted Using 256 bit AES](#level_3_heading_6)
  * [Example #5: Opening a Cryptoloop Container Encrypted Using 256 bit AES and rmd160 Hash](#level_3_heading_7)
  * [Example #6: Opening a Cryptoloop Container Encrypted Using 256 bit AES and Seed Value](#level_3_heading_8)
  * [Example #7: Opening a Cryptoloop Container Encrypted Using 256 bit AES and Offset](#level_3_heading_9)
  * [Example #8: Opening a Cryptoloop Container Encrypted Using 256 bit Twofish](#level_3_heading_10)

* * * 
<A NAME="level_3_heading_1">
### Initial Setup
</A>

To begin using Cryptoloop under Linux, ensure that the various kernel modules are installed:

<blockquote>
<pre>
modprobe cryptoloop

modprobe deflate
modprobe zlib_deflate
modprobe twofish
modprobe serpent
modprobe aes_i586
modprobe blowfish
modprobe des
modprobe sha256
modprobe sha512
modprobe crypto_null
modprobe md4
modprobe md5
modprobe arc4
modprobe khazad
modprobe anubis
</pre>
</blockquote>

Typing "lsmod" will show you which modules are currently installed.

The examples shown below may then be followed to create and use various container files.

* * * 
<A NAME="level_3_heading_2">
### Defaults
</A>

If not overridden by the user, Cryptoloop defaults to no encryption. If the user specifies that they **do** want encryption (i.e. passes "losetup" a "-e" parameter), Cryptoloop defaults to the following:

<TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Cypher:</TH>
      <TD>As specified by the user (no encryption takes place if no cypher is specified)</TD>
    </TR>
    <TR>
      <TH>Cypher keysize:</TH>
      <TD>128 bit</TD>
    </TR>
    <TR>
      <TH>User key processed with:</TH>
      <TD>The hash used to process the user's key is dependent on the cypher's keysize:  <TABLE style="text-align: left;">
  <TBODY>
    <TR>
      <TH>Cypher keysize</TH>
      <TH>Hash</TH>
    </TR>
    <TR>
      <TD>128 - 191 bits</TD>
      <TD>SHA-256</TD>
    </TR>
    <TR>
      <TD>192 - 255 bits</TD>
      <TD>SHA-384</TD>
    </TR>
    <TR>
      <TD>256+ bits</TD>
      <TD>SHA-512</TD>
    </TR>
  </TBODY>
</TABLE>

"Hash with "A"s, if hash output is too short" option - this option should  **not** be selected; if the hash used outputs too few bits, its output is right-padded with 0x00 characters to the required length.
      </TD>
    </TR>
    <TR>
      <TH>IV generation:</TH>
      <TD>32 bit sector ID</TD>
    </TR>
  </TBODY>
</TABLE>

* * * 
<A NAME="level_3_heading_3">
### Example #1: Opening a Cryptoloop Container Without Encryption
</A>

This is the simplest form of Linux container file, and the recommended starting point for checking that LibreCrypt is operating correctly.

Creating the container file under Linux:

		
		dd if=/dev/zero of=./vol_none bs=1k count=1024
		losetup /dev/loop0 ./vol_none
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Select "Linux | Open..."
1. Select the container file
1. "Key" tab:

	* Leave key blank
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Leave seed blank
	* Select the "Null" hash
	* Leave iteration count at 0

1. "Encryption" tab:

	* Select the "Null" cypher
	* Select the "Null IV" IV generation method
	* The "Hash with "A"s, if hash output is too short" makes no difference

1. "File options" tab:

	* Leave offset at 0
	* Leave sizelimit at 0

1. "Open options" tab:

	* Select any unused drive letter
	* Leave readonly unchecked

1. Click the "OK" button

* * * 
<A NAME="level_3_heading_4">
### Example #2: Opening a Cryptoloop Container Encrypted Using XOR
</A>

This is the second simplest form of Linux container file, and is the simplest case to confirm that passwords are being accepted and used correctly.

Creating the container file under Linux:

		dd if=/dev/zero of=./vol_xor bs=1k count=1024
		losetup -e XOR /dev/loop0 ./vol_xor
		# Enter password: password1234567890ABC
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Click "File | Open dm-crypt ..."
1. Select the container file
1. "Key" tab:
	+ Enter "password1234567890ABC" as the key
	+ Leave GPG executable blank
	+ Leave GPG keyfile  blank
	+ Leave seed blank
	+ Select the "Null" hash
	+ Leave iteration count at 0
	
1. "Encryption" tab:
	* Select the "XOR" cypher
	* Select the "Null IV" IV generation method
	* The "Hash with "A"s, if hash output is too short" makes no difference.

1. "File options" tab:

	* Leave offset at 0
	* Leave sizelimit at 0

1. "Open options" tab:

	* Select any unused drive letter
	* Leave readonly unchecked

1. Click the "OK" button

* * * 
<A NAME="level_3_heading_5">
### Example #3: Opening a Cryptoloop Container Encrypted Using 128 bit AES
</A>

This example demonstrates use of a Linux AES128 container.

Creating the container file under Linux:

		dd if=/dev/zero of=./vol_aes128 bs=1k count=1024
		losetup -e AES128 /dev/loop0 ./vol_aes128
		# Enter password: password1234567890ABC
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Click "File | Open dm-crypt ..."
1. Select the container file
1. "Key" tab:
		* Enter "password1234567890ABC" as the key
		* Leave GPG executable blank
		* Leave GPG keyfile  blank
		* Leave seed blank
		* Select the "SHA-256 (256/512)" hash
		* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked. 
		* Leave iteration count at 0
1. "Encryption" tab:
		* Select the "AES (CBC; 128/128)" cypher
		* Select the "32 bits sector IV" IV generation method
		* Set "Sector zero location" to "Start of host file"
1. "File options" tab:
		* Leave offset at 0
		* Leave sizelimit at 0
1. "Open options" tab:
		* Select any unused drive letter
		* Leave readonly unchecked
1. Click the "OK" button

* * * 
<A NAME="level_3_heading_6">
### Example #4: Opening a Cryptoloop Container Encrypted Using 256 bit AES
</A>

This example demonstrates use of a dm-crypt AES256 container.

Creating the container file under Linux:

		
		dd if=/dev/zero of=./vol_aes256 bs=1k count=1024
		losetup -e AES256 /dev/loop0 ./vol_aes256
		# Enter password: password1234567890ABC
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

<OL>

1. Click "File | Open dm-crypt ..."
* Select the container file
* "Key" tab:
	<UL>
	* Enter "password1234567890ABC" as the key
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Leave seed blank
	* Select the "SHA-512 (512/1024)" hash
	* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked.
	
	* Leave iteration count at 0
	</UL>
* "Encryption" tab:
<UL>
	* Select the "AES (CBC; 256/128)" cypher
	
	* Select the "32 bits sector IV" IV generation method
	
	* Set "Sector zero location" to "Start of host file"
	
	</UL>
* "File options" tab:
	<UL>
	* Leave offset at 0
	* Leave sizelimit at 0
	</UL>
* "Open options" tab:
	<UL>
	* Select any unused drive letter
	* Leave readonly unchecked
	</UL>
* Click the "OK" button

</OL>

* * * 

<A NAME="level_3_heading_7">
### Example #5: Opening a Cryptoloop Container Encrypted Using 256 bit AES and rmd160 Hash
</A>

This example demonstrates use of a Linux AES256 container using the rmd160
hash to process the user's password instead of the default SHA hash.

*WARNING:* Note that this example uses the "rmd160" and not "ripemd160" hash.

Creating the container file under Linux:

			
			dd if=/dev/zero of=./vol_aes256_rmd160 bs=1k count=1024
			losetup -e AES256 -H rmd160 /dev/loop0 ./vol_aes256_rmd160
			# Enter password: password1234567890ABC
			mkdosfs /dev/loop0
			mkdir ./test_mountpoint
			mount /dev/loop0 ./test_mountpoint
			echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
			cp TEST_FILE_1.dat ./test_mountpoint
			cp TEST_FILE_2.dat ./test_mountpoint
			cp TEST_FILE_3.dat ./test_mountpoint
			umount /dev/loop0
			losetup -d /dev/loop0
			rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

<OL>

1. Click "File | Open dm-crypt ..."
* Select the container file
* "Key" tab:
	<UL>
	* Enter "password1234567890ABC" as the key
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Leave seed blank
	* Select the "RIPEMD-160 (Linux; Twice, with A)" hash
	* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked.
	
	* Leave iteration count at 0
	</UL>
* "Encryption" tab:
	<UL>
	* Select the "AES (CBC; 256/128)" cypher
	
	* Select the "32 bits sector IV" IV generation method
	
	* Set "Sector zero location" to "Start of host file"
	
	</UL>
* "File options" tab:
		<UL>
		* Leave offset at 0
		* Leave sizelimit at 0
		</UL>
* "Open options" tab:
	<UL>
	* Select any unused drive letter
	* Leave readonly unchecked
	</UL>
* Click the "OK" button

</OL>

* * * 
<A NAME="level_3_heading_8">
### Example #6: Opening a Cryptoloop Container Encrypted Using 256 bit AES and Seed Value
</A>

This example demonstrates use of a Linux AES256 container with seeding. The seed used here is the string "seedvalue"

Creating the container file under Linux:

		dd if=/dev/zero of=./vol_aes256_seeded bs=1k count=1024
		losetup -e AES256 -S seedvalue /dev/loop0 ./vol_aes256_seeded
		# Enter password: password1234567890ABC
		losetup -a
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		cp TEST_FILE_1.dat ./test_mountpoint
		cp TEST_FILE_2.dat ./test_mountpoint
		cp TEST_FILE_3.dat ./test_mountpoint
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Click "File | Open dm-crypt ..."
1. Select the container file
1. "Key" tab:
	<UL>
	* Enter "password1234567890ABC" as the key
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Change the seed to "seedvalue"
	* Select the "SHA-512 (512/1024)" hash
	* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked.
	
	* Leave iteration count at 0
	</UL>
1. "Encryption" tab:
<UL>
	* Select the "AES (CBC; 256/128)" cypher
	* Select the "32 bits sector IV" IV generation method
	
	* Set "Sector zero location" to "Start of host file"

</UL>
1. "File options" tab:
	<UL>
	* Leave offset at 0
	* Leave sizelimit at 0
	</UL>
1.* "Open options" tab:
	<UL>
	* Select any unused drive letter
	* Leave readonly unchecked
	</UL>
1. Click the "OK" button

* * * 
<A NAME="level_3_heading_9">
### Example #7: Opening a Cryptoloop Container Encrypted Using 256 bit AES and Offset
</A>

This example demonstrates use of a Linux AES256 container, with the encrypted container beginning at an offset of 2560 bytes into the container file.

Creating the container file under Linux:

		dd if=/dev/zero of=./vol_aes256_2560 bs=1k count=1024
		losetup -e AES256 -o 2560 /dev/loop0 ./vol_aes256_2560
		# Enter password: password1234567890ABC
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Click "File | Open dm-crypt ..."
1. Select the container file
1. "Key" tab:

	* Enter "password1234567890ABC" as the key
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Leave seed blank
	* Select the "SHA-512 (512/1024)" hash
	* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked.	
	* Leave iteration count at 0

1. "Encryption" tab:

	* Select the "AES (CBC; 256/128)" cypher
	* Select the "32 bits sector IV" IV generation method
	
	* Set "Sector zero location" to "Start of host file"

1. "File options" tab:

	* Change offset to 2560 bytes* Leave sizelimit at 0

1. "Open options" tab:

		* Select any unused drive letter
		* Leave readonly unchecked

1. Click the "OK" button

* * * 
<A NAME="level_3_heading_10">
### Example #8: Opening a Cryptoloop Container Encrypted Using 256 bit Twofish
</A>

This example demonstrates use of a Linux Twofish 256 bit container.

Creating the container file under Linux:

		dd if=/dev/zero of=./vol_twofish256 bs=1k count=1024
		losetup -e twofish256 /dev/loop0 ./vol_twofish256
		# Enter password: password1234567890ABC
		losetup -a
		mkdosfs /dev/loop0
		mkdir ./test_mountpoint
		mount /dev/loop0 ./test_mountpoint
		echo "This is a text test file" > ./test_mountpoint/SHORT_TEXT.txt
		umount /dev/loop0
		losetup -d /dev/loop0
		rm -rf ./test_mountpoint

Opening the container under LibreCrypt:

1. Click "File | Open dm-crypt ..."
1. Select the container file
1. "Key" tab:
	
	* Enter "password1234567890ABC" as the key
	* Leave GPG executable blank
	* Leave GPG keyfile  blank
	* Leave seed blank
	* Select the "SHA-512 (512/1024)" hash
	
	* Make sure that the "Hash with "A"s, if hash output is too short" is **not** checked.
	
	* Leave iteration count at 0
	
1. "Encryption" tab:
	
	* Select the "Twofish (CBC; 256/128)" cypher
	* Select the "32 bits sector IV" IV generation method
	
	* Set "Sector zero location" to "Start of host file"
	
	
1. "File options" tab:
	
	* Leave offset at 0
	
	* Leave sizelimit at 0
	
1. "Open options" tab:
	
	* Select any unused drive letter
	* Leave readonly unchecked
	
2. Click the "OK" button


