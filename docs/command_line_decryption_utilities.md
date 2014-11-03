

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Appendix F: Command Line Decryption Utilities</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***
      
            
## Appendix F: Command Line Decryption Utilities

* * * 
<A NAME="level_3_heading_1">
### Overview
</A>
*_Note: The development of the command line decryption utilities has ceased. This functionality has been superceded with the development of DoxBox Explorer_*

DoxBox is relatively unique in that comes complete with software which may be used to decrypt encrypted volumes (provided the correct decryption key is known!).

This software is designed to fulfil two main objectives:

  1. To increase and encourage peer review of DoxBox
  1. To act as a "security blanket" for users - should development of DoxBox ever be dropped, it will still be possible for users to recover their data, regardless of the state of the DoxBox project.

Functionally, this software has one task: to decrypt the encrypted partition area of DoxBox files and to write out the plaintext version for examination.

This software is considerably easier to understand than the kernel mode drivers, and does **not** require the Microsoft SDK/DDK to be present. As a result, any competent software engineer should be able to modify the software as appropriate and confirm that data is being encrypted correctly by the DoxBox system.

This software is **not** intended for general public use, but by those who understand and can write C. In order to use it, modifications to the source code will most probably be required (to change the decryption keys used, if nothing else). For this purpose, the command line decryption utilities are not released in binary form, only as source code which must be compiled by the user.

* * * 
<A NAME="level_3_heading_2">
### Operation
</A>
Each of the command line decryption utilities is designed to operate in the following manner:

  1. Open the (input) encrypted volume file.
			* The filename used is **hard coded** to "inFile.dat"; obviously this may be changed as required.
	
  1. Open/Create the (output) plaintext volume file.
			* The filename used is **hard coded** to "outFile.dat"; obviously this may be changed as required.
	
  1. Generate an IV, if required
			* The method of generating the IV may vary, dependent on how the volume was encrypted

			1. Read in a sector's worth of data from the input (encrypted) file
  1. Decrypt the sector, block by block
			* The key used here is **hard coded** in the source, and must be the actual key that was used to encrypt the data (obviously!)
			* The way in which decryption is carried out is cypher, and cypher implementation dependent

  1. Write the decrypted sector to the output (plaintext) file
  1. Repeat steps 3-6 until all data has been decrypted
  1. Close the output file
  1. Close the input file

Please note:

 1. This software is focused only on decrypting data. They do **not** hash user keys, etc
 1. The hard coded keys must represent the actual encryption keys. In the case of Linux volumes, this is the user's password hashed as appropriate. In the case of DoxBox volumes, this is the "master key" stored in the volume's "critical data block"

At time of writing, although a separate command line decryption utility to decode a DoxBox's CDB/keyfiles has not been implemented, the DoxBox GUI does incorporate this functionality allowing developers to extract all of the information required contained within a CDB/keyfile. **(Note: For obvious reasons, this requires the volume's password and all other details that are required to use the CDB are known - it is simply not possible to decrypt this information otherwise)**



