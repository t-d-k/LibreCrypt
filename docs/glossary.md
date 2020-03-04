<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Appendix D: Glossary</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.tdksoft.co.uk/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.tdksoft.co.uk/): Open-Source disk encryption for Windows_
</SPAN>
***
<SPAN class="tip">
The latest version of this document can be found at the [LibreCrypt project site](https://github.com/t-d-k/librecrypt)
</SPAN>      
            

## Appendix D: Glossary

*CDB*

Acronym: Critical Data Block

See "Critical Data Block"

*Critical Data Block*

AKA "CDB"

The header of FreeOTFE format containers. A block of data holding data vital to the correct opening and use of an encrypted partition (container). Among other things, a container's CDB contains the master key used for encrypting/decrypting data as it is written/read from a the container. CDBs are *encrypted*. A full description of what LibreCrypt stores in its CDBs can be found in the [Technical Details](technical_details.htm#technical_details) section.

*LES file*

Linux Encryption Settings file; a text file in which encryption settings for Linux containers are held. Created by using the "Load..."/"Save..." buttons on the password entry dialog when opening Linux containers.

*OTFE*

Acronym: On The Fly Encryption

Any of a number of encryption systems where data is stored on disk in encrypted format. When it is read in from disk, it is transparently decrypted "on the fly" before user applications receive it. In a similar manner, when applications, etc write data back to the disk, it is automatically encrypted before being written.



