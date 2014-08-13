

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Appendix D: Glossary</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:sdean12@sdean12.org">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://DoxBox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_OpenSource disk encryption for Windows_
</SPAN>

      
            

## Appendix D: Glossary

*CDB*

				Acronym: Critical Data Block

				See "Critical Data Block"

*Critical Data Block*

				AKA "CDB"

				A block of data holding data vital to the correct mounting and
use of an encrypted partition (volume). Among other things, a volume's
CDB contains the master key used for encrypting/decrypting data as it
is written/read from a the volume. CDBs are **encrypted**. A full description of what FreeOTFE
stores in its CDBs can be found in the [Technical Details](technical_details.htm#technical_details) section.

*LES file*

				Linux Encryption Settings file; a text file in which encryption settings for Linux volumes are held. Created by using the "Load..."/"Save..." buttons on the password entry dialog when mounting Linux volumes.

*OTFE*

				Acronym: On The Fly Encryption

				Any of a number of encryption systems where data is stored on
disk in encrypted format. When it is read in from disk, it is
transparently decrypted "on the fly" before user applications receive
it. In a similar manner, when applications, etc write data back to the
disk, it is automatically encrypted before being written.



