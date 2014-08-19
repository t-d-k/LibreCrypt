

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: Random Number Generators (RNGs)</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:sdean12@sdean12.org">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://DoxBox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_Open-Source disk encryption for Windows_
</SPAN>

      
            

### Technical Details: Random Number Generators (RNGs)

FreeOTFE offers a choice of four different random number generators (RNGs) for use when creating new FreeOTFE volumes:

<OL>

  * Microsoft CryptoAPI
  * Mouse movement
  * cryptlib
  * PKCS#11 tokens

</OL>

Whichever one is selected must produce 4096 bits (512 bytes) of cryptographically secure random numbers. This random data is used in three ways:

<OL>

  * As the master key used for encrypting/decrypting your data
  * For salting
  * As random "padding" to make up otherwise unused space within the DoxBox's critical data block. (See volume layout documentation for further details)

</OL>

If more than one RNG is selected, their output will be combined (XOR'd together) and the resulting data used. In this way, the random data generated will never be weaker than the strongest selected RNG.

<A NAME="level_4_heading_1">
#### Microsoft CryptoAPI
</A>

The Microsoft CryptoAPI is used to generate random data.

<A NAME="level_4_heading_2">
#### Mouse Movement
</A>

This relies on the user "waggling" the mouse in a random fashion to generate random data.

Every 100ms the mouse pointer is checked. If it has moved significantly, then the X and Y coordinates of the mouse pointer are sampled, and the LSB of each is added to the random data collected.

Due to the volume of random data required, and the fact that only 2 bits of random data are collected for each mouse position sampled, this is a relatively slow process.

<A NAME="level_4_heading_3">
#### cryptlib
</A>
cryptlib is used to generate random data.

Note: This option is only available if cryptlib (cl32.dll) is installed; see the [cryptlib](http://www.cs.auckland.ac.nz/%7Epgut001/cryptlib/) web site for further details and download.

<A NAME="level_4_heading_4">
#### PKCS#11 Tokens
</A>

If you have a security token or smartcard, this may be used as a RNG.

See the [Security Token/Smartcard Support]() section for more information on setting up and using PKCS#11 tokens.



