

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: Encrypted Partition Image Encryption/Decryption</TITLE>

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

      
            

### Technical Details: Encrypted Partition Image Encryption/Decryption

The encrypted partition that forms the bulk of an encrypted volume is encrypted on a 512 byte, sector-by-sector basis using:

<UL>

* The master key stored within the volume's CDB (or keyfile)
* **_A per-sector IV_ (provided that the cypher used has a fixed blocksize, greater than zero)**

</UL>

<A NAME="level_4_heading_1">
#### Per-Sector IV Generation
</A>

The manner in which per-sector IVs are generated depends on the IV
generation method the user selected when creating the volume:

<TABLE style="text-align: left;">

  <TBODY>
    <TR>
      <TH>IV Generation Method </TH>
      <TH>Description </TH>
    </TR>
    <TR>
      <TD>Null IV</TD>
      <TD>No IV is used/a null IV is used.  i.e. A block of data consisting of with 0x00 (null) characters is used as the IV</TD>
    </TR>
    <TR>
      <TD> 32 bit sector ID</TD>
      <TD>The least significant 32 bits of the sector ID is right-padded with 0x00 characters, and used as the IV.  These bits are ordered MSB..LSB.</TD>

    </TR>
    <TR>
      <TD>  64 bit sector ID</TD>
      <TD>As the 32-bit sector ID method, but a 64 bits sector ID is used.  Note: This is unlikely to offer any security advantage over using 32 bit sector IDs, unless used with a volume file ((2^32) * 512) bytes long (2048GB), or greater</TD>
    </TR>
    <TR>
      <TD>Hashed 32 bit sector ID</TD>
      <TD>The least significant 32 bits of the sector ID is hashed with the user's choice of hash algorithm.  The resulting hash value will be truncated/right padded with 0x00 characters until it is the same length as the cypher's blocksize.</TD>
    </TR>
    <TR>
      <TD>Hashed  64 bit sector ID</TD>
      <TD>As the hashed 32-bit sector ID method, but a 64 bits sector ID is used.  Note: This is unlikely to offer any security advantage over using 32 bit sector IDs, unless used with a volume file ((2^32) * 512) bytes long (2048GB), or greater</TD>
    </TR>
    <TR>
      <TD>  ESSIV</TD>
      <TD>This option offers the most security. On mounting the DoxBox, the master key used for encrypting/decrypting the volume is hashed with the hash algorithm chosen by the user when the volume was created. If the cypher used for encryption/decryption has a fixed keysize, this hash output is truncated/right padded with 0x00 characters until it matches the cypher's keysize and stored as the "ESSIV key" (or "salt"). If the cypher doesn't have a fixed keysize, the full hash output is stored as this key ("salt"). When a per-sector IV is required, the 64 bit sector ID is encrypted using the "ESSIV key". This encrypted sector ID is truncated/right-padded with 0x00 characters until it matches the cypher's blocksize.</TD>
    </TR>
  </TBODY>
</TABLE>

In all cases, the sector ID is calculated as:

<blockquote>
Sector ID = (**Ostart** - **Soffset**) % **Ssize**

</blockquote>

where:

<TABLE>

  <TBODY>
    <TR>
      <TD>**Ostart**</TD>

      <TD>The offset within the host volume/partition from where the encrypted partition begins (i.e. after any CDB)</TD>

    </TR>
    <TR>
      <TD>**Soffset**</TD>

      <TD>The offset from within the encrypted partition from where the sector begins</TD>

    </TR>
    <TR>
      <TD>**Ssize**</TD>

      <TD>The sector size of the emulated drive (i.e. 512 bytes)</TD>

    </TR>
    <TR>
      <TD>%</TD>

      <TD>is the modulus operator</TD>

    </TR>
  </TBODY>
</TABLE>

Putting it another way, this is the sector ID (starting from zero) of the partition as it appears to the host OS after mounting.

If the user opted to additionally use per-volume IVs when the volume was created, IVs generated using the method selected by the user when the volume was created are XORd with a "per-volume" IV. This "per-volume" IV consists of a block of data equal to the blocklength of the cypher used to encrypt the volume and consists of random data generated when the volume was created, and stored within the volume's CDB (or keyfile).



