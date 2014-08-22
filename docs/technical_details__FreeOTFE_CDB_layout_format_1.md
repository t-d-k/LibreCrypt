

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. Using this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: DoxBox Critical Data Block (CDB) Layout (CDB Format ID 1)</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="../src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](../src/Common/Common/images/DoxBox128.png)](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.squte.com/): Open-Source disk encryption for Windows_
</SPAN>   
            

### Technical Details: DoxBox Critical Data Block (CDB) Layout (CDB Format ID 1)

<font color="RED">*NOTE: This CDB layout is *_obsolete_*; all new volumes should use the [latest CDB format](technical_details__FreeOTFE_CDB_layout_format_3.md).*</font>

<A NAME="level_4_heading_1">
#### Overview
</A>

The following table describes the high-level layout of all DoxBox (**not **Linux) volume files:

<TABLE style="width: 100%;">

<TBODY><TR>
<TD style="background-color: rgb(255, 204, 204);">Critical data block:
<TABLE>
<TBODY><TR>
<TD>Password salt</TD>

<TD style="background-color: rgb(153, 255, 255);">Encrypted block:
<TABLE>
<TBODY><TR>
<TD>Check hash</TD>

<TD>Volume details block: <TABLE>
<TBODY>
<TR>
<TD>Critical data version</TD>
<TD>Volume flags</TD>
<TD>Encrypted partition image length</TD>
<TD>Master key length</TD>
<TD>Master key</TD>
<TD>Requested drive letter</TD>
<TD>Random padding #2</TD>
</TR>
</TBODY>
</TABLE>
</TD>

</TR>
</TBODY></TABLE>
</TD><TD>Random padding #1</TD>

</TR>
</TBODY></TABLE>
</TD>

<TD style="width: 100%; background-color: rgb(153, 255, 153);">Encrypted partition image</TD>
</TR>
</TBODY>
</TABLE>

Color key:

</p>

<TABLE style="text-align: left;">

<TBODY>
<TR>
<TH>Color
</TH>
<TH>Encryption used
</TH>
</TR>
<TR>
<TD style="vertical-align: top; background-color: rgb(255, 204, 204);">	</TD>

<TD>Red items are not encrypted</TD>

</TR>
<TR>
<TD style="vertical-align: top; background-color: rgb(153, 255, 255);">	</TD>

<TD>Blue items are encrypted with the user's password, password salt, and user's chosen hash/cypher algorithms</TD>
</TR>
<TR>
<TD style="vertical-align: top; background-color: rgb(153, 255, 153);">	</TD>

<TD>Green items are encrypted with the master key and the user's chosen cypher, with IVs generated with the user's chosen sector IV method</TD>

</TR>
</TBODY>
</TABLE>

Seem intimidating? Read on, and all will become clear... When broken down into its component parts,
the volume structure is reasonably straightforward to understand.
</p>

</p>

* * *

#### Breakdown: Top level

<TABLE width="100%">

<TBODY><TR>
<TD>Critical data block</TD>

<TD width="100%">Encrypted partition image</TD>
</TR>
</TBODY>
</TABLE>

At the highest level, volume files can be broken down into just two
basic parts: A critical data area, and the encrypted partition itself.

	
<TABLE>
<TBODY><TR>
<TH>Item name</TH>

<TH>Size (in bits)
</TH>

<TH>Description</TH>
</TR>

<TR>
<TD>Critical data block</TD>

<TD>4096</TD>

<TD>This part of the volume file contains "critical data", including the master en/decryption key used for the encrypted partition image, the method used for generating sector IDs, and a check hash.  See below for further breakdown.</p></TD>
</TR>

<TR>
<TD>Encrypted partition image</TD>

<TD>User defined.  (max 2^64 bytes; circa 16777216 TB)</TD>

<TD>This is a literal partition image which represents the volume. This image is encrypted with:  

<OL>
* The master key

* The user's chosen cypher

* The user's chosen method for generating different sector IVs

</OL>
</TD>
</TR>

<TR>
<TD>*_Total size:_*</TD>

<TD>4096 + Encrypted partition image size</TD>

<TD></TD>

</TR>
</TBODY></TABLE>

</p>

* * *

#### Breakdown: Critical data block layout

<TABLE width="50%">

<TBODY><TR>
<TD>Password salt</TD>

<TD width="100%">Encrypted block</TD><TD>Random padding #1</TD>

</TR>
</TBODY>
</TABLE>

<TABLE>

<TBODY><TR>
<TH>Item name</TH>

<TH>Size (in bits)</TH>

<TH>Description</TH>
</TR>

<TR>
<TD>Password salt</TD>

<TD>**sl**  (User specified to a max 512)</TD>

<TD>This data is appended onto the user's password, before it's hashed. The resulting hash may then be used to encrypt/decrypt the "Encrypted block".</TD>
</TR>

<TR>
<TD>Encrypted block</TD>

<TD>If **bs**&gt;0 then:  ((4096 - salt length) div **bs****)** * **bs**_****_  If **bs**&lt;=0 then:  (4096 - salt length)</TD>

<TD>This block contains the actual key which is used to encrypt/decrypt the encrypted partition image.  See below for further breakdown.  </p></TD>
</TR>

<TR>
<TD>Random padding #1</TD>

<TD>((4096 - **sl**) % **bs**)</TD>

<TD>Random "padding" data. Required to pad out any remaining, unused, bits in the "critical data block"****</TD>

</TR>
<TR>
<TD>*_Total size:_*</TD>

<TD>4096</TD>

<TD></TD>

</TR>
</TBODY>
</TABLE>

</p>

* * *

#### Breakdown: Encrypted block layout

<TABLE width="50%">

<TBODY><TR>
<TD>Check hash</TD>

<TD width="100%">Volume details block</TD>

</TR>
</TBODY>
</TABLE>

As described above, this entire block is encrypted using the user's
password, salt, and chosen hash and cypher algorithms. 

</p>

As this block is encrypted, its length (in bits) must be a multiple of the cypher's blocksize.

<TABLE>
<TBODY><TR>
<TH>Item name</TH>

<TH>Size (in bits)</TH>

<TH>Description</TH>
</TR>

<TR>
<TD>Check hash</TD>

<TD>**hk**</TD>

<TD>This is the hash of the plaintext version of the "Volume details block".  If **hk** is zero, then this hash will be either truncated to 512 bits, or right-padded with 0 bits up to a maximum of 512 bits</TD>

</TR>

<TR>
<TD>Volume details</TD>

<TD>_Total size_ - _hk_</TD>

<TD>This stores the details of how to encrypt/decrypt the encrypted partition.</TD>
</TR>

<TR>
<TD>*_Total size:_*</TD>

<TD>If **bs**&gt;8 then:  ((4096 - **sl**) / **bs****)** * **bs**_****_  If **bs**&lt;=8 then:  (4096 - **sl**) </TD>

<TD>Note: "/" represents **integer** division</TD>

</TR>
</TBODY></TABLE>

</p>

* * *

#### Breakdown: Volume details block layout

<TABLE>

<TBODY>
<TR>
<TD>Critical data version</TD>
<TD>Volume flags</TD>
<TD>Encrypted partition image length</TD>
<TD>Master key length</TD>
<TD>Master key</TD>
<TD>Requested drive letter</TD>
<TD>Random padding #2</TD>

</TR>
</TBODY>
</TABLE>

Finally, we reach the details that the critical data block was designed
to protect:

	
<TABLE>
<TBODY><TR>
<TH>Item name</TH>

<TH>Size (in bits)</TH>

<TH>Description</TH>
</TR>

<TR>
<TD>Critical data version</TD>

<TD>8</TD>

<TD>This is a version ID which identifies the layout of the remainder of the volume details block  When this layout format is used, this will always be set to 1.</p></TD>
</TR>

<TR>
<TD>Volume flags</TD>

<TD>32</TD>

<TD>Bitmap flagging various items.

Currently, this only identifies which method is used in generating sector IVs, used
in encrypting/decrypting the encrypted partition image.

Bit - Description

0 - Use different IVs for each sector  						 0 = Use NULL IV  						 1 = Use sector ID (possibly hashed) as IV  1 - Sector ID zero is at the start of the file  						 0 = Sector ID zero is at the start of the encrypted data  						 1 = Sector ID zero is at the start of the host volume file  2 - &lt;unused&gt;  3 - Hash sector ID before use  						 0 = Sector ID used as IV  						 1 = Hash sector ID before using is as an IV</TD>

</TR>

<TR>
<TD>Encrypted partition image length</TD>
<TD>64</TD>
<TD>This stores the length (in bytes) of the encrypted partition image</TD>
</TR>
<TR>
<TD>Master key length</TD>

<TD>32</TD>

<TD>This will be set to the length of the master key **in bits**. Not strictly needed, but used as a sanity check.</TD>
</TR>

<TR>
<TD>Master key</TD>

<TD>_ks  (max 1536)  _</TD>

<TD>This is set to the random data generated when the volume was created; it represents the en/decryption key used to encrypt the encrypted partition image</TD>
</TR>

<TR>
<TD>Requested drive letter</TD>

<TD>8</TD>
<TD>The drive letter the volume should be typically be mounted as. Set to 0x00 if there is no particular drive letter the volume should be mounted as; mount using the first available drive letter.</TD>
</TR>
<TR>
<TD>Random padding #2</TD>

<TD>All remaining bits to pad out to _Total size_ </TD>

<TD>Random "padding" data. Required to pad out the encrypted block to a multiple of **bs**, and to increase the size of this block to the maximum length that can fit within the "critical data block".</TD>

</TR>

<TR>
<TD>*_Total size:_*</TD>

<TD>(((4096 - **sl**) / **bs****)** * **bs**_****_**)** - _hk_</TD>

<TD>Note: "/" represents **integer** division </TD>
</TR>
</TBODY></TABLE>

</p>

<hr style="width: 100%; height: 2px;">
<A NAME="level_4_heading_2">
#### Miscellaneous Comments Regarding the DoxBox File Layout
</A>

The design of the critical data layout eliminates the need for the
cypher/hash used to be stored anywhere, denying an attacker this
information and increasing the amount of work required to attack a
volume file.

The "critical data block" is encrypted.

The "password salt" appears **before**
the "encrypted block", and no indication of the length of salt used is
stored anywhere in order to prevent an attacker from even knowing where
the "encrypted block" starts within the "critical data block".

The "Check hash" appears **before**
the volume details block. This may seem a little strange since the size
of "Check hash" is variable (its actual length is dependent on the
hash algorithm chosen), but appears first in order to reduce the
amount of "known" (or predictable) data early on in the volume.
Theoretically this is desirable as (for example) cyphers operating in
CBC (or similar) modes which "whiten" data will do so with data that is
less predictable than would occur if the hash appeared towards the end
of the block it appears in.

The "Check hash" is limited to 512 bits. This is limited as, in
practical terms, some kind of limit is required if the critical data
block is to be of a predetermined size. See section on mounting volume
files for how multiple matching check hashes is handled.

The "Password salt" is (fairly arbitrarily) limited to 512 bits. Again, this is primarily done for practical reasons.

Although at time of writing (2004) this limit to the length of salt
used should be sufficient, the format of the critical data block (with
included layout version ID) does allow future scope for modification in
order to allow the critical data block to be extended (e.g. from 4096
to 8192 bits), should this limit be determined as insufficient.

The "Encrypted block" does contain a certain amount of data
that may be reasonably guessed by an attacker (e.g. the critical data
version), however this would be of very limited use to an attacker
launching a "known plaintext" attack as the amount of this data is
minimal, and as with pretty much any OTFE system, the "Encrypted partition image" can reasonably expected to
contain significantly more known plaintext anyway (e.g. the partition's boot sector)

<hr style="width: 100%; height: 2px;">
<A NAME="level_4_heading_3">
#### Creating DoxBox Volumes
</A>

To create a DoxBox file, a fairly significant amount of
information is required due to freedom that DoxBox gives you in
creating volume files. 

Broadly speaking, creating a DoxBox consists of three distinct stages:

<OL>
  * Creating a file large enough on the local filesystem
  * Writing the critical data block to the volume file
  * Mounting the volume, formatting it, and "shredding" (overwriting) all free space
</OL>

Stage 1 is straightforward; write data to the file until is has gained
the required size. This stage is skipped in the case of creating a
hidden volume.

Stage 2 is more complex; and will be described below.

Stage 3 is required in set the volume up for use, and increase security.

<A NAME="level_4_heading_4">
#### Writing the critical data block.
</A>

The following procedure is used to build up a DoxBox's critical volume block:

<OL>

* Obtain all the information which will be stored within the volume's **"Volume details block"**
* The following information is determined:

  <OL>

  </OL>
  <OL>
    * "**sl**" - The number of "salt" bits required by the user

    * "**ks**" - The keysize (in bits) of the user's chosen cypher. If the cypher driver reports that this value is "-1", then "**ks**" should be assumed to be 8 bits.
    * "**bs**" - The blocksize (in
bits) of the user's chosen cypher. If the cypher driver reports that
this value is "-1", then "**bs**" should be assumed to be 8 bits.
    * "**hk**"
- The length (in bits) of hashes generated by the user's chosen hash
algorithm. If the hash driver reports that this value is "-1", then "**hk**" should be assumed to be 512 bits.

  </OL>
  <OL>

  </OL>

  * Build the "Volume details block" in memory. The length of this block (in bits) is calculated as:

<blockquote>(((4096 - **sl**) / **bs****)** * **bs**_****_**)** - _hk_</blockquote>

(See the layout breakdown diagrams of the "Critical data area" to understand where this algorithm comes from.)

  * With the user's selection of hash algorithm, hash the "Volume details block" to generate the "Check hash".

  * If the "Check hash" generated has less than "**hk**" bits, it should be right-padded with zero bits until it is "**hk**" bits long.

  * If the "Check hash" generated has more than "**hk**" bits, it should be truncated to the first "**hk**" bits.
* Prepend the "Check hash" onto the "Volume details block" to build the plaintext version of the "Encrypted block"

  * Append the salt onto the user's password

  * Hash the salted password with the user's chosen hash algorithm

  * If the hash generated has less than "**ks**" bits, it should be right-padded with zero bits until it is "**ks**" bits long.

  * If the hash generated has more than "**ks**" bits, it should be truncated to the first "**ks**" bits.

    **The truncated/padded hash will be used as the "critical data key"**
  * Encrypt the plaintext "Encrypted block" with the user's selection of cypher, an IV of "**bs"** zeroed bits, and the "critical data key" 
  * Prepend the salt onto the cyphertext version of the "Encrypted block"

  * Append "Random padding #1" to the "Encrypted block" in order to
pad it out to 4096 bits (512 bytes) and form the complete "Critical
data block"
  * Write the "Critical data block" to the volume file, beginning
from the start of the file for normal DoxBox volumes, or from
the user's offset in the case of a hidden volume.
</OL>

<hr style="width: 100%; height: 2px;">
<A NAME="level_4_heading_5">
#### Mounting DoxBox Volumes
</A>

IMPORTANT: Versions of FreeOTFE (v00.00.0x) that used this CDB format had an implementation fault that caused the Volume Details Block to be incorrectly parsed when it was read in. It incorrectly used the 32 bits starting from offset byte 10 within the Volume Details Block (i.e. data storing the part of the encrypted partition image length) as the VolumeFlags. This has been compensated for in later versions of FreeOTFE, which use a later CDB layout in any case.



To mount a DoxBox, the following information must be determined:</p>

<OL>

  * The location of the critical data block within the volume file* The user's password

  * The length of salt used, in bits (**sl**)* The algorithm used to hash the user's password and salt

  * The cypher used for all en/decryption operations
  * 
The master key to be used to en/decrypt the encrypted partition image

  * 
The method used to generate the different IVs for each sector of the encrypted partition image
  * 
The length of the encrypted partition image

</OL>

<UL>

</UL>

Items 1-3 are obtained by asking the user.

Items 3 and 4 are determined by brute force; by hashing the user's password with the first **sl**
bits of the critical data block with each of the hash algorithms
installed, then using that information to decrypt the remainder of the
critical data block using each of the installed cyphers in turn. After
each decryption, the decrypted critical data block is hashed, and this
hash value is compared to the decrypted version of the check hash.

Items 6-8 are then taken from the successfully decrypted critical data block, and the volume is mounted.

In detail, the following procedure is used:
<OL>

* 
The user is prompted for the following information:

<UL>
* 
The volume's password

* 
The drive letter they wish to mount the volume as.

* 
In case the user opted to change the default amount of "salt" used during
volume creation, the user is also be prompted to enter the size of the salt used, in bits (Call this value "**sl"**).

* The offset within the file where the critical data block is
located. Typically this will be zero, unless a hidden volume is to be
accessed (see information
on "hidden volumes").

</UL>

* 
The first 512 bytes (4096 bits) of the file are read in, beginning from the user specified offset.

* The first "**sl**" bits of which are assumed to be the salt, and are appended to the user's password.

* 
For each hash algorithm installed on the user's computer:

_Note: We will use "_hk_" to describe the length of hash values
generated by each hash algorithm as it is used in turn. This value is as reported by the hash driver.
If the hash driver reports that the hash length "-1", we will use the
smaller of 512 bits, and the length of the data that the hash algorithm actually returns, as "_hk_"._
<OL>
* 
The combined user's password and salt previously generated is hashed, giving an **hk** bit "critical key".

* 
For each cypher installed on the user's computer:

_Note: We will use "_ks_" to describe the keysize of each cypher as it is used in turn,
the keysize being as reported by the cypher driver. If the cypher driver reports that the
cypher's keysize is "-1" (no specific keysize), we will use "_hk_" as "_ks_"._

_Note: We will use "_bs_" to describe the blocksize of the cypher,
as reported by the cypher driver. If the cypher driver reports that the
cypher's keysize is "-1" (no blocksize), we will use 8 bits (1 byte) as "_bs_"._
        <OL>

* 
The "Encrypted block" is extracted from the critical data block. The length of the "Encrypted block" is calculated as:

<blockquote>((4096 - **sl**) / **bs**) * _bs
_</blockquote>
Alternatively, the length of "Random padding #1" can be viewed as being:

<blockquote>((4096 - **sl**) % **bs**)
</blockquote>
Where % represents the modulus, and / integer division* The "Encrypted block" is decrypted using:

          <OL>

            * 
The first "_ks_" bits of the hash value previously generated as the
decryption key; if "_hk_" is less than "_ks_", then the hash will have extra
zero bits appended to it in order to pad it out to "_ks_"

            * 
An IV consisting of a block of "_bs_" bits, all set to 0.

          </OL>

* Next, the "Check hash" and "Volume details block" are extracted
from the decrypted data. In order to do this, the length of the check
hash must be known; if the hash algorithm used on the salted user's
password is reported to generate hashes "-1" bits long, then the check
hash is assumed to be 512 bits long. Otherwise, the actual hash length
is used.
* The "Volume details block" is hashed and right
padded with zero bits to make it the same length as the "Check hash",
which it is then compared with.
If
the hash generated matches the "Check hash", then the "Encrypted
block" is deemed to have been successfully decrypted; a note will be
made of the cypher and hash used, together with the contents of the
decrypted "Volume details block".

In either case, all remaining hash/cypher combinations will be processed in the same manner.</OL>

      

</OL>

* 
After all possible hash/cypher combinations have been exhausted:

<UL>
* 
If no cypher/hash combination successfully decrypted the "Volume details
block", the user will be informed that they have entered incorrect details.

* 
If more than one cypher/hash combination successfully decrypted the "Volume
details block", the user should be prompted to choose which of the combinations they
wish to use. The system will proceed as if only the user selected combination had been successful in decrypting.
* If exactly one cypher/hash combination successfully decrypted the "Volume
details block", then the "Encrypted partition image" will be decrypted
as necessary using this cypher/hash combination, together with the information
obtained from the decrypted "Volume details block".

</UL>
</OL>

As a result of the volume's layout, it is not necessary to store the cypher or hash algorithm used
for en/decryption anywhere. Nor is it necessary to prompt the user for this information since during mounting
a DoxBox formatted volume, this information can be determined dynamically by attempting the mount using
all possible combinations of installed hash/cypher in conjunction with the check hash.

</p>



