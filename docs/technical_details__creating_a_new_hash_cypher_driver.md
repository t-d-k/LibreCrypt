

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Technical Details: Creating a New Hash/Cypher Driver</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

### Technical Details: Creating a New Hash/Cypher Driver

These
instructions specify in general terms, the steps in taking an existing
cypher driver (Blowfish, in this case), and modifying it to create a
new cypher driver. The procedure for creating a new hash driver is
practically identical, but one of the existing hash drivers should be
used as a base, instead of the Blowfish cypher.

<OL>

  * Create a new directory to contain your new cypher driver
  <UL>
    * Make a copy of the directory containing the Blowfish cypher driver ("CYPHER_BLOWFISH")

  </UL>
  <UL>
    * Rename this directory to reflect the new cypher's name

  </UL>
  * Modify "my_build_sys.bat"

  <UL>
    * Edit this file using a text editor to change:

    <UL>
      * All instances of "Blowfish" to reflect new cypher's name

      * The files needed

    </UL>

  </UL>
  * Modify "CYPHER_BLOWFISH.vcproj"

  <UL>
    * Rename this file to reflect the new cypher's name

    * Edit this file using a text editor to change:

    <UL>
      * All instances of "Blowfish" to reflect new cypher's name

      * What files are needed, as per my_build_sys.bat

    </UL>
  </UL>
  * Modify "src/sources"

  <UL>
    * Edit this file using a text editor to change:

    <UL>
* All instances of "Blowfish" to reflect new cypher's name
* What files are needed, as per my_build_sys.bat

    </UL>
  </UL>
  * In the "src" directory, rename all the "*Blowfish*" files to reflect the new cypher's name

  * Start VC++ and load the "FreeOTFE.sln" Visual Studio Solution.

  * Add the "....vcproj" project file you modified above into the solution.

  * Within VC++, modify the new FreeOTFECypher**XXX**.rc file to change all instances of "Blowfish" to reflect new cypher's name

  * Within VC++, modify FreeOTFECypher**XXX**.h

  <UL>
    * Change:

  </UL>
  <OL>
    <UL>
      * All instances of "Blowfish" to reflect new cypher name
* Add/remove any "DRIVER_CIPHER_TITLE_**XXX**" entries as required

      * All of the GUID values (this is **important!** The DoxBox GUI uses these values to differentiate between the different cypher implementations)* Set the definition of
"CYPHERS_SUPPORTED" to reflect the number of different cyphers the driver will
provide (i.e. the number of "DRIVER_CIPHER_TITLE_**XXX**" definitions you have)

    </UL>
  </OL>
  * Within VC++, modify FreeOTFECypher**XXX**.c

  <UL>
    * Change:

    <UL>
      * The cipher descriptions returned

      * The initialization and encrypt/decrypt routines to check for, and use the correct cypher

    </UL>
  </UL>
* Modify
"clean.bat", in the top level "src" directory to clean up any object,
garbage, etc files that are created when your driver is built

  * You should now be able to build your new cypher driver, which may then be installed as per any other cypher or hash driver.

</OL>

<A NAME="level_4_heading_1">
#### Hash Length/Blocksize
</A>

When your hash driver is asked for details of the hash algorithms it
provides, the hash length returned for each hash algorithm must be one
of the following:

<TABLE>

<TBODY>
<TR>
<TH style="vertical-align: top; width: 10%;">Hash length

</TH>
<TH style="vertical-align: top; width: 5%;">Validity

</TH>
<TH style="vertical-align: top; width: 25%;">Meaning

</TH>
<TH>Comments
</TH>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash values returned are 0 bits long.</TD>
<TD>Hashes which return zero length hash values cannot be used with DoxBox volumes. (DoxBox volumes use PKCS#5 PBKDF2 (HMAC), which requires that the length of hash values returned is greater than zero.)</TD>

</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Less than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash values returned are of variable length (e.g. the "NULL" hash, which returns its input as the generated hash value.)</TD>
<TD>Hashes which return variable length hash values cannot be used with DoxBox volumes. (DoxBox volumes use PKCS#5 PBKDF2 (HMAC), which requires that the length of the hash values used is fixed.) </TD>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Greater than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash values returned have a fixed, defined length (e.g. SHA-512's hash length is 512 bits)</TD>

<TD>Must be a multiple of 8.</TD>

</TR>
</TBODY>
</TABLE>

When your hash driver is asked for details of the hash algorithms it
provides, the blocksize returned for each hash algorithm must be one
of the following:

<TABLE>

<TBODY>
<TR>
<TH style="vertical-align: top; width: 10%;">Hash blocksize

</TH>
<TH style="vertical-align: top; width: 5%;">Validity

</TH>
<TH style="vertical-align: top; width: 25%;">Meaning

</TH>
<TH>Comments
</TH>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash algorithm does not process input data.</TD>
<TD>Hashes may only have a blocksize of 0 bits if the length of the hash values they output is also 0 bits long, or if they ignore their input.  Hashes which use zero length blocksizes cannot be used for DoxBox volumes. (DoxBox volumes use HMAC, which requires that the blocksize of hashes used is greater than zero.)</TD>

</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Less than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash algorithm processes input data in variable-length blocks.</TD>
<TD>Hashes which use variable length blocksizes cannot be used for DoxBox volumes. (DoxBox volumes use HMAC, which requires that the blocksize of hashes is a fixed size.) </TD>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Greater than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Hash algorithm processes input data in defined, fixed blocks (e.g. SHA-512's block size is 1024 bits)</TD>

<TD>Must be a multiple of 8. </TD>
</TR>
</TBODY>
</TABLE>

<A NAME="level_4_heading_2">
#### Cypher Keysize/Blocksize
</A>

When your hash driver is asked for details of the cyphers it
provides, the keysize returned for each cypher must be one
of the following:

<TABLE>

<TBODY>
<TR>
<TH style="vertical-align: top; width: 10%;">Cypher keysize

</TH>
<TH style="vertical-align: top; width: 5%;">Validity

</TH>
<TH style="vertical-align: top; width: 25%;">Meaning

</TH>
<TH>Comments
</TH>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">No key is used during encryption (e.g.
if the cypher doesn't encrypt data, just returns the plaintext as the
cyphertext; or if the cypher uses a hardcoded key)</TD>

<TD>       </TD>

</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Less than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">The cypher takes variable keylengths (e.g. the "XOR" cypher)</TD>
<TD> </TD>

</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Greater than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">The cypher accepts only a specific keysize (e.g. full-strength DES only accepts 64 bit keys)</TD>

<TD>Must be a multiple of 8. </TD>
</TR>
</TBODY>
</TABLE>

When your hash driver is asked for details of the cyphers it
provides, the blocksize returned for each cypher must be one
of the following:

<TABLE>

<TBODY>
<TR>
<TH style="vertical-align: top; width: 10%;">Cypher blocksize

</TH>
<TH style="vertical-align: top; width: 5%;">Validity

</TH>
<TH style="vertical-align: top; width: 25%;">Meaning

</TH>
<TH>Comments
</TH>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Cypher does not process input data. (e.g. the "NULL" cypher, which just returns the supplied plaintext as cyphertext)</TD>

<TD>If the blocksize is 0, then no IVs will be used for encrypting/decrypting. </TD>
</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Less than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Cypher processes input data in
variable-length blocks. (e.g. XOR processes data in blocks with the
same length as the key being used)</TD>

<TD>If the blocksize isn't fixed, then no IVs will be used for encrypting/decrypting.</TD>

</TR>
<TR>
<TD style="vertical-align: top; width: 10%;">Greater than 0</TD>

<TD style="vertical-align: top; width: 5%;">Valid</TD>

<TD style="vertical-align: top; width: 25%;">Cypher processes input data in defined, fixed blocks (e.g. AES has a block size is 128 bits)
</TD>
<TD>Must be a multiple of 8. </TD>
</TR>
</TBODY>
</TABLE>

<h4>Miscellaneous Comments: Cypher Drivers
</H4>
<UL>
  * 
When called upon to encrypt/decrypt data, if the "IVLength" passed in is 0, then "IV" should be **ignored** (it **may** be set to NULL)

  * When writing a cypher driver, the encrypt/decrypt implementation should **not **write to the input buffer; only the output buffer (i.e. when encrypting, do **not **write to the plaintext buffer passed in; when decrypting, do **not **write
to the cyphertext buffer). An optimisation in the driver involves the
use of a single buffer for input/output. You may find that you'll need
to create a temporary buffer equal to your blocksize when implementing
some modes of operation (e.g. CBC)

* *Important:* Don't read/write to the _input_ plaintext/cyphertext buffer! Only the _output_ ones!
  

</UL>



