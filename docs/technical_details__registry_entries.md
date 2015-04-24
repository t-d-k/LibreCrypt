
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Technical Details: Registry Entries</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

### Technical Details: Registry Entries

LibreCrypt doesn't create any registry entries for itself unless the user chooses to associate ".vol" files with the application, in which case only those registry entries which are required to associate the LibreCrypt executable with the filename extension are created. All user options and settings are stored in a ".ini" file, unless the user _explicitly_ configures them to be stored in the registry.

In addition to this, MS Windows does create a registry entry for each LibreCrypt driver used. This is inevitable; **all** transparent encryption systems running under MS Windows are required to do this in order to function correctly.

The following detail the registry entries are typically created by MS Windows:

Registry key: HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\**&lt;driver name&gt;**

These keys may have the following values under them:

<TABLE>
  <TBODY>
    <TR>
      <TH>Value
      </TH>
      <TH>Type
      </TH>
      <TH>Meaning
      </TH>
    </TR>
    <TR>
      <TD>ErrorControl</TD>

      <TD>DWORD</TD>
      <TD>0x00000001 - Normal error handling</TD>

    </TR>
    <TR>
      <TD>Start</TD>

      <TD>DWORD</TD>
      <TD>0x00000001 - Driver started at system  0x00000003 - Driver started manually</TD>

    </TR>
    <TR>
      <TD>Type</TD>

      <TD>DWORD</TD>

      <TD>0x00000001</TD>

    </TR><TR>
      <TD>Portable</TD>

      <TD>DWORD</TD>

      <TD>This value is optional, but set to 0x00000001 if present.  This value flags that the relevant driver was installed in "portable mode", and should be removed</TD>

    </TR>

  </TBODY>
</TABLE>

* * * 
<A NAME="level_3_heading_1">
### Portable Mode Impact
</A>

 

Although no files are copied to your computers hard drive when using portable mode, because part of the manner in which MS Windows manages device drivers, Windows still writes certain details about the portable mode drivers to the registry. Specifically, the full path and filename of the drivers used together with other basic information on the drivers as detailed above.

When portable mode is stopped, most of this information is deleted by Windows automatically. However:

1. Because of the way in which the registry stores data, an attacker **may** be able to recover that information which has been deleted (this is analogous to deleting a file on your filesystem; although its directory entry may have been marked as "deleted", the data may still be recoverable)
1. When Windows deletes its registry entries, it doesn't delete all of them (e.g. HKLM\SYSTEM\CurrentControlSet\Enum\Root\LEGACY_...)

It is not possible to securely delete the relevant registry entries without "going behind Windows' back" - not exactly recommended when working with kernel mode device drivers!

_It should be noted that this applies equally to *all* disk encryption systems that support any kind of "portable mode"._

Should it be a concern that an attacker may discover which LibreCrypt drivers were being used, it is suggested that you either:

1. Change the filenames of the FreeOTFE drivers you will be using in portable mode (e.g. rename "FreeOTFECypherAES.sys" to "FreeOTFECypherTwofish.sys"). This will cause the data written to the registry to reflect this new filename, hopefully convincing an attacker that the driver used was a different one.
1. If you only use one cypher and hash driver in portable mode, store a number (or all) of the other cypher/hash drivers with your "portable" version of LibreCrypt. Even if an attacker can determine which drivers you were using in portable mode, that attacker will not be able to determine which of the portable drivers you were actually using to encrypt/decrypt your data with.


