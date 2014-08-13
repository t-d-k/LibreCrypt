

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Installation and Upgrading from a Previous Version: PC Version</TITLE>

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

      
            
## Installation and Upgrading from a Previous Version: PC Version

*NOTE: Windows Vista x64 (64 bit) users _only_ should _also_ read the [additional information for Windows Vista x64 users](impact_of_kernel_driver_signing.htm) section, in order to configure their system before use*

if you wish to use FreeOTFE in portable mode, you do **not **have to carry out the installation described below; simply copy the binary (".exe" and ".sys") files to wherever you would like them stored, and doubleclick on "DoxBox.exe" (i.e. the file with the combination lock icon). From there, you can then start using FreeOTFE in [Portable Mode](portable_mode.htm).

Alternatively, if you would like to carry out a full installation, which will allow non-administrator users to mount and use DoxBoxes, follow _either_ of the automatic or manual installation instructions detailed below.

* * * 
<A NAME="level_3_heading_1">
### Automatic installation
</A>

1. Download the "installer" version of DoxBox, and run the executable.
1. After prompting you for some simple details, DoxBox will be automatically installed on your PC.

If you are using the 64 bit version of Windows Vista (Vista x64), you will have to reboot your PC after the automatic installation is complete.

* * * 
<A NAME="level_3_heading_2">
### Manual installation
</A>

  1. Uncompress the release ZIP to where you would the software installed. 
  1. Login to your computer as administrator (if appropriate) - installation of the device drivers requires administrator privileges to create the registry entries, etc
  1. Create shortcuts to "DoxBox.exe", if required.
  1. Launch "DoxBox.exe". You will be prompted that the system could not connect to the main FreeOTFE driver - ignore this error for now as you have not yet installed the main FreeOTFE driver.
  1. Select "File | Drivers...". If you are running under Windows Vista, you will see the standard UAC consent/credential dialog at this point because you are about to install a device driver, which requires Administrator privileges. Authorise DoxBox to continue by clicking "Allow" or entering your Administrator's password to continue.
  1. Install each of the ".sys" files included with the release in turn, as follows:

  
    2. Click the "Install..." button
    2. Select the ".sys" file to be installed from the release, and click "OK". (This should copy the driver file into your &lt;windows&gt;\system32\drivers dir, and setup a few registry entries)
    2. Select the driver's name which should now appear in the list of installed drivers, and click "Start". (This should start the driver selected)
    2. Change the driver's startup option to "At system startup" and click "Update"

  1. Click the "Close" button

These steps will install and setup a DoxBox in a minimalist installation. With these steps complete, you may now use DoxBox to create and mount DoxBox and Linux encrypted volumes.

The drivers installed are configured such that they **do not automatically startup when your system boots**. Once you are happy that DoxBox is stable enough, you may return to the driver management dialog, select each driver in turn, and set them to start automatically at system startup.

Note: In order to use DoxBox, you **must **have the following drivers **installed**, and **running** as the bare minimum for DoxBox to operate correctly:

  1. The main "FreeOTFE" driver
  1. At least one hash driver ("FreeOTFEHash...")
  1. At least one cypher driver ("FreeOTFECypher...")

A number of the cyphers (e.g. RC6 and Twofish) have multiple drivers; these reflect different implementations (e.g. DoxBox comes with three Twofish implementations; one based on the libtomcrypt library, one based on the optimised reference implementation, and one based on Brian Gladman's implementation). Each of these drivers provides pretty much the same functionality, it's just the implementation that differs.	 
**Were multiple drivers are provided for the same cypher, you only need to install one of them**. You **can** mix drivers based on different implementations (e.g. install the Gladman version of Twofish, while installing the libtomcrypt version of AES). Installing **all** of the drivers them is harmless, but does mean that you'll be prompted which one to use during mounting a volume, if more than one can be used to encrypt/decrypt that volume.

<A NAME="level_4_heading_1">
#### Upgrading from a Previous Version
</A>

Because of potential changes within the driver API, you must ensure that you completely uninstall your existing DoxBox installation before installing and using the latest version. Please see the section on [uninstalling](uninstalling.htm) for details on how to do this.

<A NAME="level_5_heading_1">
##### Special Notes if Upgrading from v2.00
</A>

Although the FreeOTFE drivers are backwardly compatible, v2.00 of the main FreeOTFE driver cannot correctly use LRW or XTS encrypted volumes. Such volumes may be mounted with the v2.00 driver, but their contents will not be usable.

<A NAME="level_5_heading_2">
##### Special Notes if Upgrading from v00.00.02
</A>

With the possible exception of those volumes which were generated using the "NULL" hash algorithm (which was only intended for testing purposes), the latest version of DoxBox should be backward compatible with existing volume files and you should experience no problems with mounting and using them.

You will notice that when mounting volumes, you are presented with more options than with your previous version of DoxBox. For the purposes of mounting older volumes, these additional controls can be ignored **except for the "salt length" option**. Newer versions of DoxBox default this value to 256 bits, and you may well have to change this to the previous default of 128 bits. 

When mounting DoxBoxes, DoxBox will attempt to mount using the latest CDB format, only falling back and attempting to mount using the older CDB format if this fails.

Although DoxBox is backward compatible with existing volumes, it is **very highly recommended** that
you update your volume files to ensure that they use the new CDB
format, as this will allow you to take advantage of the numerous
security improvements that the latest version provides. The easiest way
to update your volume files to the new CDB format is to simply change
their passwords by selecting
"Tools | Change volume/keyfile password/details...".

IMPORTANT: If you created any ".les" (Linux Encryption Settings) files,
please double check the next time you reload them, and ensure that your
settings are correct. Changes due to ongoing development **may** cause some settings to change; to fix, simply confirm your settings are correct, and save them out again.



