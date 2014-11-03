

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Installation and Upgrading from a Previous Version</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>

***      
            
## Installation and Upgrading from a Previous Version

If you wish to use DoxBox in portable mode, you do **not** have to carry out the installation described below; simply :

1. Unzip the DoxBox_portable.zip file to wherever you want 
2. When logged on as a user with administrator rights, double-click on "DoxBox.exe" (i.e. the file with the black box icon).
3. Click on the menu Tools->Allow Test-signed Drivers.
4. Reboot.

Then you can then start using DoxBox in [Portable Mode](portable_mode.html).

Alternatively, to install the app, which will allow non-administrator users to create and open DoxBoxes, run _InstallDoxBox\_vNN.exe_ as described below.

A manual installation procedure is also detailed below.

* * * 
<A NAME="level_3_heading_1">
## Automatic installation
</A>

1. Download the DoxBox installer called InstallDoxBox_vNN.exe where NN is the version number, and run it. Accept any prompts.
1. After prompting you for some simple details, DoxBox will be installed on your PC.
1. Reboot your PC after the installation is complete.
1. You will see the text that Windows is in 'Test Mode' on the desktop - to remove this see [here](impact_of_kernel_driver_signing.html). 
1. Right click on the DoxBox icon and click "run as Administrator" - you will see a dialog saying "Driver Installation Successful" (the first time DoxBox is run it installs its drivers).
1. After that DoxBox may be run normally as a non-administrator.

### Trouble shooting

* If you get a dialog saying "Windows requires a digitally signed driver", you forgot to reboot before running DoxBox. Reboot and try again.
* If you get a dialog saying "The main DoxBox driver does not appear to be installed", then please report this as a bug, and follow the manual installation below, from stage 4.
* DoxBox does not run on Windows XP. Please use FreeOTFE instead.

* * * 
<A NAME="level_3_heading_2">
## Manual installation
</A>

1. Uncompress the release ZIP to where you would the software installed. 
2. Login to your computer as administrator (if appropriate) - installation of the device drivers requires administrator privileges to create the registry entries, etc
3. Create shortcuts to "DoxBox.exe", if required.
4. Launch "DoxBox.exe". You will be prompted that the system could not connect to the main FreeOTFE driver - ignore this error for now as you have not yet installed the drivers.
5. Click on the menu Tools->Allow Test-signed Drivers, and reboot.
6. Restart DoxBox.
7. For this step, you may see the standard UAC consent/credential dialog because you are about to install a device driver, which requires administrator privileges. 		Authorise DoxBox to continue by clicking "Allow" or entering your Administrator's password to continue.

	Either:
	* Click Tools->Install DoxBox Drivers
	
	Or:
	* Click "File -> Drivers...". Install each of the ".sys" files included with the release in turn, as follows:
		2. Click the "Install..." button
		2. Select the ".sys" file to be installed from the release, and click "OK". (This should copy the driver file into your &lt;windows&gt;\system32\drivers dir, and setup a few registry entries)
		2. Select the driver's name which should now appear in the list of installed drivers, and click "Start". (This should start the driver selected)
		2. Change the driver's startup option to "At system startup" and click "Update"	
9. Click the "Close" button

These steps will install and setup a DoxBox in a minimalist installation. With these steps complete, you may now use DoxBox to create and open DoxBox and Linux encrypted volumes.

The drivers installed are configured such that they **do not automatically startup when your system boots**. Once you are happy that DoxBox is stable enough, you may return to the driver management dialog, select each driver in turn, and set them to start automatically at system startup.

Note: In order to use DoxBox, you **must** have the following drivers **installed**, and **running** as the bare minimum for DoxBox to operate correctly:

  1. The main "FreeOTFE" driver
  1. At least one hash driver ("FreeOTFEHash...")
  1. At least one cypher driver ("FreeOTFECypher...")

A number of the cyphers (e.g. RC6 and Twofish) have multiple drivers; these reflect different implementations (e.g. DoxBox comes with three Twofish implementations; one based on the libtomcrypt library, one based on the optimised reference implementation, and one based on Brian Gladman's implementation). Each of these drivers provides pretty much the same functionality, it's just the implementation that differs.	 
**Were multiple drivers are provided for the same cypher, you only need to install one of them**. You **can** mix drivers based on different implementations (e.g. install the Gladman version of Twofish, while installing the libtomcrypt version of AES). Installing **all** of the drivers them is harmless, but does mean that you'll be prompted which one to use during mounting a volume, if more than one can be used to encrypt/decrypt that volume.

<A NAME="level_4_heading_1">
## Upgrading from a Previous Version, or from FreeOTFE
</A>

Because of potential changes within the driver API, you must ensure that you completely uninstall your existing DoxBox installation before installing and using the latest version. Please see the section on [uninstalling](uninstalling.html) for details on how to do this.
This does not apply if upgrading from FreeOTFE 5.2 to DoxBox 6.0, as the driver API is the same.

<A NAME="level_5_heading_1">
### Special Notes if Upgrading from v2.00 of FreeOTFE
</A>

Although the FreeOTFE drivers are backwardly compatible, v2.00 of the main FreeOTFE driver cannot correctly use LRW or XTS encrypted volumes. Such volumes may be mounted with the v2.00 driver, but their contents will not be usable.

<A NAME="level_5_heading_2">
### Special Notes if Upgrading from v00.00.02 of FreeOTFE
</A>

With the possible exception of those volumes which were generated using the "NULL" hash algorithm (which was only intended for testing purposes), the latest version of DoxBox should be backward compatible with existing volume files and you should experience no problems with mounting and using them.

You will notice that when mounting volumes, you are presented with more options than with your previous version of DoxBox. For the purposes of mounting older volumes, these additional controls can be ignored **except for the "salt length" option**. Newer versions of DoxBox default this value to 256 bits, and you may well have to change this to the previous default of 128 bits. 

When mounting DoxBoxes, DoxBox will attempt to mount using the latest CDB format, only falling back and attempting to mount using the older CDB format if this fails.

Although DoxBox is backward compatible with volumes created with FreeOTFE older than version 5.2, it is **highly recommended** that you update your volume files to ensure that they use the new CDB format, as this will allow you to take advantage of the numerous security improvements that the latest version provides. The easiest way to update your volume files to the new CDB format is to simply change their passwords by selecting "Tools | Change volume/keyfile password/details...".

IMPORTANT: If you created any ".les" (Linux Encryption Settings) files, please double check the next time you reload them, and ensure that your settings are correct. Changes due to ongoing development **may** cause some settings to change; to fix, simply confirm your settings are correct, and save them out again.



