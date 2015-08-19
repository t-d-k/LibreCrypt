
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Installation and Upgrading from a Previous Version</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">

<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>

***
<SPAN class="tip">
The latest version of this document can be found at the [LibreCrypt project site](https://github.com/t-d-k/librecrypt)
</SPAN>   

## SafeBoot
Regardless of which installation procedure is used 'Safe Boot' needs to be disabled first, it is not possible to use 'Test Signed' drivers with this on.
Once installed it should be possible to turn it on again.

## Kaspersky
There have been reports that Kapsersky anti-virus falsely reports LibreCrypt as a virus. Please disable this before installing.
          
## Installation and Upgrading from a Previous Version

If you wish to use LibreCrypt in portable mode, you do **not** have to carry out the installation described below; simply:

1. Unzip the LibreCrypt_portable.zip file to wherever you want 
2. When logged on as a user with administrator rights, double-click on "LibreCrypt.exe" (i.e. the file with the black box icon).
3. Click on the menu Tools->Allow Test-signed Drivers.
4. Reboot.

Then you can then start using LibreCrypt in [Portable Mode](portable_mode.md).

Alternatively, to install the app, which will allow non-administrator users to create and open containers, run _InstallLibreCrypt\_vNN.exe_ as described below.

A manual installation procedure is also detailed below.

* * * 
<A NAME="level_3_heading_1">
## Automatic installation
</A>

1. Download the LibreCrypt installer called InstallLibreCrypt_vNN.exe where NN is the version number.
1. Turn off 'Safe Boot', & Disklocker
1. Run InstallLibreCrypt_vNN.exe, accept any prompts.
1. After prompting you for some simple details, LibreCrypt will be installed on your PC.
1. Reboot your PC after the installation is complete.
1. You will see the text that Windows is in 'Test Mode' on the desktop - to remove this see [here](impact_of_kernel_driver_signing.md). 
1. Right click on the LibreCrypt icon and click "run as Administrator" - you will see a dialog saying "Driver Installation Successful" (the first time LibreCrypt is run it installs its drivers).
1. After that LibreCrypt may be run normally as a non-administrator.

### Trouble shooting


* If you get a dialog saying "Windows requires a digitally signed driver", you forgot to reboot before running LibreCrypt. Reboot and try again.
* If you get a dialog saying "The main LibreCrypt driver does not appear to be installed", then please report this as a bug, and follow the manual installation below, from stage 4.
* LibreCrypt does not run on Windows XP. Please use FreeOTFE instead.

* * * 
<A NAME="level_3_heading_2">
## Manual installation
</A>

1. Uncompress the release ZIP to where you would the software installed. 
2. Login to your computer as administrator (if appropriate) - installation of the device drivers requires administrator privileges to create the registry entries, etc
3. Create shortcuts to "LibreCrypt.exe", if required.
4. Launch "LibreCrypt.exe". You will be prompted that the system could not connect to the main FreeOTFE driver - ignore this error for now as you have not yet installed the drivers.
5. Click on the menu Tools->Allow Test-signed Drivers, and reboot.
6. Restart LibreCrypt.
7. For this step, you may see the standard UAC consent/credential dialog because you are about to install a device driver, which requires administrator privileges. 		Authorise LibreCrypt to continue by clicking "Allow" or entering your Administrator's password to continue.

	Either:
	* Click Tools->Install LibreCrypt Drivers
	
	Or:
	* Click "File -> Drivers...". Install each of the ".sys" files included with the release in turn, as follows:
		2. Click the "Install..." button
		2. Select the ".sys" file to be installed from the release, and click "OK". (This should copy the driver file into your &lt;windows&gt;\system32\drivers dir, and setup a few registry entries)
		2. Select the driver's name which should now appear in the list of installed drivers, and click "Start". (This should start the driver selected)
		2. Change the driver's startup option to "At system startup" and click "Update"	
9. Click the "Close" button

These steps will install and setup a container in a minimalist installation. With these steps complete, you may now use LibreCrypt to create and open LibreCrypt and Linux encrypted containers.

The drivers installed are configured such that they **do not automatically startup when your system boots**. Once you are happy that LibreCrypt is stable enough, you may return to the driver management dialog, select each driver in turn, and set them to start automatically at system startup.

Note: In order to use LibreCrypt, you **must** have the following drivers **installed**, and **running** as the bare minimum for LibreCrypt to operate correctly:

  1. The main "FreeOTFE" driver
  1. At least one hash driver ("FreeOTFEHash...")
  1. At least one cypher driver ("FreeOTFECypher...")

A number of the cyphers (e.g. RC6 and Twofish) have multiple drivers; these reflect different implementations (e.g. LibreCrypt comes with three Twofish implementations; one based on the libtomcrypt library, one based on the optimised reference implementation, and one based on Brian Gladman's implementation). Each of these drivers provides pretty much the same functionality, it's just the implementation that differs.	 
**Were multiple drivers are provided for the same cypher, you only need to install one of them**. You **can** mix drivers based on different implementations (e.g. install the Gladman version of Twofish, while installing the libtomcrypt version of AES). Installing **all** of the drivers them is harmless, but does mean that you'll be prompted which one to use during opening a container, if more than one can be used to encrypt/decrypt that container.

<A NAME="level_4_heading_1">
## Upgrading from a Previous Version, or from FreeOTFE
</A>

Because of potential changes within the driver API, you must ensure that you completely uninstall your existing LibreCrypt installation before installing and using the latest version. Please see the section on [uninstalling](uninstalling.md) for details on how to do this.
This does not apply if upgrading from FreeOTFE 5.2 to LibreCrypt 6.0, as the driver API is the same.

<A NAME="level_5_heading_1">
### Special Notes if Upgrading from versions before 5.2 of FreeOTFE
</A>

LibreCrypt dropped support for containers created in these versions in LibreCrypt version 6.3.

Containers created with these versions will not open in LibreCrypt 6.3 or later.

The easiest way to update your container files to the new format is to:
* Open the containers in LibreCrypt version 6.2 or before, or FreeOTFE 5.2
* Change the container passwords by selecting "Tools | Change Container/keyfile password/details...".

IMPORTANT: If you created any ".les" (Linux Encryption Settings) files, please double check the next time you reload them, and ensure that your settings are correct. Changes due to ongoing development **may** cause some settings to change; to fix, simply confirm your settings are correct, and save them out again.




