<LINK href="docs/styles_common.css" rel="stylesheet" type="text/css">
<LINK rel="shortcut icon" href="src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
[![DoxBox logo](src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
[![LibreCrypt logo](src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
_[LibreCrypt](http://Librecrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>


<DIV class="download-group">
<BUTTON class="download">
<a href="https://github.com/t-d-k/doxbox/releases/download/v6.2-beta/InstallLibreCrypt_v62Beta.exe">Download</a>
</BUTTON> 
</DIV>
<DIV class="download-group">
<BUTTON class="download-alt">
<a href="https://github.com/t-d-k/doxbox/releases/download/v6.2-beta/LibreCryptExplorer_v6.2.zip">Download LibreCrypt Portable</a>
</BUTTON>
</DIV>

## Please take the survey on [LibreCrypt](https://www.surveymonkey.com/r/PFC8VJ6) 

##	Features

* Easy to use, with a 'wizard' for creating new 'containers'.
* Full transparent encryption, containers appear as removable disks in Windows Explorer.
* Explorer mode lets you access containers when you don't have administrator permissions.
* Compatible with Linux encryption, Cryptoloop "losetup", dm-crypt, and LUKS. Linux shell scripts support deniable encryption on Linux.
* Supports smartcards and security tokens.
* Encrypted containers can be a file, a partition, or a whole disk.
* Opens legacy volumes created with FreeOTFE
* Runs on Windows Vista onwards (see note below for 64 bit versions).
* Supports many hash (including SHA-512, RIPEMD-320, Tiger) and encryption algorithms (Including AES, Twofish, and Serpent) in several modes (CBC, LRW, and XTS).
* Optional 'key files' let you use a thumb-drive as a key.
* Portable mode doesn't need to be installed and leaves little trace on 3rd party PCs (administrator rights needed).
* Deniable encryption in case of 'rubber hose cryptography'.

**Please note this is a Beta version with some known limitations. Particularly on 64 bit Windows the text 'Test Mode' is shown on the desktop.**
	
## New features in version 6.2

 * Change of name to 'LibreCrypt'
 * Many UI bugs fixed - see [Issue 20](https://github.com/t-d-k/doxbox/issues/20)
 * Improved support for GPT partitioned discs.
 * Improved new password dialog.
 * Improved partition information when running as non-admin.

## Release notes
*Important: LibreCrypt in Portable mode will not work on Windows Vista and later 64 bit versions without a extra step before use.*

*LibreCrypt installed on Windows Vista and later 64 bit versions adds the text "Test Mode" to the Windows desktop. Please see the documentation for details on removing this.*

* On Windows 8 please turn off 'Safe Boot' and disklocker before installing.
* There has been a report that Kaspersky anti-virus falsely reports LibreCrypt as having the 'generic.Trojan' virus, please disable or replace this before installing.
* LUKs partitions on LVM volumes, or LVM volumes in LUKS partitions cannot be accessed due to Windows limitations
* This release has changes to the drivers, if upgrading from previous versions of LibreCrypt,DoxBox or FreeOTFE please completely uninstall the old version first

* Please follow these instructions to run LibreCrypt in portable mode on 64 bit Windows; if you do not do this you will get the error "Windows requires a digitally signed driver" when starting the drivers. There is no need to do this if LibreCrypt is installed.
	+ Start LibreCrypt, click 'No' on the prompt to start the portable drivers, and 'OK' on the warning dialog about not having any loaded drivers.
	+ Click the Tools->"Allow Test-signed drivers" menu item.
	+ Reboot	
	+ After rebooting the words "Test Mode" appear in the four corners of the Desktop. Please see the documentation for details on removing this.

*	To run in portable mode, you need to have admin rights.  
*	Support for the following cyphers will be removed in a future version, please convert to another: xor, plain, single DES. Ditto for the 'plain' hash. 
*	LibreCrypt does not support encryption of the operating system partition, for this we recommend Ubuntu Linux.
* LibreCrypt needs to be run as administrator the first time it is run. After that it can be run as an ordinary user. 

####	Passwords
*These issues relate to passwords (keyphrases) containing non-ASCII characters, e.g. accented letters and non Latin scripts, **not** to ASCII special characters like '$&^'.*

*	The handling of keyphrases containing non-ASCII characters will change in a future version. This change will not be backwards compatible. So in this version it is recommended to use only ASCII characters in keyphrases.
*	There are possible bugs in opening volumes created with FreeOTFE with non-ASCII characters. If you experience problems, please use the legacy app to change the password to an ASCII one and retry. Alternatively move the files to a native container.

For more details, please see the [getting started guide](http://LibreCrypt.eu/doxbox/getting_started.html) and [FAQ](http://LibreCrypt.eu/doxbox/FAQ.html).
 
