<LINK href="docs/styles_common.css" rel="stylesheet" type="text/css">
<LINK rel="shortcut icon" href="src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>


<DIV class="download-group">
<BUTTON class="download">
<a href="https://github.com/t-d-k/doxbox/releases/download/v6.1-beta/InstallDoxBox_v61Beta.exe">Download</a>
</BUTTON> 
<BUTTON class="download-alt">
<a href="https://github.com/t-d-k/doxbox/releases/download/v6.1-beta/DoxBoxExplorer_v6.1.zip">Download DoxBox Portable</a>
</BUTTON>
</DIV>

## Please take the survey on [new features](https://www.surveymonkey.com/s/XFHP5LS) for DoxBox

##	Features

* Easy to use, with a 'wizard' for creating new 'DoxBoxes'.
* Full transparent encryption, DoxBoxes appear as removable disks in Windows Explorer.
* Explorer mode lets you access DoxBoxes when you don't have admin permissions.
* Compatible with Linux encryption, Cryptoloop "losetup", dm-crypt, and LUKS. Linux shell scripts support deniable encryption on Linux.
* Supports smartcards and security tokens.
* Encrypted DoxBoxes can be a file, a partition, or a whole disk.
* Opens legacy volumes created with FreeOTFE
* Runs on Windows Vista onwards (see note below for 64 bit versions).
* Supports numerous hash (including SHA-512, RIPEMD-320, Tiger) and encryption algorithms (Including AES, Twofish, and Serpent) in several modes (CBC, LRW, and XTS), giving more options than any other disk encryption software.
* Optional 'key files' let you use a thumb-drive as a key.
* Portable mode doesn't need to be installed and leaves little trace on 3rd party PCs (administrator rights needed).
* Deniable encryption protects you from 'rubber hose cryptography'.

**Please note this is a Beta version with some known limitations. Particularly on 64 bit Windows the text 'Test Mode' is shown on the desktop.**
	
## New features in version 6.1

* Fix security flaw in [plausible deniability](http://DoxBox.eu/doxbox/plausible_deniability.html) inherited from FreeOTFE
* Prompts before connecting to the internet
* Translations available
* Ease of use improvements including automatically formatting a new Box

## Release notes
*Important: DoxBox in Portable mode will not work on Windows Vista and later 64 bit versions without a extra step before use.*

*DoxBox installed on Windows Vista and later 64 bit versions adds the text "Test Mode" to the Windows desktop. Please see the documentation for details on removing this.*
*On Windows 8 please turn off 'Safe Boot' and disklocker before installing.*
*There has been a report that Kaspersky anti-virus falsely reports DoxBox as having the 'generic.Trojan' virus, please disable or replace this before installing.*

* Please follow these instructions to run DoxBox in portable mode on 64 bit Windows; if you do not do this you will get the error "Windows requires a digitally signed driver" when starting the drivers. There is no need to do this if DoxBox is installed.
	+ Start DoxBox, click 'No' on the prompt to start the portable drivers, and 'OK' on the warning dialog about not having any loaded drivers.
	+ Click the Tools->"Allow Test-signed drivers" menu item.
	+ Reboot	
	+ After rebooting the words "Test Mode" appear in the four corners of the Desktop. Please see the documentation for details on removing this.
	+ After finishing with DoxBox you can click the Tools->"Disallow Test-signed drivers" menu item to remove the "Test Mode" text.
*	To run in portable mode, you need to have admin rights.  
*	Backwards compatibility with older versions of FreeOTFE (before 5.21) will be removed in a future version. Please convert any FreeOTFE volumes. This can be done by creating a new 'box' and copying the files across.
*	Support for the following cyphers will be removed in a future version, please convert to another: xor, plain, single DES. Ditto for the 'plain' hash. 
*	DoxBox does not support encryption of the operating system partition, for this we recommend Ubuntu Linux.
* DoxBox needs to be run as administrator the first time it is run. After that it can be run as an ordinary user. 

####	Passwords
*These issues relate to passwords (keyphrases) containing non-ASCII characters, e.g. accented letters and non Latin scripts, **not** to ASCII special characters like '$&^'.*

*	The handling of keyphrases containing non-ASCII characters will change in a future version. This change will not be backwards compatible. So in this version it is recommended to use only ASCII characters in keyphrases.
*	There are possible bugs in opening volumes created with FreeOTFE with non-ASCII characters. If you experience problems, please use the legacy app to change the password to an ASCII one and retry. Alternatively move the files to a native DoxBox.

For more details, please see the [getting started guide](http://DoxBox.eu/doxbox/getting_started.html) and [FAQ](http://DoxBox.eu/doxbox/FAQ.html).
 
