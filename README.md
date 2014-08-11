# DoxBox
------------

##	Features

* Easy to use, with a 'wizard' for creating new 'DoxBoxes'.
* Portable mode doesn't need to be installed and leaves little trace on 3rd party PCs (administrator rights needed).
* Full transparent encryption, DoxBoxes appear as removable discs in Windows Explorer.
* Opens legacy volumes created with Truecrypt, E4M, Scramdisc, Bestcrypt, Crosscrypt, FreeOTFE and PGPdisc (only some versions supported).
* Explorer mode lets you access DoxBoxes when you don't have admin permisssions.
* Deniable encryption protects you from 'rubber hose cryptography'.
* Compatible with Linux encryption, Cryptoloop "losetup", dm-crypt, and LUKS. Linux shell scripts support deniable encryption on Linux.
* Supports smartcards and security tokens.
* Encrypted DoxBoxes can be a file, a partition, or a whole disk.
* Runs on Windows XP onwards (see note below for 64 bit versions).
* Supports numerous hash (including SHA-512, RIPEMD-320, Tiger) and encryption algorithms (Including AES, Twofish, and Serpent) in several modes (CBC, LRW, AND XTS), giving more options than any other disk encryption software.
* Optional 'key files' let you use a thumbdrive as a key.

**Please note this is an alpha version with some known limitations. Particularly an extra step is needed after installation on 64 bit Windows to avoid the error message "Windows Requires a digitally signed driver". Please do not report a bug that you get the message "Windows Requires a digitally signed driver" on 64 bit Windows before reading the release notes.**
	
## Release notes
*Important: DoxBox will not work on Windows Vista x64, Windows 7 x64 and later 64 bit versions without a extra step before use.*

* Please follow these instructions on 64 bit Windows, if you do not do this you will get the error "Windows Requires a digitally signed driver" when starting DoxBox.
	+ Click the "Start" button on the Windows taskbar, type "CMD" in the search box, and then press &lt;CTRL+SHIFT+ENTER&gt; (this will open a DOS prompt as administrator) 
	+ In the command prompt window which appears paste:
		
				bcdedit.exe /set TESTSIGNING ON	
	+ Press return and reboot the PC.  
	+ After rebooting the words "Test Mode" appear in the four corners of the Desktop. Please see the documentation for details on removing this.			
*	On W7 & later, to run in portable mode, you need to have admin rights.
*	There is a security flaw in the use of hidden volumes. For now its recommended not to use hidden DoxBoxes. This does not affect normal DoxBoxes and only affects the deniability, not security, of hidden ones.
*	Support for E4M, Bestcrypt, Crosscrypt and ScramDisc containers will be removed in the next version. It's recommended to convert these volumes to DoxBoxes.
<!-- *	Support for Truecrypt volumes will be removed at some point. It's recommended to convert these volumes to DoxBoxes. -->
*	Backwards compatibility with PDA and Windows CE versions of FreeOTFE will be removed in the next version.
*	Backwards compatibility with older versions of FreeOTFE (before 5.21) will be removed in the next version. Please convert any volumes. This can be done by creating a new 'box' and copying the files across.
*	Support for the following cyphers will be removed in the next version, please convert to another: xor, plain, single DES. Ditto for the 'plain' hash. 
*	The gladman implementation of the twofish cypher (available in FreeOTFE) has been removed.
*	DoxBox does not support encryption of the Operating System partition, for this I recommend Ubuntu Linux.

####	Passwords
*These issues relate to passwords containing non-ASCII characters, e.g. accented letters and non Latin scripts, **not** to ASCII special characters like '$&^'.*

*	The handling of passwords containing non-ASCII characters will change in the next version. This change will not be backwards compatible. So in this version it is recommended to use only ASCII characters in passwords.
*	There are possible bugs in opening volumes created with legacy apps (e.g. Truecrypt) with non-ASCII characters. If you experience problems, please use the legacy app to change the password to an ASCII one and retry. Alternatively move the files to a native DoxBox.

For more documentation, please see [index.md](docs/index.md).

