

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Miscellaneous Notes: PC Version Specific</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.tdksoft.co.uk/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.tdksoft.co.uk/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Miscellaneous Notes



* After creating a new container, it is automatically opened and formatted. 
* Linux encryption settings files (".les") files are straightforward text files which contain the settings entered.
* Peter Gutmann's "cryptlib" may (**optionally**) be used as an RNG, this is inculded in the LibreCrypt installation. It may also be downloaded from [http://www.cs.auckland.ac.nz/~pgut001/cryptlib/](http://www.cs.auckland.ac.nz/%7Epgut001/cryptlib/). 
If installed, the "cryptlib" option is not greyed out on RNG selection dialogs.
* User settings configured via the "View | Options" menu are stored within a configuration file (".ini" file) which is located in the default Windows location (usually 'C:\Users\ [your name] \AppData\Roaming\LibreCrypt.ini') If stored in the same directory as the LibreCrypt executable, it must be run as an Administrator. User options are **not** stored within the registry, unless configured to store them in it. User settings are stored in a separate file, instead of the registry, for two reasons:
   
    1. If LibreCrypt is stored on removable media (e.g. a USB flash drive, CDROM), your settings can be stored together with LibreCrypt; there is no need to configure LibreCrypt every time you use it on a different computer - this would not be possible to do if the registry was used.
    2. When user settings are stored in a flat file, as opposed to the registry, security is increased. It is trivial to overwrite a simple file if needed, but
removing registry entries completely is another matter.

* Creating an encrypted partition/disk will **overwrite** whatever data was stored on the partition/disk you select. **Be careful!**
* An additional option is available to allow a program to be automatically executed:
  
    * After a container has been opened
    * Before a container is closed
    * After a container is closed

This functionality gives significantly more flexibility than the standard Windows "autorun" feature, and allows automated integrity checking, setup, cleanup to be carried out

* For security reasons, any file launched by the post-open and pre-close options must reside on the encrypted container; a path relative to the root directory of the opened container should be specified in the options dialog (e.g. "\MyFiles\start.bat").
For the post-close option, the absolute path to the file to be launched should be specified (e.g. "C:\container_just_closed.bat")

* The pre-close executable must terminate before the close will be carried out.
* The pre-close executable will not be launched in case of a forced/emergency close


<A NAME="level_4_heading_1">
#### Windows Vista and Windows 7 Specific
</A>
##### User Access Control (UAC)

Windows Vista incorporates a [new security system](http://www.microsoft.com/windows/products/windowsvista/features/details/useraccountcontrol.mspx) called "User Access Control" (UAC), which is there to help prevent malicious software from doing things which could be harmful to your computer.

As part of this new security system, you will find a number of LibreCrypt's menuitems are marked with a "shield" icon - specifically, those which
relate to installing or changing LibreCrypt's drivers, starting/stopping portable mode, and formatting.

Whenever you attempt to use functionality which is marked with one of these icons, Windows will display a dialog (the "consent/credential" dialog), asking for your permission to allow LibreCrypt to continue. **This is for your protection**, and is perfectly normal. You will be shown this dialog even if you are logged on as an Administrator

Although the LibreCrypt binaries are digitally signed using the Microsoft standard, Windows refuses to identify LibreCrypt, and as such this dialog will state that "An unidentified program wants access to your computer". Again, this is perfectly normal; if you would like to check that your copy of LibreCrypt is an unmodified, original copy, you may do so by checking the hashes/signatures available from the [LibreCrypt web site](http://LibreCrypt.tdksoft.co.uk/).

If you are logged on as a "standard" (i.e. non-Administrator) user, the prompt you are shown will also ask for an Administrator's password. It should be emphasised that it is Windows Vista itself which is generating these prompts, and not LibreCrypt, which will have no access to the password you type in. The same type of warning dialogs will  appear when you attempt to (for example) change the system time or dete by going to Window's Control Panel, selecting "Date and Time", and then changing the computer's time/date.

If you are happy for LibreCrypt to carry out the operation you requested of it, you should select the relevant option from the consent/credential dialog to allow LibreCrypt to proceed.

You can find out more about UAC from the [Microsoft web site](http://technet.microsoft.com/en-us/windowsvista/aa906022.aspx).



