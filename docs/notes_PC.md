

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. Using this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Miscellaneous Notes: PC Version Specific</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="../src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](../src/Common/Common/images/DoxBox128.png)](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.squte.com/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Miscellaneous Notes

<UL>

* After creating a new volume, it must be mounted and formatted. After this, it is highly recommended that you overwrite all the free space on the drive ("Tools | Overwrite free space"...)

* Linux encryption settings files (".les") files are straightforward text files which contain the settings entered.
* Peter Gutmann's "cryptlib" may (**optionally**) be used as an RNG, provided that it has been installed correctly. This may be downloaded from [http://www.cs.auckland.ac.nz/~pgut001/cryptlib/](http://www.cs.auckland.ac.nz/%7Epgut001/cryptlib/). 
After installation, the "cryptlib" option will no longer be greyed out on RNG selection dialogs.
* User settings configured via the "View | Options" menu are stored within a configuration file (".ini" file) which is located in the same directory the DoxBox executable is launched from. User options are **not** stored within the registry, unless configured to store them in it. By storing user settings in a separate
file, as opposed to the registry, DoxBox achieves two things:
  <OL>
    
    * If
DoxBox is stored on removable media (e.g. a USB flash drive, CDROM), your settings can be stored together with DoxBox; there is no need to configure DoxBox every time you use it
on a different computer - this would not be possible to do if the registry
was used.
    * When user settings are stored in a flat file, as opposed to the registry, security
is increased. It is trivial to overwrite a simple file if needed, but
removing registry entries completely is another matter.
  </OL>
* Creating an encrypted partition/disk will **overwrite** whatever data was stored on the partition/disk you select. **Be careful!**
* An additional option is available to allow a program to be automatically executed:
  
  <UL>
    * After a volume has been mounted
    * Before a volume is dismounted
    * After a volume is dismounted
  </UL>

This functionality gives significantly more flexibility
than the standard Windows "autorun" feature, and allows automated
integrity checking, setup, cleanup to be carried out

* For security reasons, any file launched by the post-mount and
pre-dismount options must reside on the encrypted volume; a path
relative to the root directory
of the mounted volume should be specified in the options dialog
(e.g. "\MyFiles\start.bat").
For the post-dismount option, the absolute path to the file to be
launched should be specified (e.g. "C:\volume_just_dismounted.bat")

* The pre-dismount executable must terminate before the dismount will be carried out.
* The pre-dismount executable will not be launched in case of a forced/emergency dismount

</UL>

<A NAME="level_4_heading_1">
#### Windows Vista and Windows 7 Specific
</A>
##### User Access Control (UAC)

Windows Vista incorporates a [new security system](http://www.microsoft.com/windows/products/windowsvista/features/details/useraccountcontrol.mspx) called "User Access Control" (UAC), which is there to help prevent malicious software from doing things which could be harmful to your computer.

As part of this new security system, you will find a number of DoxBox's menuitems are marked with a "shield" icon - specifically, those which
relate to installing or changing DoxBox's drivers, starting/stopping portable mode, and formatting.

Whenever you attempt to use functionality which is marked with one of
these icons, Windows will display a dialog (the "consent/credential"
dialog), asking for your permission to allow
DoxBox to continue. **This is for your protection**, and is perfectly normal. You will be shown this dialog even if you are logged on as an Administrator

Although the DoxBox binaries are digitally signed using
the Microsoft standard, Windows refuses to identify DoxBox, and as such
this dialog will state that "An unidentified
program wants access to your computer". Again, this is perfectly
normal; if you would like to check that your copy of DoxBox is an
unmodified, original copy, you may do so by checking the hashes/signatures available
from the [DoxBox web site](http://DoxBox.squte.com/).

If you are logged on as a "standard" (i.e. non-Administrator) user, the prompt you are shown will also ask for an Administrator's password. It
should be emphasised that it is Windows Vista itself which is
generating these prompts, and not DoxBox, which will have no access
to the password you type in. The same type of warning dialogs will 
appear when
you attempt to (for example) change the system time or dete by going to Window's Control Panel, selecting
"Date and Time", and then changing the computer's time/date.

If you are happy for DoxBox to carry out the operation you requested
of it, you should select the relevant option from the consent/credential dialog to allow DoxBox to proceed.

You can find out more about UAC from the [Microsoft web site](http://technet.microsoft.com/en-us/windowsvista/aa906022.aspx).



