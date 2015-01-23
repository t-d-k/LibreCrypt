
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Getting Started Guide</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***
<SPAN class="tip">
The latest version of this document can be found at the [DoxBox project site](https://github.com/t-d-k/doxbox)
</SPAN>       
            
## Getting Started Guide

Pretty much everything in DoxBox should be fairly self-explanatory. Most items have 'tool-tips', so if you hover the mouse over them you will see a description.

  * [Installation](#level_3_heading_1)
  * [Portable mode](#port)
  * [Using DoxBox](#level_3_heading_2)
  * [Creating a new DoxBox](#level_3_heading_3)
  * [Opening DoxBoxes](#level_3_heading_4)
  * [Closing DoxBoxes](#level_3_heading_5)
  * [Changing a DoxBox's keyphrase](#level_3_heading_6)


* * * 
<A NAME="level_3_heading_1">
### Installation
</A>

Before DoxBox can be used, it must first be installed. You need to have administrator privileges to install, after it will prompt you to reboot. 
After rebooting the words 'test mode' appear on the Desktop.
After it has been installed, run it the first time as administrator; this will install the drivers. 

After that, it may be run as a non-administrator.

Please see the section on [Installation and Upgrading](installation_and_upgrading__PC.html) for more instructions.

<A NAME="port">
### Portable mode
</A>

Before DoxBox Portable can be used, it must be unzipped from the portable zip file.
It needs to be run as administrator, in order to install the drivers in 'portable mode'. 

Please see the section on [Installation and Upgrading](installation_and_upgrading__PC.html) for more instructions.

* * * 
<A NAME="level_3_heading_2">
### Using DoxBox
</A>

Once DoxBox is installed (or started in portable mode), securing your data is simple:

1. Create a DoxBox to store your files in
1. 'Open' the Box created.
1. It will appear as a new drive on your computer.
		
	_Anything_ and _everything_ stored on this drive (documents, pictures, videos, software - whatever you like) will be automatically encrypted and stored within the Box you created, at the time that it's written to the drive.
	
	The encryption process is totally transparent, and is carried out 'on-the-fly' as data is written to the drive. At no time is any unencrypted data stored on the disc. Similarly, decryption is carried out transparently when data is read from it.	
	
	You can drag files to and from this drive, open and save files from applications to it, run applications directly from it - in fact, _everything_ that you can do with a normal drive, you can do with a DoxBox.

1. To secure your data, simply 'lock' the Box. At that point, the drive disappears and the encryption/decryption key is overwritten in memory - making your data totally inaccessible until the keyphrase is supplied, and the DoxBox is opened again.

The following sections give more detailed instructions on how do carry out each of these steps.

* * * 
<A NAME="level_3_heading_3">
### Creating a new Box
</A>

In order to use DoxBox, you must first create a "DoxBox" (also called a "volume") to represent your new drive.

<SPAN CLASS="tech_note">
This consists of using DoxBox to create a large file (or setup a partition) on your computer's hard drive filled with random data.
This volume will hold an encrypted "disk image" of your DoxBox, and is where DoxBox will store all data written to your virtual drive. 
</SPAN>

This file (or partition) can then be "opened", at which point a virtual drive will appear on your computer - anything stored on one of these virtual drives will be automatically encrypted before being written to the file.

To create a new DoxBox, click the 'New' button on the toolbar to show the "New Box wizard", which will guide you through the process in a series of simple steps.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/images/screenshots/PC/NewVolumeWizard.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _New volume wizard_
    </TD>
  </TR>
</TABLE>

When prompted to choose between creating a file or partition based Box, choose "File" - partition based volumes are intended for advanced users.

If you are unfamiliar with disk encryption you may not understand all of the options. If you are in this position, just accept the default values, which will give you a volume that will be secure enough. 

DoxBox is a highly flexible system that is used by both novice and advanced users alike; many of the options that the new box wizard provides you with are intended for more advanced users who understand the options provided (e.g. storing a volume's CDB separately to the volume file it relates to), and how they operate.

 
<SPAN class="tip">
  You may want to create and use multiple Boxes; one to store work related files, one for personal files, etc    
</SPAN>

 
<SPAN class="security_tip">
Do not simply copy an existing volume file to create a new one - even if you change the keyphrase on the "new" volume. If you do this, both volumes will have the same encrypted master key, which reduces the amount of security offered.  
</SPAN>


<SPAN CLASS="security_tip">
By default, to increase security, as part of creating a volume it is overwritten with secure pseudo-random data. However, this process can take some time and may be skipped if required. This option is on the 'chaff' tab of the advanced dialog. See section on [plausible deniability](plausible_deniability.html) for further details  
</SPAN>

After clicking 'OK' on the Box creation dialog, you will see a Windows 'format drive' dialog. Use this to format the virtual drive.

If you wish to use your volume on both PCs and Linux,it should be formatted as FAT, FAT32 or NTFS.
If you wish to use it with DoxBox Explorer, it should be formatted as either FAT or FAT32.

* * * 
<A NAME="level_3_heading_4">
### Opening volumes
</A>

Once you have created a volume, it must be "opened" in order for it to appear as a virtual drive on your computer.

Click the 'Open' toolbar button to open a file based Box, or "File | Mount partition..." to open a partition based volume.

You will then be able to choose which box you want to open; do so, and click "OK" to display the keyphrase entry dialog.

 
<SPAN class="tip">
   You can also open boxes by dragging and dropping the encrypted file onto the main DoxBox window.     
</SPAN>


<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/images/screenshots/PC/MountBasic.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _Keyphrase entry dialog_
    </TD>
  </TR>
</TABLE>

Enter your Box's keyphrase, and click "OK". If the correct keyphrase has been entered, the volume will be opened and shown in the main DoxBox window.
 
<SPAN class="tip"> To reduce the time taken DoxBox spends opening boxes, see the FAQ [How can I speed DoxBox up when mounting my volumes?](FAQ.html#bm)  </SPAN>

Once opened, a virtual drive can be used in the same way _as any other drive_ (e.g. they will appear in Windows Explorer and in Open/Save dialogs shown by applications); transparently encrypting and decrypting your files as and when needed.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/images/screenshots/PC/ExplorerWithMounted.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _Explorer showing mounted volume_
    </TD>
  </TR>
</TABLE>

 
<SPAN class="tip">You can have more than one box open at the same time</SPAN>

* * * 
<A NAME="level_3_heading_5">
### Dismounting volumes
</A>

Once you have finished using your secured drive, it should be "locked". This will remove the virtual drive, and wipe any sensitive information DoxBox has stored in the computers memory.

Select which boxes shown in the main DoxBox window you wish to 'lock' and click the 'lock' toolbar button; or use the context menu shown by right-clicking on one of the boxes shown.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/images/screenshots/PC/MainContextMenu.png">
    </TD>
  </TR>
  <TR>
    <TD>       <FONT SIZE=-1>(PC version)</FONT>
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _Main window context menu_
    </TD>
  </TR>
</TABLE>

To quickly dismount all mounted volumes, use the "Dismount all" menu-item.

* * * 
<A NAME="level_3_heading_6">
### Changing a volume's keyphrase
</A>

To change a volume's keyphrase (or a keyfile's keyphrase), select the "Tools | Change volume/keyfile keyphrase/details..." menu-item to display the "change keyphrase wizard", which will guide you through the process in a series of simple steps.

Note that volumes must be _locked_ first before they can be modified in this way.

 
<SPAN class="tip">
You may also change certain Box/keyfile details via this wizard; for example, the default drive letter which the volume will normally be mounted as. Advanced users may also change more technical details, such as the length of salt used in encrypting the volume's CDB/keyfile    
</SPAN>

 
<SPAN CLASS="tech_note">
In common with most disk encryption systems, DoxBox uses an "encrypted master key" system to secure volumes. Every DoxBox has its own "master encryption key" which is generated when the volume is created. This master key is used to carry out the actual encryption/decryption process used to secure data stored within the volume. A volume's master encryption key is, in turn, encrypted with the (PBKDF2 processed) user's keyphrase. So DoxBox doesn't need to decrypt and re-encrypt the entire volume to change the user's keyphrase - only the encrypted master encryption key. This makes changing a volume/keyfile's keyphrase quicker and safer than a complete volume re-encryption.     
</SPAN>



