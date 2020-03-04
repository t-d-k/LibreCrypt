
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Getting Started Guide</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.tdksoft.co.uk/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.tdksoft.co.uk/): Open-Source disk encryption for Windows_
</SPAN>
***
<SPAN class="tip">
The latest version of this document can be found at the [LibreCrypt project site](https://github.com/t-d-k/librecrypt)
</SPAN>       
            
## Getting Started Guide

Pretty much everything in LibreCrypt should be fairly self-explanatory. Most items have 'tool-tips', so if you hover the mouse over them you will see a description.

  * [Installation](#level_3_heading_1)
  * [Portable mode](#port)
  * [Using LibreCrypt](#level_3_heading_2)
  * [Creating a new container](#level_3_heading_3)
  * [Opening containers](#level_3_heading_4)
  * [Closing containers](#level_3_heading_5)
  * [Changing a container's keyphrase](#level_3_heading_6)


* * * 
<A NAME="level_3_heading_1">
### Installation
</A>

Before LibreCrypt can be used, it must first be installed. You need to have administrator privileges to install, after it will prompt you to reboot. 
After rebooting the words 'test mode' appear on the Desktop.
After it has been installed, run it the first time as administrator; this will install the drivers. 

After that, it may be run as a non-administrator.

Please see the section on [Installation and Upgrading](installation_and_upgrading__PC.md) for more instructions.

<A NAME="port">
### Portable mode
</A>

Before LibreCrypt Portable can be used, it must be unzipped from the portable zip file.
It needs to be run as administrator, in order to install the drivers in 'portable mode'. 

Please see the section on [Installation and Upgrading](installation_and_upgrading__PC.md) for more instructions.

* * * 
<A NAME="level_3_heading_2">
### Using LibreCrypt
</A>

Once LibreCrypt is installed (or started in portable mode), securing your data is simple:

1. Create a container to store your files in
1. 'Open' the Container created.
1. It will appear as a new drive on your computer.
		
	_Anything_ and _everything_ stored on this drive (documents, pictures, videos, software - whatever you like) will be automatically encrypted and stored within the Container you created, at the time that it's written to the drive.
	
	The encryption process is totally transparent, and is carried out 'on-the-fly' as data is written to the drive. At no time is any unencrypted data stored on the disc. Similarly, decryption is carried out transparently when data is read from it.	
	
	You can drag files to and from this drive, open and save files from applications to it, run applications directly from it - in fact, _everything_ that you can do with a normal drive, you can do with a container.

1. To secure your data, simply 'lock' the Container. At that point, the drive disappears and the encryption/decryption key is overwritten in memory - making your data totally inaccessible until the keyphrase is supplied, and the container is opened again.

The following sections give more detailed instructions on how do carry out each of these steps.

* * * 
<A NAME="level_3_heading_3">
### Creating a new Container
</A>

In order to use LibreCrypt, you must first create a "container" to contain your new drive.

<SPAN CLASS="tech_note">
This consists of using LibreCrypt to create a large file (or a partition) on your computer's hard drive filled with random data.
This containers will hold an encrypted "disk image" of your container, and is where LibreCrypt will store all data written to your virtual drive. 
</SPAN>

<SPAN CLASS="tech_note">
'Containers' are commonly called 'volumes' in other encryption programs
</SPAN>

This file (or partition) can then be "opened", at which point a virtual drive will appear on your computer - anything stored on one of these virtual drives will be automatically encrypted before being written to the file.

To create a new container, click the 'New' button on the toolbar to show the "New Container wizard", which will guide you through the process in a series of simple steps.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/images/screenshots/PC/NewVolumeWizard.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _New container wizard_
    </TD>
  </TR>
</TABLE>

When prompted to choose between creating a file or partition based container, if you are unsure, choose "File". Partition based containers are stored directly on your hard disc and are harder to delete.

The default values in the dialog are ones that will give you the most secure container; chane them only if you understand the change you are making.

LibreCrypt is a highly flexible system that is used by both novice and advanced users alike; many of the options that the new container wizard provides you with are intended for more advanced users who understand the options provided (e.g. storing a containers's header separately to the container it relates to), and how they operate. These options are on 'advanced' tabs that can be seen by clicking 'next' after the finish button is enabled.

 
<SPAN class="tip">
  You may want to create and use multiple Containers; one to store work related files, one for personal files, etc    
</SPAN>

 
<SPAN class="security_tip">
Do not simply copy an existing containers file to create a new one - even if you change the keyphrase on the "new" containers. If you do this, both containers will have the same encrypted master key, which reduces the amount of security offered.  
</SPAN>


<SPAN CLASS="security_tip">
By default, to increase security, as part of creating a containers it is overwritten with secure pseudo-random data. However, this process can take some time and may be skipped if required. This option is on the 'chaff' tab of the advanced dialog. See section on [plausible deniability](plausible_deniability.md) for further details  
</SPAN>

After clicking 'OK' on the container creation dialog, you may see a Windows 'format drive' dialog. Use this to format the virtual drive.

If you wish to use your containers on both PCs and Linux,it should be formatted as FAT, FAT32 or NTFS.
If you wish to use it with LibreCrypt Explorer, it should be formatted as either FAT or FAT32.

* * * 
<A NAME="level_3_heading_4">
### Opening containers
</A>

Once you have created a containers, it must be "opened" in order for it to appear as a virtual drive on your computer.

Click the 'Open' toolbar button to open a file based container, or "File | Open partition..." to open a partition based container.

You will then be able to choose which container you want to open; do so, and click "OK" to display the keyphrase entry dialog.

 
<SPAN class="tip">
   You can also open containers by dragging and dropping the encrypted file onto the main LibreCrypt window.     
</SPAN>


<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/images/screenshots/PC/MountBasic.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _Keyphrase entry dialog_
    </TD>
  </TR>
</TABLE>

Enter your Container's keyphrase, and click "OK". If the correct keyphrase has been entered, the containers will be opened and shown in the main LibreCrypt window.
 
<SPAN class="tip"> To reduce the time taken LibreCrypt spends opening containers, see the FAQ [How can I speed LibreCrypt up when opening my containers?](FAQ.md#bm)  </SPAN>

Once opened, a virtual drive can be used in the same way _as any other drive_ (e.g. they will appear in Windows Explorer and in Open/Save dialogs shown by applications); transparently encrypting and decrypting your files as and when needed.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/images/screenshots/PC/ExplorerWithMounted.png">
    </TD>
  </TR>
  <TR>
    <TD COLSPAN="2">
      _Explorer showing mounted volume_
    </TD>
  </TR>
</TABLE>

 
<SPAN class="tip">You can have more than one container open at the same time</SPAN>

* * * 
<A NAME="level_3_heading_5">
### Closeing containers
</A>

Once you have finished using your secured drive, it should be "locked". This will remove the virtual drive, and wipe any sensitive information LibreCrypt has stored in the computers memory.

Select which containers shown in the main LibreCrypt window you wish to 'lock' and click the 'lock' toolbar button; or use the context menu shown by right-clicking on one of the containers shown.

<TABLE WIDTH="100%">
  <TR>
    <TD WIDTH="50%" class="screenshot_img" >
      <img BORDER="0" src="https://raw.githubusercontent.com/t-d-k/LibreCrypt/master/docs/images/screenshots/PC/MainContextMenu.png">
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

To quickly close all open containers, use the "Close all" menu-item.

* * * 
<A NAME="level_3_heading_6">
### Changing a containers's keyphrase
</A>

To change a containers's keyphrase (or a keyfile's keyphrase), select the "Tools | Change containers/keyfile keyphrase/details..." menu-item to display the "change keyphrase wizard", which will guide you through the process in a series of simple steps.

Note that containers must be _locked_ first before they can be modified in this way.

 
<SPAN class="tip">
You may also change certain Container/keyfile details via this wizard; for example, the default drive letter which the containers will normally be opened as. Advanced users may also change more technical details, such as the length of salt used in encrypting the containers's CDB/keyfile    
</SPAN>

 
<SPAN CLASS="tech_note">
In common with most disk encryption systems, LibreCrypt uses an "encrypted master key" system to secure containers. Every container has its own "master encryption key" which is generated when the containers is created. This master key is used to carry out the actual encryption/decryption process used to secure data stored within the containers. A containers's master encryption key is, in turn, encrypted with the (PBKDF2 processed) user's keyphrase. So LibreCrypt doesn't need to decrypt and re-encrypt the entire containers to change the user's keyphrase - only the encrypted master encryption key. This makes changing a containers/keyfile's keyphrase quicker and safer than a complete containers re-encryption.     
</SPAN>



