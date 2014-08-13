

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Portable Mode</TITLE>

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

      
            

## Portable Mode

 
<TABLE class="note">
  <TR>
    <TD> <img src="./images/note_VistaPCIcon.png" alt="PC version only" >
    </TD>
    <TD> This section applies to the PC version of FreeOTFE _only_      </TD>
  </TR>
</TABLE>

Portable mode allows you to use FreeOTFE without first installing it on your computers hard drive.

To start portable mode, simply:

1. Uncompress the FreeOTFE release (.ZIP file version) into a suitable location
1. Run "DoxBox.exe" by doubleclicking on it.
1. Select the "Tools | Portable mode drivers" option, or click on the "world" icon on the toolbar

To stop portable mode, select the "Tools | Portable mode drivers" option, or select the "world" icon on the toolbar again.

No files are copied to your hard drive when starting/stopping portable mode.

 
<SPAN class="tip">
Administrator rights are required in order to start and stop portable mode. You _can_ still use FreeOTFE on a computer which you do not have Administrator rights on, but will need to ask an Administrator to install it for you first; see the [installation section](installation_and_upgrading__PC.htm) for further details. Alternatively, use [FreeOTFE Explorer](http://DoxBox.squte.com/) instead - which doesn't need drivers _at all!_    
</SPAN>

 

<SPAN class="tip">
You can check to see which drivers are operating in portable mode by selecting "File	|	Drivers..." - those drivers currently running in portable mode are with a small "world" icon to the left of their names.     
    </SPAN>
    

IMPORTANT: Stopping portable mode when any mounted volumes are still using one or more FreeOTFE drivers operating in portable mode can cause unexpected results - for this reason it is recommended that you dismount all volumes before stopping portable mode.

For safety reasons, drivers installed in portable mode are installed such that if you reboot your system without first stopping portable mode via the FreeOTFE interface, those drivers will not automatically start up when your computer comes back up. If they were automatically restarted, and your portable drivers were stored on removable media which was taken out while rebooting, possible problems could occur during your boot sequence.

Because of this, if you exit FreeOTFE without stopping all portable mode drivers, and then reboot your computer, you will find that the next time you launch FreeOTFE, the "Portable mode drivers" menuitem will appear checked as FreeOTFE detects your portable mode drivers. If you select this menuitem, the previous portable mode drivers should be stopped and uninstalled as per normal; clearing this menuitem, ready for you to restart portable mode.

* * * 
<A NAME="level_3_heading_1">
### Portable Mode and Removable Media
</A>

You may wish to run FreeOTFE in portable mode from removable media such as a USB flash drive, or similar. In this way, your copy of FreeOTFE can easily be transported and used on different computers.

In order to do so, the following files should be copied onto your flash drive:

<OL>
* The main FreeOTFE GUI: "DoxBox.exe"
* The main FreeOTFE driver: "FreeOTFE.sys"
* At least one hash driver: "FreeOTFEHash_XYZ_.sys"
* At least one cypher driver: "FreeOTFECypher_XYZ_.sys"
</OL>

Please note that if you attempt to start portable mode on a computer which already has one or more of the FreeOTFE drivers installed, FreeOTFE will display an error stating that it cannot start the relevant driver in portable mode - which is hardly surprising if it's already there!



