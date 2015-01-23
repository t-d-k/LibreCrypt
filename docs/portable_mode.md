

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Portable Mode</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            

## Portable Mode

Portable mode allows you to use DoxBox without first installing it on your computers hard drive.

To start portable mode, simply:


1. Unzip the DoxBox_portable.zip file to wherever you want 
2. When logged on as a user with administrator rights, double-click on "DoxBox.exe" (the file with the black box icon).
3. Click on the menu Tools->Allow Test-signed Drivers.
4. Reboot.
5. Restart DoxBox and click on the "world" icon on the toolbar

To stop portable mode, select the "Tools | Portable mode drivers" option, or select the "world" icon on the toolbar again.

No files are copied to your hard drive when starting/stopping portable mode.

 
<SPAN class="tip">
Administrator rights are required in order to start and stop portable mode. You _can_ still use DoxBox on a computer which you do not have Administrator rights on, but will need to ask an Administrator to install it for you first; see the [installation section](installation_and_upgrading__PC.html) for further details. Alternatively, use [DoxBox Explorer](http://DoxBox.eu/) instead - which doesn't need drivers at all    
</SPAN>

 

<SPAN class="tip">
You can check to see which drivers are operating in portable mode by clicking "File	->	Drivers..." - those drivers currently running in portable mode are with a small "world" icon to the left of their names.     
    </SPAN>
    

IMPORTANT: Stopping portable mode when any DoxBoxes are open in portable mode can cause unexpected results - it is recommended that you lock all Boxes before stopping portable mode.

For safety reasons, drivers installed in portable mode are installed such that if you reboot your system without first stopping portable mode via the DoxBox interface, those drivers will not automatically start up when your computer comes back up. If they were automatically restarted, and your portable drivers were stored on removable media which was taken out while rebooting, possible problems could occur during your boot sequence.

Because of this, if you exit DoxBox without stopping all portable mode drivers, and then reboot your computer, you will find that the next time you launch DoxBox, the "Portable mode drivers" menu-item will appear checked as DoxBox detects your portable mode drivers. If you select this menu-item, the previous portable mode drivers should be stopped and uninstalled as per normal; clearing this menu-item, ready for you to restart portable mode.

* * * 
<A NAME="level_3_heading_1">
### Portable Mode and Removable Media
</A>

You may wish to run DoxBox from removable media such as a USB flash drive. This way, your copy of DoxBox can easily be transported and used on different computers.

To do this, the following files should be copied onto your flash drive:

1. The main DoxBox GUI: "DoxBox.exe"
1. The main FreeOTFE driver: "FreeOTFE.sys"
1. At least one hash driver: "FreeOTFEHash_XYZ_.sys"
1. At least one cypher driver: "FreeOTFECypher_XYZ_.sys"

There is a menu item - "Copy to portable Disc", that does this automatically.

Please note that if you attempt to start portable mode on a computer which already has one or more of the DoxBox drivers installed, DoxBox will display an error stating that it cannot start the relevant driver in portable mode - which is hardly surprising if it's already there!



