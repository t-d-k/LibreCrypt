

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Appendix G: Uninstalling</TITLE>

<LINK href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<LINK rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

      
            
## Appendix G: Uninstalling

To uninstall LibreCrypt, please carry out the steps detailed in either of the sections below:

* * * 
<A NAME="level_3_heading_1">
### Automatic Uninstall
</A>
If installed via the installation wizard, LibreCrypt may be uninstalled by either:


1. Using the "Add and Remove Programs" control panel applet.
1. Running "uninstall.exe", found in the directory LibreCrypt was installed in


* * * 
<A NAME="level_3_heading_2">
### Manual Uninstall
</A>

<OL>

1. Launch "LibreCrypt.exe".
1. Unmount **all** mounted volumes.

1. Select "File | Drivers..."

1. Select each of the drivers you have installed, and click "Uninstall". Repeat this until all drivers have been uninstalled. If you encounter errors in this step, don't worry; just continue uninstalling your other remaining drivers
1. Exit LibreCrypt.

1. Reboot your computer
1. You shouldn't need to, but if you encountered any errors while uninstalling the drivers:
	1. Run "regedit.exe", and remove all registry keys under: `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services` that have "FreeOTFE" in their name.	
	2. Delete all files beginning "FreeOTFE" from your `<windows>\system32\drivers` directory (you may need to reboot your computer again before you can do this)
	3. If you deleted any registry entries or files, reboot your computer again.


1. Finally, delete "LibreCrypt.exe", and any shortcuts you may have created.
</OL>



