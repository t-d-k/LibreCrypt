

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Appendix G: Uninstalling</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:sdean12@sdean12.org">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://DoxBox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_Open-Source disk encryption for Windows_
</SPAN>

      
            
## Appendix G: Uninstalling

To uninstall FreeOTFE, please carry out the steps detailed in either of the sections below:

* * * 
<A NAME="level_3_heading_1">
### Automatic Uninstall
</A>
If installed via the installation wizard, FreeOTFE may be uninstalled by either:

<OL>
  * Using the "Add and Remove Programs" control panel applet.

  * Running "uninstall.exe", found in the directory FreeOTFE was installed in

</OL>

* * * 
<A NAME="level_3_heading_2">
### Manual Uninstall
</A>

<OL>

  * Launch "DoxBox.exe".
* Unmount **all** mounted volumes.

  * Select "File | Drivers..."

  * Select each of the
drivers you have installed, and click "Uninstall". Repeat this until
all drivers have been uninstalled. If you encounter errors in this
step, don't worry; just continue uninstalling your other remaining
drivers
* Exit FreeOTFE.

  * Reboot your computer
* You shouldn't need to, but if you encountered any errors while uninstalling the drivers:
<OL>
  * Run
"regedit.exe", and remove all registry keys under:
HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services
that have "FreeOTFE" in their name.

  * Delete all files beginning "FreeOTFE" from your
&lt;windows&gt;\system32\drivers directory (you may need to reboot your
computer again before you can do this)
  * If you deleted any registry entries or files, reboot your computer again.

</OL>
  * Finally, delete "DoxBox.exe", and any shortcuts you may have created.
</OL>



