

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>TODO List</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

                  
## TODO List
### high priority	
	* add password entropy checking
	* can run bcedit from app? - investigate
### medium priority
	* Update hash and cypher libraries
	* fix crashes in some alternate versions of drivers
	* integrate securetrayutils - specifically legacy volume support.	
### low priority	
	* GPG integration; support for using GPG to generate random data and Linux AES multikey support (this only really need the GPG interface finishing off) 
	* Support for Linux key iterations (-C option)
	* Other MAC algorithms (e.g. PMAC, OMAC)
	* update Command line utilities
	* update docs to reflect name change and feature changes
	* refactor forms out of components into common library	
	* add ability to create LUKS conainers. -> make this default 
	* hidden vols on linux with salt? investigate
	* improve linux scripts
	* write quick usage guide
	* (Constructive) suggestions welcome.




