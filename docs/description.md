

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content=
"LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Introduction</TITLE>

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
            
## Introduction

LibreCrypt: An Open-Source "on-the-fly" transparent disk encryption program for MS Windows 2000/Vista/Windows 7 PCs (both 32 and 64 bit)

This software can create "virtual disks" on your computer - anything written to these disks is automatically encrypted before being stored on your computer's hard drive.

* * * 
<A NAME="level_3_heading_1">
### Features
</A>

* *Source code freely available*
* Easy to use
* Linux compatibility (Cryptoloop "losetup", dm-crypt and LUKS supported)
* Powerful: Supports more hash and encryption algorithms than any other transparent encryption system
* Available in English, Spanish, German, Italian, French, Czech, Japanese, Croatian, Greek and Russian - with support for other language translations
* Supports the modern recommended cypher algorithms such as AES and XTS  
* Operates under MS Windows 2000/Vista/Windows 7 platforms
* "Portable mode" included; LibreCrypt doesn't need to be installed before it can be used - making it ideal for carrying your data securely on USB drives!
* "Hidden" containers may be concealed within other containers, providing "plausible deniability"

### Technical details
* Hash algorithms include: MD5, SHA-512, RIPEMD-320, Tiger and _many_ more
* Cyphers include AES (256 bit), Twofish (256 bit), Blowfish (448 bit), Serpent (256 bit) and _many_ more
* Cypher modes supported include XTS, LRW and CBC (including XTS-AES-128 and XTS-AES-256)
* Security tokens/smartcards supported for extra (optional) security
* containers have no "signature" to allow them to be identified as such
* Encrypted containers can be either file or partition based.
* Modular design allowing 3rd party drivers to be created, incorporating new hash/cypher algorithms
* Supports password salting (up to 512 bits), reducing the risks presented by dictionary attacks.
* 'keyfiles' allow multiple users to share a container without sharing keyphrases and support key backup.
* Container file timestamps and attributes are reset after closing, increasing "plausible deniability"
* containers up to 8,388,608 TB 

Screenshots of LibreCrypt are [here](http://LibreCrypt.tdksoft.co.uk/screenshots_pc_main.html)

Cyphers included:

<TABLE style="text-align: left;">

  <TBODY> 
<TR> <TH>Cypher </TH>  <TH>Key length (in bits)</TH> <TH>Block Length (in bits) </TH> <TH>Modes </TH> <TH>Source Library </TH> <TH>Comments </TH> </TR> 

<TR> <TD>AES</TD>        <TD>128</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>XTS version aka XTS-AES-128</TD> </TR> 
<TR> <TD>AES</TD>        <TD>192</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD><TD></TD> </TR> 
<TR> <TD>AES</TD>        <TD>256</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>XTS version aka XTS-AES-256</TD> </TR> 
<TR> <TD>AES</TD>        <TD>128</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD>XTS version aka XTS-AES-128</TD> </TR> 
<TR> <TD>AES</TD>        <TD>192</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>AES</TD>        <TD>256</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD>XTS version aka XTS-AES-256 </TD> </TR> 
<TR> <TD>Blowfish</TD>  <TD>128</TD>     <TD>64</TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Blowfish</TD>  <TD>160</TD>     <TD>64 </TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Blowfish</TD>  <TD>192</TD>     <TD>64 </TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Blowfish</TD>  <TD>256</TD>     <TD>64</TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Blowfish</TD>  <TD>448</TD>     <TD>64 </TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>CAST5</TD>     <TD>128</TD>     <TD>64</TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD>aka CAST-128</TD> </TR> 
<TR> <TD>CAST6</TD>  <TD>128</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>aka CAST-256</TD> </TR> 
<TR> <TD>CAST6</TD>  <TD>160</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>aka CAST-256</TD> </TR> 
<TR> <TD>CAST6</TD>  <TD>192</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>aka CAST-256</TD> </TR> 
<TR> <TD>CAST6</TD>  <TD>224</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>aka CAST-256</TD> </TR> 
<TR> <TD>CAST6</TD>  <TD>256</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD>aka CAST-256</TD> </TR> 
<TR> <TD>DES</TD>     <TD>64</TD>     <TD>64</TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>3DES</TD>     <TD>192</TD>     <TD>64</TD>     <TD>CBC              </TD>     <TD>LibTomCrypt </TD><TD>Standard encrypt, decrypt, encrypt</TD> </TR> 
<TR> <TD>MARS</TD>  <TD>128</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>MARS</TD>  <TD>192</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>MARS</TD>  <TD>256</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Null</TD>     <TD>0</TD>     <TD>(variable)</TD>     <TD>n/a</TD>     <TD>n/a</TD>     <TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>128</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>192</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>256</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>128</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>192</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>256</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>RC-6</TD>     <TD>1024</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS       </TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Serpent</TD>  <TD>128</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Serpent</TD>  <TD>192</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Serpent</TD>     <TD>256</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Twofish</TD>  <TD>128</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Twofish</TD>  <TD>192</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Twofish</TD>  <TD>256</TD>     <TD>128</TD>     <TD>CBC/XTS       </TD>     <TD>Dr. Brian R. Gladman</TD>     <TD></TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>128</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Hi/fn and Counterpane Systems </TD><TD>x86 systems _only_</TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>192</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Hi/fn and Counterpane Systems</TD><TD>x86 systems _only_</TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>256</TD>     <TD>128</TD>     <TD>CBC       </TD>     <TD>Hi/fn and Counterpane Systems</TD><TD>x86 systems _only_</TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>128</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS</TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>192</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS</TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>Twofish</TD>     <TD>256</TD>     <TD>128</TD>     <TD>CBC/LRW/XTS</TD>     <TD>LibTomCrypt </TD><TD></TD> </TR> 
<TR> <TD>XOR</TD>     <TD>(variable)</TD>     <TD>(variable)</TD>     <TD>n/a</TD>     <TD>n/a</TD>     <TD></TD> </TR> 
</TBODY> </TABLE> 
  
  Hash algorithms included:
<TABLE style="text-align: left;">

  <TBODY> 
<TR> <TH>Hash </TH> <TH>Hash Length (in bits) </TH> <TH>Block Length (in bits) </TH> <TH>Source Library </TH> </TR> 

<TR> <TD>MD2</TD>     <TD>128</TD>     <TD>128</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>MD4</TD>     <TD>128</TD>     <TD>512</TD>     <TD>LibTomCrypt </TD> </TR> 
<TR> <TD>MD5</TD>     <TD>128</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>Null</TD>     <TD>(variable)</TD>     <TD>(variable)</TD>     <TD>n/a</TD> </TR> 
<TR> <TD>RIPEMD-128</TD>     <TD>128</TD>     <TD>512</TD>     <TD>LibTomCrypt </TD> </TR> 
<TR> <TD>RIPEMD-160</TD>     <TD>160</TD>     <TD>512</TD>     <TD>LibTomCrypt </TD> </TR> 
<TR> <TD>RIPEMD-160 (Linux; Twice, with A)</TD>     <TD>320</TD>     <TD>512</TD>     <TD>LibTomCrypt </TD> </TR> 
<TR> <TD>RIPEMD-256</TD>     <TD>256</TD>     <TD>512</TD>     <TD>LibTomCrypt</TD> </TR> 
<TR> <TD>RIPEMD-320</TD>     <TD>320</TD>     <TD>512</TD>     <TD>LibTomCrypt</TD> </TR> 
<TR> <TD>SHA-1</TD>     <TD>160</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>SHA-224</TD>     <TD>224</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>SHA-256</TD>     <TD>256</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>SHA-384</TD>     <TD>384</TD>     <TD>1024</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>SHA-512</TD>     <TD>512</TD>     <TD>1024</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>Tiger</TD>     <TD>192</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR> 
<TR> <TD>Whirlpool</TD>     <TD>512</TD>     <TD>512</TD>     <TD>LibTomCrypt        </TD> </TR>
</TBODY>
</TABLE>



