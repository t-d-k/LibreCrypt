

<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Appendix E: PKCS#11 Driver Libraries</TITLE>

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

      
            

## Appendix E: PKCS#11 Driver Libraries

If you have a token which supports the PKCS#11 standard, as most do, your token can be used by FreeOTFE. Below is a list of well known smartcards/tokens which do support this standard, and the suggested library filename to use.

Please note that:

<OL>
* Token manufacturers may change their driver filenames without notice; check with your supplier if the information listed below doesn't work.
* This list is *_not exhaustive_* - many more tokens are supported than are listed here.
</OL>

If you are using a token which isn't mentioned on the list below, please check with your token supplier as to what to enter, and [get in touch](contact_details.htm) to have it added to the list.

<TABLE BORDER=1>
<TR>
<TH>Manufacturer</TH>
<TH>Device</TH>
<TH>Library</TH>
<TH>web site</TH>
<TH>Notes</TH>
</TR>
<TR>
<TD ROWSPAN="1">ACS</TD>
<TD >ACOS5 smartcards</TD>
<TD >acospkcs11.dll</TD>
<TD >	</TD>
<TD >For use with ACOS5 smartcards. ACOS5 smartcards unable to create data objects, and contain no mechanisms for creating secret keys (except public/private keypairs)</TD>
</TR>
<TR>
<TD ROWSPAN="1">AET</TD>
<TD >Rainbow iKey 3000 series and G&D StarCos 2.3 SPK cards</TD>
<TD >aetpkss1.dll</TD>
<TD >[(WWW)](http://www.aeteurope.nl/ download from http://www.gi-de.com/portal/page?_pageid=42,124645&_dad=portal&_schema=PORTAL)</TD>
<TD >aka StarSign Middleware. Also installs PKCS#11 drivers aetpksse.dll (Entrust PKCS#11 lib) and aetpkssw.dll (PKCS #11 library wrapper that detects the need for automatic login on aetpkss1.dll)</TD>
</TR>
<TR>
<TD ROWSPAN="2">Aladdin</TD>
<TD >eToken PRO</TD>
<TD >etpkcs11.dll</TD>
<TD >[(WWW)](http://www.aladdin.com/)</TD>
<TD >Confirmed working with FreeOTFE</TD>
</TR>
<TR>
<TD >eToken R2</TD>
<TD >etpkcs11.dll</TD>
<TD >[(WWW)](http://www.aladdin.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Algorithmic Research</TD>
<TD >MiniKey</TD>
<TD >sadaptor.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Aloaha</TD>
<TD >Smart Card Connector</TD>
<TD >aloaha_pkcs11.dll</TD>
<TD >[(WWW)](http://www.aloaha.com/wi-software-en/aloaha-cryptographic-service-provider.php)</TD>
<TD >Not fully tested, but from initial checks, this library appears _badly broken_ - it reports tokens present when no card reader is installed. When creating data objects on the phantom token, it reports that the objects were _successfully created_, but doesn't actually _do anything_! Possible that it works correctly if both hardware and token present?</TD>
</TR>
<TR>
<TD ROWSPAN="1">A-Sign</TD>
<TD >A-Sign premium cards</TD>
<TD >psepkcs11.dll</TD>
<TD >	</TD>
<TD >Reputedly v1.0 of this library is incomplete and has many bugs??</TD>
</TR>
<TR>
<TD ROWSPAN="1">Athena</TD>
<TD >Athena Smartcard System ASE Card</TD>
<TD >asepkcs.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">A-Trust</TD>
<TD >a-sign</TD>
<TD >asignp11.dll</TD>
<TD >[(WWW)](http://www.a-trust.at/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Belgian Government</TD>
<TD >Belgian Electronic Identity (eID) Card </TD>
<TD >Belgium Identity Card PKCS11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="2">Chrysalis</TD>
<TD >	</TD>
<TD >cryst32.dll</TD>
<TD >[(WWW)](www.chrysalis-its.com)</TD>
<TD >web site no longer exists</TD>
</TR>
<TR>
<TD >LUNA</TD>
<TD >cryst201.dll</TD>
<TD >[(WWW)](www.chrysalis-its.com)</TD>
<TD >web site no longer exists</TD>
</TR>
<TR>
<TD ROWSPAN="1">Dallas Semiconductors</TD>
<TD >iButton</TD>
<TD >dspkcs.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="2">Eracom </TD>
<TD >(hardware)</TD>
<TD >cryptoki.dll</TD>
<TD >	</TD>
<TD >Full path to library may be required, e.g:
C:\ Program Files\ ERACOM\ Cprov SDK\ bin\ csa\ cryptoki.dll
On 1st December2005 Eracom Technologies AG was acquired by SafeNet </TD>
</TR>
<TR>
<TD >(software emulation)</TD>
<TD >cryptoki.dll</TD>
<TD >	</TD>
<TD >Full path to library may be required, e.g.:
C:\ Program Files\ ERACOM\ Cprov SDK\ bin\ sw\ cryptoki.dll
On 1st December2005 Eracom Technologies AG was acquired by SafeNet </TD>
</TR>
<TR>
<TD ROWSPAN="1">Estonian Government</TD>
<TD >Estonian Electronic Identity (eID) Card </TD>
<TD >opensc-pkcs11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Eutron</TD>
<TD >Crypto Identity</TD>
<TD >sadaptor.dll</TD>
<TD >[(WWW)](http://www.eutron.com)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="5">Feitain technologys Co.,Ltd</TD>
<TD >ePass 1000</TD>
<TD >EP1PK111.DLL</TD>
<TD >[(WWW)](http://www.epass.nl/ and http://www.ftsafe.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >ePass 2000</TD>
<TD >ep2pk11.dll</TD>
<TD >[(WWW)](http://www.epass.nl/ and http://www.ftsafe.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >ePass 2000_FT11</TD>
<TD >ngp11v211.dll</TD>
<TD >[(WWW)](http://www.epass.nl/ and http://www.esoftkey.com/)</TD>
<TD >Confirmed working with FreeOTFE</TD>
</TR>
<TR>
<TD >ePass 3000</TD>
<TD >ngp11v211.dll</TD>
<TD >[(WWW)](http://www.ftsafe.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >ePass 3003</TD>
<TD >ShuttleCsp11_3003.dll</TD>
<TD >[(WWW)](http://www.epass.nl/ and http://www.esoftkey.com/)</TD>
<TD >Confirmed working with FreeOTFE</TD>
</TR>
<TR>
<TD ROWSPAN="2">Gemplus</TD>
<TD >	</TD>
<TD >gclib.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >	</TD>
<TD >pk2priv.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">GemPlus/GemSoft</TD>
<TD >GemPlus/GemSoft Smartcard</TD>
<TD >w32pk2ig.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="2">GemSafe</TD>
<TD >	</TD>
<TD >gclib.dll</TD>
<TD >	</TD>
<TD >GemSafe new version</TD>
</TR>
<TR>
<TD >	</TD>
<TD >pk2priv.dll</TD>
<TD >	</TD>
<TD >GemSafe old version</TD>
</TR>
<TR>
<TD ROWSPAN="6">IBM</TD>
<TD >IBM 4758</TD>
<TD >cryptoki.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >IBM Digital Signature for the Internet (DSI) for MFC cards </TD>
<TD >CccSigIT.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >IBM Embededded Security Subsystem</TD>
<TD >csspkcs11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >IBM Netfinity PSG Chip1</TD>
<TD >ibmpkcss.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >IBM SecureWay Smartcard</TD>
<TD >w32pk2ig.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >	</TD>
<TD >cryptoki.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">ID2</TD>
<TD >	</TD>
<TD >id2cbox.dll</TD>
<TD >	</TD>
<TD >Full path to library may be required, e.g.:
C:\ Data\ Development\ SmartCardIntegration\ PKCS11wrapper\ JavaToPKCS11\ demo\ test\ old\ id2cbox.dll</TD>
</TR>
<TR>
<TD ROWSPAN="1">Mozilla/Netscape</TD>
<TD >Mozilla or Netscape crypto module</TD>
<TD >softokn3.dll</TD>
<TD >	</TD>
<TD >Cannot be used with FreeOTFE; requires additional parameters to initailize</TD>
</TR>
<TR>
<TD ROWSPAN="2">nCipher</TD>
<TD >nFast</TD>
<TD >cknfast.dll</TD>
<TD >[(WWW)](http://www.ncipher.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >nShield</TD>
<TD >cknfast.dll</TD>
<TD >[(WWW)](http://www.ncipher.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Nexus</TD>
<TD >	</TD>
<TD >nxpkcs11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">OpenSC</TD>
<TD >(multiple)</TD>
<TD >opensc-pkcs11.dll</TD>
<TD >[(WWW)](http://www.opensc-project.org/scb/)</TD>
<TD >Free, open source library</TD>
</TR>
<TR>
<TD ROWSPAN="1">Orga Micardo</TD>
<TD >	</TD>
<TD >micardoPKCS11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="8">Rainbow</TD>
<TD >CryptoSwift Accelerator</TD>
<TD >Cryptoki22.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >CryptoSwift HSM</TD>
<TD >iveacryptoki.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Ikey 1000</TD>
<TD >cryptoki22.dll</TD>
<TD >	</TD>
<TD >From wiki.cacert.org: for USB use Datakey driver</TD>
</TR>
<TR>
<TD >iKey 1000/1032</TD>
<TD >k1pk112.dll</TD>
<TD >[(WWW)](http://www.rainbow.msk.ru)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >iKey 2000 series and for DataKey cards</TD>
<TD >dkck201.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >iKey 2000/2032</TD>
<TD >dkck232.dll</TD>
<TD >[(WWW)](http://www.rainbow.msk.ru)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >iKey 2032</TD>
<TD >dkck201.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >	</TD>
<TD >cryptoki22.dll</TD>
<TD >	</TD>
<TD >From wiki.cacert.org: for USB use Datakey driver</TD>
</TR>
<TR>
<TD ROWSPAN="1">Safelayer</TD>
<TD >HSM </TD>
<TD >p11card.dll</TD>
<TD >	</TD>
<TD >From wiki.cacert.org: for USB use Datakey driver</TD>
</TR>
<TR>
<TD ROWSPAN="3">Schlumberger</TD>
<TD >Cryptoflex</TD>
<TD >acpkcs.dll</TD>
<TD >[(WWW)](http://www.schlumberger.com)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Cryptoflex</TD>
<TD >slbck.dll</TD>
<TD >[(WWW)](http://www.schlumberger.com)</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Cyberflex Access</TD>
<TD >slbck.dll</TD>
<TD >[(WWW)](http://www.schlumberger.com)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">SeTec</TD>
<TD >SeTokI cards</TD>
<TD >SetTokI.dll</TD>
<TD >	</TD>
<TD >Full path to library may be required, e.g.:
C:\ Program Files\ Setec\ SetTokI\ SetTokI.dll</TD>
</TR>
<TR>
<TD ROWSPAN="2">Siemens</TD>
<TD >HiPath SIcurity Card</TD>
<TD >siecap11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Some Siemens Card OS cards</TD>
<TD >eTpkcs11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">SmartTrust</TD>
<TD >	</TD>
<TD >smartp11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Spyrus</TD>
<TD >	</TD>
<TD >SpyPK11.dll</TD>
<TD >[(WWW)](http://www.spyrus.com/)</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="1">Utimaco</TD>
<TD >Cryptoki for SafeGuard</TD>
<TD >pkcs201n.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD ROWSPAN="10">?</TD>
<TD >ActivCard cards</TD>
<TD >acpkcs.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >ActivClient</TD>
<TD >acpkcs211.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Datakey</TD>
<TD >dkck201.dll</TD>
<TD >	</TD>
<TD >From wiki.cacert.org: for Entrust</TD>
</TR>
<TR>
<TD >Datakey</TD>
<TD >pkcs201n.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Datakey CIP</TD>
<TD >dkck201.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Datakey/iKey </TD>
<TD >dkck232.dll</TD>
<TD >	</TD>
<TD >From wiki.cacert.org: NB: buggy, use 201</TD>
</TR>
<TR>
<TD >Fortezza Module</TD>
<TD >fort32.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >Oberthur AuthentIC</TD>
<TD >AuCryptoki2-0.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >SCW PKCS 3GI 3-G International</TD>
<TD >3gp11csp.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
<TR>
<TD >TeleSec</TD>
<TD >pkcs11.dll</TD>
<TD >	</TD>
<TD >	</TD>
</TR>
</TABLE>

The information listed above was compiled from multiple sources, including:

<UL>
* [CAcert Wiki: Pkcs11TaskForce](http://wiki.cacert.org/wiki/Pkcs11TaskForce)
* [StrongDisk Server (Russian documentation)](http://209.85.165.104/search?q=cache:-QugArnZ2I8J:www.strongdisk.ru/free/tech/SDSSettings.pdf+eutron+sadaptor.dll&hl=en&ct=clnk&cd=13)
* [cryptlib v3.1 testlib.c](http://csourcesearch.net/package/cryptlib/3.1/test/testlib.c)
* [IAIKPkcs11.properties](http://www.koders.com/noncode/fid57E0E0B63C88E5FF7A14B501B48BF0C95CF59E8D.aspx)
* [Using the IAIK JCE Provider for PKCS#11](http://jce.iaik.tugraz.at/sic/layout/set/print/products/core_crypto_toolkits/pkcs_11_provider/using)

</UL>



