<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="LibreCrypt: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;containers&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean 2015 tdk">


<TITLE>Technical Details: Mounting LibreCrypt Volumes</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/librecrypt/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![LibreCrypt logo](https://github.com/t-d-k/librecrypt/raw/master/src/Common/Common/images/DoxBox128.png)](http://LibreCrypt.tdksoft.co.uk/)
</SPAN>
<SPAN CLASS="master_title">
_[LibreCrypt](http://LibreCrypt.tdksoft.co.uk/): Open-Source disk encryption for Windows_
</SPAN>
***      
            

### Technical Details: Mounting LibreCrypt Volumes

To open a container, the following information must be obtained from the user:



 1 The volume to be mounted
 1 The user's password
 1 If the CDB to be used for mounting is stored in a keyfile, the location of the keyfile
 1 The length of salt used in the CDB (**sl**) 
 1 The number of key iterations
 1 When mounting a hidden volume, the following information is also required:
 1 The offset within the host file/volume that the hidden volume resides at
 1 If the offset specified gives the location of a CDB, or the "Encrypted partition image"

Although this list may sound as though it places a significant burden on the user to remember a significant amount of information. However, for most users the only information required will be the volume the wish to open, and their password; all of the other details may be defaulted to sensible values unless the user chooses to take advantage of the more advanced features of LibreCrypt.

From the information supplied by the user, the following can be determined automatically:

  1  The cypher used
  1 The hash algorithm used
  1 All of the details stored within "Volume details block" of the CDB used for mounting

The first two items are determined by brute force; by decrypting the CDB with every possible combination of hash/cypher installed on the user's system until the CDB's "Check MAC" is decrypted successfully.

In detail, the following procedure is used:

1 The information listed above is obtained from the user.

  
  
* The 512 bytes (4096 bits) CDB is read in.
	This will be taken from one of: 
	
		
		1 The start of the volume being mounted (provided the volume includes a CDB) 
		1 A host file, starting from a user specified offset, if mounting a hidden volume (provided the hidden volume includes a CDB) 
		1 A keyfile relating to the volume being mounted
		
	

* The first "**sl**" bits are stripped off the CDB, and are assumed to be the salt
* For each hash algorithm installed on the user's computer:
    


1 For each cypher algorithm installed on the user's computer:
	
	2 A "critical data key" is derived by processing the user's password and salt with PKCS #5 PBKDF2. The PRF used with PBKDF2 will be HMAC, with the current hash algorithm. The derived key should be **ks** bits long (i.e. the cypher's keysize). If **ks** is undefined, then 512 bits will be used.
	2 The number of "Random padding #1" bytes is determined, based on the cypher's blocksize. This random padding is then stripped off the CDB to give an "Encrypted block".
	2 The "Encrypted block" is decrypted using the "critical data key" generated above and the current cypher.
	2 The first 512 bytes of the decrypted "Encrypted block" are split off and retained as a plaintext check MAC and "Random padding #3".
	2 The remainder of the decrypted "Encrypted block" is assumed to be a "Volume details block". The HMAC of this "Volume details block" is generated, using the current hash algorithm and the "critical data key" just used for decryption.
	2 The generated MAC is truncated to its first 512 bytes, if it's longer than this.
	2 The resulting "n" bit MAC is compared with the first "n" bits of the "Check MAC"/"Random padding #3"
	2 If the MAC is identical to the "Check MAC", it is assumed that a valid cypher/hash combination has been found - the "Volume details block" is parsed and stored together with the hash/cypher combination. 
	2 Regardless of whether the generated MAC is identical to the "Check MAC", all remaining hash/cypher combinations will be processed in the same manner, to check if there are any other matching hash/cypher combinations.

        



After all possible hash/cypher combinations have been exhausted:

* If no cypher/hash combination successfully decrypted the "Volume details block", the user will be informed that they have entered incorrect details.

* If more than one cypher/hash combination successfully decrypted the "Volume details block", the user should be prompted to choose which of the combinations they wish to use. The system will proceed as if only the user selected combination had been successful in decrypting.

* If exactly one cypher/hash combination successfully decrypted the "Volume details block", then the "Encrypted partition image" will be decrypted as necessary using this cypher/hash combination, together with the information obtained from the decrypted "Volume details block".

As a result of the CDB layout, it is not necessary to store either the cypher or hash algorithm used for en/decryption anywhere. Nor is it necessary to prompt the user for this information as this information can be determined dynamically by attempting the open using all possible combinations of installed hash/cypher and testing if the same check MAC can be generated using the decrypted data.

<A NAME="level_4_heading_1">
#### Additional information
</A>
The driver API supports passing volume "metadata" to the driver when mounting a volume. Information passed in this way to the driver is simply stored by the driver against the mounted volume, and is not processed in any way.

When a user application calls DeviceIOControl with IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS, the number of bytes of volume metadata is returned. By calling with IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA, the actual data that was stored when mounting can be retrieved.

This is intended for future development; to enable user applications to store information against volumes they open (e.g. "This is a Linux volume", "This is a container"), which can later be retrieved for display to the user.

Volume metadata passed to the driver in this way should be kept to the **absolute minimum possible**.



