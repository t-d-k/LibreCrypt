<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">


<TITLE>Technical Details: Mounting DoxBox Volumes</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">


<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***      
            

### Technical Details: Mounting DoxBox Volumes

To mount a DoxBox, the following information must be obtained from the user:


<OL>
 * The volume to be mounted
 * The user's password
 * If the CDB to be used for mounting is stored in a keyfile, the location of the keyfile
 * The length of salt used in the CDB (**sl**) 
 * The number of key iterations
 * When mounting a hidden volume, the following information is also required:
 * The offset within the host file/volume that the hidden volume resides at
 * If the offset specified gives the location of a CDB, or the "Encrypted partition image"
</OL>
Although this list may sound as though it places a significant burden
on the user to remember a significant amount of information. However,
for most users the only information required will be the volume the
wish to mount, and their password; all of the other details may be
defaulted to sensible values unless the user chooses to take advantage
of the more advanced features of DoxBox.

From the information supplied by the user, the following can be determined automatically:

<OL>

  *  The cypher used
  * The hash algorithm used
  * All of the details stored within "Volume details block" of the CDB used for mounting
</OL>
The first two items are determined by brute force; by decrypting the CDB with every possible combination of hash/cypher installed on the user's system until the CDB's "Check MAC" is decrypted successfully.

In detail, the following procedure is used:

<OL>

* The information listed above is obtained from the user.

  <OL>
  </OL>
* The 512 bytes (4096 bits) CDB is read in.
	This will be taken from one of: 
		<OL>
		
		* The start of the volume being mounted (provided the volume includes a CDB) 
		* A host file, starting from a user specified offset, if mounting a hidden volume (provided the hidden volume includes a CDB) 
		* A keyfile relating to the volume being mounted
		
		</OL>

* The first "**sl**" bits are stripped off the CDB, and are assumed to be the salt
* For each hash algorithm installed on the user's computer:
    

<OL>
* For each cypher algorithm installed on the user's computer:
<OL>
* A "critical data key" is derived by processing the user's password and salt with PKCS #5 PBKDF2. The PRF used with PBKDF2 will be HMAC, with the current hash algorithm. The derived key should be **ks** bits long (i.e. the cypher's keysize). If **ks** is undefined, then 512 bits will be used.
* The number of "Random padding #1" bytes is determined, based on the cypher's blocksize. This random padding is then stripped off the CDB to give an "Encrypted block".
* The "Encrypted block" is decrypted using the "critical data key" generated above and the current cypher.
* The first 512 bytes of the decrypted "Encrypted block" are split off and retained as a plaintext check MAC and "Random padding #3".
* The remainder of the decrypted "Encrypted block" is assumed to be a "Volume details block". The HMAC of this "Volume details block" is generated, using the current hash algorithm and the "critical data key" just used for decryption.
* The generated MAC is truncated to its first 512 bytes, if it's longer than this.
* The resulting "n" bit MAC is compared with the first "n" bits of the "Check MAC"/"Random padding #3"
* If the MAC is identical to the "Check MAC", it is assumed that a valid cypher/hash combination has been found - the "Volume details block" is parsed and stored together with the hash/cypher combination. 
* Regardless of whether the generated MAC is identical to the "Check MAC", all remaining hash/cypher combinations will be processed in the same manner, to check if there are any other matching hash/cypher combinations.

        

</OL>

</OL>

</OL>

After all possible hash/cypher combinations have been exhausted:

* If no cypher/hash combination successfully decrypted the "Volume details block", the user will be informed that they have entered incorrect details.

* If more than one cypher/hash combination successfully decrypted the "Volume details block", the user should be prompted to choose which of the combinations they wish to use. The system will proceed as if only the user selected combination had been successful in decrypting.

* If exactly one cypher/hash combination successfully decrypted the "Volume details block", then the "Encrypted partition image" will be decrypted as necessary using this cypher/hash combination, together with the information obtained from the decrypted "Volume details block".

As a result of the CDB layout, it is not necessary to store either the cypher or hash algorithm used for en/decryption anywhere. Nor is it necessary to prompt the user for this information as this information can be determined dynamically by attempting the mount using all possible combinations of installed hash/cypher and testing if the same check MAC can be generated using the decrypted data.

<A NAME="level_4_heading_1">
#### Additional information
</A>
The driver API supports passing volume "metadata" to the driver when mounting a volume. Information passed in this way to the driver is simply stored by the driver against the mounted volume, and is not processed in any way.

When a user application calls DeviceIOControl with IOCTL_FREEOTFE_GET_DISK_DEVICE_STATUS, the number of bytes of volume metadata is returned. By calling with IOCTL_FREEOTFE_GET_DISK_DEVICE_METADATA, the actual data that was stored when mounting can be retrieved.

This is intended for future development; to enable user applications to store information against volumes they mount (e.g. "This is a Linux volume", "This is a DoxBox"), which can later be retrieved for display to the user.

Volume metadata passed to the driver in this way should be kept to the **absolute minimum possible**.



