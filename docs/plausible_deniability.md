
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Plausible Deniability</TITLE>

<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">

<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***
            
## Plausible Deniability

  * [Overview](#level_3_heading_1)
  * [Legal Issues](#level_3_heading_2)
  * [Hidden Volumes](#level_3_heading_3)
  * [Legacy Versions](#older)  
  * [Multiple Hidden Volumes](#multiple)
	
<!--  * [Practical problems with deniability](#practical) -->		
 

* * * 

<A NAME="level_3_heading_1">
### Overview
</A>

The subject of "plausible deniability" and transparant encryption systems is a lot more involved than "do my DoxBoxes have any kind of identifying signature?"

Some people believe they can get "Plausible deniability" by simply claiming that their volume files are _not_ encrypted data: "I don't know _what_ they are - I can't be expected to know every operation that my OS carries out! Perhaps it's some corrupt data that the system recovered at some stage?"

However, while DoxBox files have no 'signature' that identifies them as such, this approach does not offer any significant form of protection because:

  * Encrypted data has high 'entropy' - i.e. it looks like 'random' data - something corrupt files, partial downloads, etc do not have. (Recovered data is likely to have some form of recognisable structure, or signature, _somewhere_ within it)
  * Having several GB of high-entropy data stored on your disk, together with an encryption app, is likely to be viewed as "suspicious".

* * * 
<A NAME="level_3_heading_2">
### Legal Issues
</A>

**Legally**, the presence of large volume files (even without any form of signature) may very well be viewed as grounds for **reasonable suspicion** to be raised; in which case further action may be taken by an attacker (e.g. arrest, interrogation, beating, torture, and other rubber hose cryptanalysis techniques). Obviously, if reasonable suspicion can be raised, this simplistic approach provides very little in the way of _plausible_ deniability!

Legally (in the US at least, and in theory) what it really boils down to is: does the fact that a prosecution cannot prove that the data held is encrypted data, together with a user's denial, produce _reasonable doubt_ as to whether the data is encrypted or not? Raising reasonable doubt as to what your volumes files really are is the aim of "plausible deniability"; leaving it up to the prosecution to prove, **beyond reasonable doubt**, they store encrypted data.

In the UK (AIUI), things are slightly different with the "<span class="pagetitle">Regulation of Investigatory Powers Act (RIPA)</span>" (see <span class="pagetitle">[http://www.legislation.hmso.gov.uk/acts/acts2000/20000023.htm](http://www.legislation.hmso.gov.uk/acts/acts2000/20000023.htm)</span>).
Depending largely on how the courts may interpret it, a user (as defendant) may eventually find themselves in the position of **effectively** having to **prove** that the data found is **not** encrypted, or of proving they have forgotten the password.

* * * 
<A NAME="level_3_heading_3">
### Hidden Volumes
</A>

More advanced transparent systems go one step further: support for 'hidden' volumes (as DoxBox does).

Here, you have a "normal" DoxBox filled with data that you are prepared to disclose to an attacker. Opening the Box with a different password causes DoxBox to read a different part of the file containing the DoxBox; giving access to a separate "hidden" DoxBox.

Here the concept of "plausible deniability" is much stronger; theoretically an attacker is not able to determine (let alone prove) whether or not such a hidden DoxBox is present.

However, the implementation of such "hidden DoxBoxes" is not as easy as it may seem at first.

The host file may well have been created by writing zeros to your HDD in order to generate a large enough file. Any hidden volume stored within such a host file will stand out from the 'background'. The hidden Box will appear as a large amount of high-entropy data, stuck in the middle of the file; interrupting the neat pattern of zeros, and an attacker will know a hidden Box exists.

So in order for this approach to be successful, the DoxBox file must be initialised by writing it with data indistinguishable from encrypted data. This background data is sometimes referred to as 'chaff'. 

The 'chaff' cannot be just pseudo-random data; pseudo-random data can potentially be distinguished from encrypted data, and even be predictable. In this case, the hidden volume will appear as high-entropy data against a pattern formed by the pseudo-random data.

Truly random data can be difficult to rapidly generate in large quantities using a computer. However data produced by a 'cryptographically secure pseudorandom number generator' (CSPRNG) is thought to be indistinguishable from random data and, importantly, from encrypted data without cracking the cypher.<1--TODO:ref-->

So to attain plausible deniability, DoxBox automatically overwrites any host file or partition, when its created, with the output from a CSPRNG.

The CSPRNG data is produced by encrypting pseudorandom data using a truly random key.

If no other source of randomness is enabled (on the 'RNG' tab of the advanced dialog) you will have to "waggle" the mouse pointer over the space shown on the dialog displayed.

This ensures that if a hidden volume is created, the encrypted data is invisible against the 'background'.

A related issue is that without this 'chaff' an attacker can easily tell the amount of data stored in the volume. This is because the encrypted data will only be in the file where data has been written. 

This overwriting can take a long time for large volumes, particularly on flash drives. It can be disabled on the 'chaff' tab of the advanced creation dialog.

If this is disabled, note the following:

1. An attacker can easily tell the amount of data stored in the volume.
2. If at a later date you add a hidden volume to the file, this can easily be found by an attacker (but see below)
3. If at a later date you create a volume with 'chaff' intending to create a hidden volume in it, an attacker may wonder why this volume is different and conclude it contains a hidden volume.

The only way to avoid this last - an attacker guessing a volume contains a hidden one, because chaff was used - is to add chaff by default to all volumes and require the user to explicitly disable it. This provides a plausible reason why chaff was used on a particular volume.

If you create a volume without using 'chaff' and at a later date you want to add a hidden volume, a way of avoiding making it visible (point 2 above) is to overwrite the free space in the 'outer' volume with CSPRNG data; to do this:

1. Mount the outer volume
1. Select the new volume just mounted, and then select the "Tools | Overwrite free space..." option  
1. Double-check that you have selected the right volume, and confirm your actions at the prompt displayed
1. Select "Encrypted data", and a suitable cypher from the drop-down list. Note that the cypher selected does not have to be the same as the one used to secure your volume.
1. Click "OK"
1. Generate some random data to be used as the key for the cypher by "waggling" the mouse pointer over the space shown on the dialog displayed (if no cryptlib is selected)
1. Click "OK"
1. Click "Yes" to confirm you wish to proceed
		
At this point, the free space will be overwritten. Depending on your hardware, _this process may take some time_ . To create a hidden volume, dismount the drive and carry out the normal procedure for creating a hidden volume (see the [Advanced Topics](advanced_topics.html) section for instructions on how to do this).	
		
Note this will miss overwriting parts of the volume which the filesystem reserves, it is also slower than using 'chaff' to start with (as the data is encrypted twice). 

### Legacy Versions		

DoxBox v6.0 and FreeOTFE did _not_ use 'chaff' as described above. A manual process was given (similar to that above), however this would have to be done on every volume created - even those without hidden volumes - to achieve plausible deniability.

If you ever intend to use hidden volumes, and you did not follow this process, it's recommended to create new volumes in DoxBox v6.1 and move any data over.
You should do likewise if you did not follow this process, and you wish to prevent an attacker knowing the amount of data stored in the Box.

<!--
<A NAME="practical">
### Practical problems with deniability
</A>

TODO:

* repeated access (backups, wear levelling) may show cgd dta and reveal where data was enxd
PD relies on the enc data looking like background 'chaff'. 
* links, MRU lists
* registry storing FS ids and device ids
* deleted files in outer vol.
* watermarking attacks
-->
<A NAME="multiple">
### Multiple Hidden Volumes
</A>

In order to increase security further, a transparant encryption system should not be limited to _just_ a _single_ hidden volume in any given volume, but should allow an arbitrary number of hidden volumes to be created.

This gives better security over systems which only permit a maximum number of hidden volumes within any given host volume. Even if an attacker manages to force the user to reveal that a hidden volume has been stored, there is still no way in which to determine whether _more_ hidden volumes are still present. This has the side effect of reducing any incentive for a user to reveal the presence of _any_ hidden volume, as doing so would not prevent an attacker from continuing to apply pressure on the user on the basis that there _may_ be further hidden volumes.




