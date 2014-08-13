

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Plausible Deniability</TITLE>

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

      
            
## Plausible Deniability

<UL>
  * [Overview](#level_3_heading_1)
  * [Legal Issues](#level_3_heading_2)
  * [Hidden Volumes](#level_3_heading_3)
  * [More Advanced Hidden Volumes](#level_3_heading_4)
  * [In Practice](#level_3_heading_5)
</UL>

* * * 
<A NAME="level_3_heading_1">
### Overview
</A>

The subject of "plausible deniability" and OTFE systems is a lot more involved than "do my volume files have any kind of identifying signature?"

"Plausible deniability" in OTFE systems is largely based on the _theory_ that you can claim that your volume files are _not_ encrypted data; you don't know _what_ they are - you can't be expected to know every operation that your OS carries out! Perhaps it's some corrupt data that the system recovered at some stage?

This _claim_ is only possible with OTFE systems which do not embed any kind of "signature" into their encrypted data (typically an _unencrypted_ critical data area).
DoxBox files have no such signature.

However, this simplistic approach to plausible deniability has many drawbacks, and is _highly unlikely_ to offer any significant form of protection; for example:

<OL>
  * Encrypted data has relatively high entropy - something "corrupt files, recovered automatically by the OS" are _not likely_ to have. (Recovered data is likely to have some form of recognisable structure, or signature, _somewhere_ within it)

  * Having several GB of high-entropy data stored on your HDD, together with an OTFE package, is likely to be viewed as "suspicious" at the very least...

</OL>

* * * 
<A NAME="level_3_heading_2">
### Legal Issues
</A>

**Legally**, the presence of large volume files (even without any form of signature) may very well be viewed as grounds for **reasonable suspicion** to be raised; in which case further action may be taken by an attacker (e.g. arrest, interrogation, beating, torture, and other rubber hose cryptanalysis techniques). Obviously, if reasonable suspicion can be raised, this simplistic approach provides very little in the way of _plausible_ deniability!

Legally (in the US at least, and in theory) what it really boils down to is: does the fact that a prosecution cannot prove that the data held is encrypted data, together with a user's denial, produce _reasonable doubt_ as to whether the data is not an OTFE volume or not? Raising reasonable doubt as to what your volumes files really are is the aim of "plausible deniability"; leaving it up to the prosecution to prove, **beyond reasonable doubt,** they store encrypted data.

In the UK (AIUI), things are slightly different with the "<span class="pagetitle">Regulation of Investigatory Powers Act (RIPA)" (see </span><span class="pagetitle"></span><a href="http://www.legislation.hmso.gov.uk/acts/acts2000/20000023.htm">http://www.legislation.hmso.gov.uk/acts/acts2000/20000023.htm</a>).
Depending largely on how the courts may interpret it, a user (as defendant) may eventually find yourself in the position of **effectively **having to ****prove that the data found is **not** an OTFE volume (or any other form of encrypted data).

* * * 
<A NAME="level_3_heading_3">
### Hidden Volumes
</A>

More advanced OTFE systems go one step further: support for hidden
volumes (as FreeOTFE does).

Here, you have a "normal" OTFE volume filled with data that you have no
objection to disclosing to an attacker. Mounting the volume with a
different password causes the OTFE system to read a different part of
the "host" volume file; giving access to a separate "hidden" volume.

Here the concept of "plausible deniability" is much stronger;
theoretically an attacker is not able to determine (let alone prove)
whether or not such a hidden volume is present.

However, the implementation of such "hidden volumes" is not as trivial
as it may seem at first.

In order for this approach to be successful, the host volume file must be initialized by writing random data to it. This is required since the host volume file may well have been created by simply writing 0x00's to your HDD in order to generate a large enough file. Any hidden volume stored within such a host volume file may well cause an attacker suspicion as to whether a hidden volume exists. (The hidden volume will appear as a large amount of high-entropy data, stuck in the middle of the volume file; interrupting the neat pattern of 0x00's!)

The "random data" used for this process cannot simply be pseudorandom data; given the size of a typical volume file (even ones as small as a MB), pseudorandom data can potentially be identified as such, and become predictable. In this case, your hidden volume will not appear as high-entropy data stuck in the middle of a series of 0x00 bytes, but as high-entropy data interrupting any pattern formed by the pseudorandom data!

Because truly random data can be difficult to rapidly generate in large
quantities using a computer. Pseudorandom data _can_
still be used though: by encrypting it before it is
written to the host volume file. In principle, although not as good as
a
cryptographically secure RNG, this should give the data written to the
volume file a suitable degree of entropy.

The easiest way of accomplishing this is, which will work with _any_
OTFE system, is to mount the host volume as per normal and overwrite
all of its free space with a single pass of pseudorandom data. The
data written to the mounted volume will be encrypted as it is written
to the host volume file.

* * * 
<A NAME="level_3_heading_4">
### More Advanced Hidden Volumes
</A>

The technique described above for mounting and overwriting a volume before creating a hidden volume on it still isn't enough though.

If you were to be forced to hand over the key to the outer, "host",
volume; an attacker could then apply the same analysis - but this time
to the mounted (plaintext) version of your host volume. Again, any
hidden volume may well stick out in any pattern within the pseudorandom data.

The solution suggested is to encrypt the pseudorandom data _before_
it is used to overwrite the mounted volume's free space; any attacker
attempting to identify a hidden volume, even with the key to the outer
"host" volume, would not be able to differentiate between your
encrypted pseudorandom data, and an encrypted hidden volume.

This all assumes the cypher used is strong enough, of course...

For obvious reasons, all such overwriting must be carried out _before_ the hidden volume is created
(doing so afterwards would probably corrupt your hidden volume!)

* * * 
<A NAME="level_3_heading_5">
### In Practice
</A>

Needless to say, FreeOTFE offers full functionality with overwriting and the encryption of random data used.

To ensure the maximum security for your volumes, the following procedure is suggested after creating each new volume:

1. Mount the new volume
1. Select the new volume just mounted, and then select the "Tools | Overwrite entire drive..." menuitem. (Note: The "Overwrite free space..." option should _not_ be selected for this purpose, as this will miss overwriting parts of the volume which the filesystem reserves)
1. Doublecheck that you have selected the right volume, and confirm your actions at the prompt displayed
1. Select "Encrypted data", and a suitable cypher from the dropdown list. Note that the cypher selected does not have to be the same as the one used to secure your volume.
1. Click "OK"
1. Generate some random data to be used as the key for the cypher by "waggling" the mouse pointer over the space shown on the dialog displayed
1. Click "OK"
1. Click "Yes" to confirm you wish to proceed

At this point, the volume will be overwritten. Depending on your hardware, _this process may take some time!_ After the overwrite completes, format the drive. The new volume will then be ready for use. To create a hidden volume within the volume just created, dismount the drive and carry out the normal procedure for creating a hidden volume (see the [Advanced Topics](advanced_topics.htm) section for instructions on how to do this).



