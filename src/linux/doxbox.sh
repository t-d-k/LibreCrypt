#!/bin/bash

verbose=true;
pathArgs=0;

function vecho
{
	if $verbose ; then echo -e "$*"; fi
}


function setup_loop()
{
	sudo /sbin/losetup  /dev/loop0 ${vol_path}
	if [[ $? != 0 ]] ; then
		echo "losetup command failed"
		exit 1
	fi
}

function setup_crypt()
{
	# get pword and create
	if [ "$hidden" != "Y" ]; then
		sudo /sbin/cryptsetup luksOpen /dev/loop0 enc1	
		if [[ $? != 0 ]] ; then
			echo "command failed"
			exit 1
		fi
	else
		# mount plain vol at offset
		#  ${offset} is actual offset/512
		sudo /sbin/cryptsetup create enc1 /dev/loop0 --offset ${offset} -s 256 -h sha512 -c aes-cbc-essiv:sha256 
		if [[ $? != 0 ]] ; then
			echo "command failed"
			exit 15
		fi
		vecho To mount in DoxBox use offset of `expr ${offset} \* 512` bytes

	fi

}

next_vol=false;
next_size=false;
next_fs=false;
create_vl=false;
mount_vl=false;
unmount_vl=false;
vol_path="";
mb_count="";
ntfs="";
for i in $*; do

	# not ideal -better to do comparison than grep
	if $next_vol ; then
		vecho "vol: $i";
		vol_path="$i";
		next_vol=false;
	else
		if $next_size ; then
			vecho "size: $i";
			mb_count="$i";
			next_size=false;
		else	
			if $next_fs ; then
				vecho "ntfs: $i";
				ntfs="$i";
				next_fs=false;
			else					
				if [ "$i" == "-h" ]; then 
					echo -e "Usage: common.sh [create|mount|unmount] [-h] [/path/to/file]";
								
					echo -e "\t-n\t\tNot verbose ";
					echo -e "\n";
					echo -e "\t\t\t";
					echo -e "\t-s\t\tsize of volume file";
					echo -e "\t--vol\t\tvolume file";
					echo;                                                                           
					exit 0;
					elif [ `echo "$i" | grep '\-n'` ]; then verbose=false; 
					elif [ `echo "$i" | grep '\-s'` ]; then next_size=true;
					elif [ `echo "$i" | grep '\-f'` ]; then next_fs=true;
					elif [ `echo "$i" | grep '\-\-vol'` ]; then			next_vol=true;
					elif [ "$i" == "create" ]; then			create_vl=true;
					elif [ "$i" == "mount" ]; then			mount_vl=true;
					elif [ "$i" == "unmount" ]; then		
						unmount_vl=true;
					else
						echo "$i is not a valid param";
						exit 1;
				fi
			fi
		fi
	fi
done;

function check_vol_path()
{
	if [ "$vol_path" = "" ]; then
		echo 'Please enter file or device ' 
		read vol_path
	fi
	echo vol=${vol_path}
}

function mount_enc()
{
	# ideally mount ro for outer vols
	mount /dev/mapper/enc1 

	if [[ $? != 0 ]] ; then
		echo "mount command failed. You probably entered the wrong passphrase"
		sudo /sbin/cryptsetup luksClose enc1
		sudo /sbin/losetup  -d /dev/loop0
		exit 16
	fi
	vecho 'mounted at /mnt'

	#  output options for DoxBox - enc type etc
	sudo /sbin/cryptsetup status enc1
	if [[ $? != 0 ]] ; then
		echo "cryptsetup status command failed"
		exit 17
	fi
}

# just unmounts and removes crypt - does not remove loop device
function unmount_crypt()
{
	umount /dev/mapper/enc1

	if [[ $? != 0 ]] ; then
		echo "umount command failed"
		exit 1
	fi
	sudo /sbin/cryptsetup luksClose enc1

	if [[ $? != 0 ]] ; then
		echo "command failed"
		exit 2
	fi
	vecho 'unmounted'
}

function get_offset()
{
	echo "Automatically calculate offset? (Y/n)"
	#todo: NO cmd line option
	read calc_offset
	if [ "$calc_offset" == "Y" ] ; then

		# if already mounted - just calc offset, else prompt to mount
		# find whether mounted
		mount_line=`mount | grep "/dev/mapper/enc1" `
		# vecho $mount_line
		if [ "$mount_line" == "" ] ; then
			echo "use OUTER volume password"
			setup_loop
			setup_crypt
			mount_enc
		fi
		# outer vol is now mounted
		calc_offset_mounted
		unmount_crypt

	else # creating hidden and not calc offset - get from user
		echo 'Enter offset in mulitples of 512 bytes'
		read offset
	fi
}

function calc_offset_mounted()
{
	# attempt to get size of mounted fs
	mount_line=`mount | grep "/dev/mapper/enc1" `
	# echo $mount_line
	# find whether mounted
	if [ "$mount_line" != "" ] ; then
		#find file size and set offset 
		mounted_size=`/bin/df |grep enc1| cut  -c 31-40`
		vecho mounted_size=$mounted_size
		# = fs size in K
		# calc number of bytes rounded up to nearest MB then divided by 512
		# as offset is in 512 bytes 
		# add luks hdr size of 2048 sectors = 1024 kbytes + add 1 mb to truncated value, convert to blocks of 512 bytes 
		offset=`expr  \( \( $mounted_size + 1024 + 1024 \)  / 1024 \) \* 2 \* 1024`
		vecho offset=$offset

	else # unmounted
		# prompt to mount /overwrite - or enter fs
		echo "The outer volume needs to be mounted to calc offset - but is not" 
		exit 12
	fi
}


function create_vol()
{
	echo 'This script will create a linux volume compatible with the defaults used by the Windows DoxBox program'
	
	check_vol_path
	
	if  [ -f "$vol_path" ]; then 
		# already exists
		echo "This file already exists. Add a hidden volume? (Y/n)"
		#todo: NO cmd line option
		read mhidden
		if [ $mhidden = "Y" ]; then
			get_offset
			echo "next enter INNER volume password"
			hidden=$mhidden
		else
			# file exists but not creating hidden
			echo "Cannot create - file already exists and not creating a hidden vol."
			echo "Please delete file and try again"
			exit 11
		fi # not hidden 
	
	else # file doesnt exist
		if [ "$mb_count" = "" ]; then
			echo 'Enter size in Mb'
			read mb_count
		fi
	fi # vol not exists
	
	if [ "$hidden" != "Y" ]; then
		#  wipe with urandom
		echo "wiping with urandom"
		echo ${mb_count} MB
		echo 'please wait'
		#todo: show progress
		# watch -n 5 killall -s USR1 dd &
		vecho of=${vol_path} bs=1M count=${mb_count}	
		dd if=/dev/urandom of=${vol_path} bs=1M count=${mb_count}	
		# echo $?
		if [[ $? != 0 ]] ; then
			echo "dd command failed"
			exit 1
		fi
	else # hidden
		#todo: wipe from offset
		echo "not wiping"
	fi
	if [ "$hidden" == "Y" ]; then
		echo "should be unmounted"
		#	unmount_crypt
	else
		vecho "setup_loop"
		setup_loop
	fi
	# get pword and create
	if [ "$hidden" != "Y" ]; then
		# create luks volume,
		vecho 'creating luks volume'
		sudo /sbin/cryptsetup -y luksFormat /dev/loop0
		if [[ $? != 0 ]] ; then
			echo "command failed"
			exit 1
		fi
		# luks_open called in setup_crypt
	else
		# create called in setup_crypt
		echo "not creating luks volume"
	fi
	
	
	setup_crypt
	vecho "setup crypt"
	
	if [ "$ntfs" = "" ]; then
		echo 'fs ntfs? (Y/n - fat otherwise)'
		read ntfs
	fi
	
	if [ $ntfs == "Y" ]; then
	
		# mkfs and mount
		sudo /sbin/mkfs.ntfs -Q /dev/mapper/enc1
	else
		sudo /sbin/mkfs.vfat /dev/mapper/enc1	
	fi
	
	mount_enc
}

function mount_vol()
{
	echo 'This script will mount a linux volume compatible with the defaults used by the Windows DoxBox program'	
	
	check_vol_path
	
	if  [ -f ${vol_path} ]; then 
		# already exists
		echo "Mount hidden volume? (Y/n)"
		# NO cmd line option
		# setup etc use hidden - so have seperate var here
		read mhidden
		if [ $mhidden = "Y" ]; then
			get_offset
			echo "Next enter INNER volume password"
			hidden=$mhidden
		else
			# unmount leaves loop setup
			setup_loop
		fi # not hidden 
	
		setup_crypt
		mount_enc  
	
	else
		echo 'cannot mount volume - doesnt exist'
		exit 4
	fi # vol not exists
}

function unmount_vol()
{
	echo 'This script will unmount a linux volume created/mounted with the DoxBox scripts compatible with the Windows DoxBox program'

	unmount_crypt
	
	sudo /sbin/losetup  -d /dev/loop0
	if [[ $? != 0 ]] ; then
		echo "command failed"
		exit 3
	fi
	
	# todo: output options for DoxBox - enc type etc

}

if  $create_vl ; then 
	create_vol
fi

if  $mount_vl  ; then 
	mount_vol
fi

if  $unmount_vl  ; then 
	unmount_vol
fi

