<!-- tdk written (c) GPL documentation licence-->
<meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An Open-Source transparent encryption program for PCs. With this software, you can create one or more &quot;DoxBoxes&quot; on your PC - which appear as disks, anything written to these disks is automatically encrypted before being stored on your hard drive.">

<TITLE>Advanced Topics</TITLE>
<link href="https://raw.githubusercontent.com/t-d-k/doxbox/master/docs/styles_common.css" rel="stylesheet" type="text/css">

<link rel="shortcut icon" href="https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](https://github.com/t-d-k/doxbox/raw/master/src/Common/Common/images/DoxBox128.png)](http://DoxBox.eu/)
</SPAN>
<SPAN CLASS="master_title">
_[DoxBox](http://DoxBox.eu/): Open-Source disk encryption for Windows_
</SPAN>
***

## Translating DoxBox

* * *

<p>DoxBox currently supports the following languages:</p>

	* Croatian
	* Czech
	* English
	* French
	* German
	* Greek
	* Italian
	* Japanese
	* Russian
	* Spanish

### To translate into a new language:

	1.  Find the two-letter ISO 639-1 Alpha-2 code for the language you want to translate to (e.g. `fr` for French, `de` for German, `ru` for Russian). 
	Call this &#39;XX&#39;.
	1.  Install the latest copy of DoxBox.
	1.  Find the directory DoxBox was installed in (by default <code>c:\Program Files (86)\DoxBox\</code> and copy the `default.po` file stored under 
	<code>locale\en\LC_MESSAGES</code> to a new directory called: <code>locale\XX\LC_MESSAGES</code> where &#39;XX&#39; is the language code.  
	You can also find this file in the DoxBoxPortable.zip archive.
	1.  Edit your copy of `default.po`, adding the translated versions of each `msgid` string as the corresponding `msgstr` text. 
	For example, a German translation would be:
	<br/>	     
	<code>#  Example translation<br />
			  msgid "Hello world!"<br />
				msgstr "Hallo Welt!"<br /> 
	</code>
			 You can do this with any text editor like Notepad, but it is easier if you use Poedit (a free software tool for editing `.po` files).<br />
			 You also must edit the string for `English` (RS_LANGUAGE) to show the language of the translation.<br />

### To test a translation (This step is optional, you only need to do this if you want to test your translation)

	1. Compile your `default.po` file into a `default.mo` file

		* If you are using Poedit, go to `File | Preferences...` within Poedit, and make sure that the `Automatically compile .mo file on save` 
		option is checked. When you save your `default.po` file, Poedit should automatically generate a corresponding `default.mo` file for you
		*  If you are not using Poedit:

			+  Download and install the latest copy of `GNU gettext for Delphi` from [http://dybdahl.dk/dxgettext/](http://dybdahl.dk/dxgettext/)
			+  In Windows explorer, right-click your `default.po` file
			+  Select `Compile to mo file` from the context menu displayed. This should then generate you a `default.mo` file

	1.  Run DoxBox.exe
	1. Select the `View | Options...` menu-item
	1. On the `General` tab, select the language of your translation from the drop-down shown in the upper right
	1. Click `OK`	
	1. DoxBox&#39;s user interface should then switch to be displayed in your selected language.	

<p>If your translation isn&#39;t listed in the `Languages` drop-down, please check:</p>

		* That you translated the word `English` to the name of your language in your `default.po` file
		* You compiled your `default.po` file to a `default.mo` file
		* Your `default.mo` file is placed in the correct `locale\XX\LC_MESSAGES` directory

### Submit your translation for inclusion in the DoxBox project
<p>Please email your translated `default.po` file to the email on the [github contact page](https://github.com/t-d-k/doxbox/blob/master/docs/contact_details.md) , or add it in github if you have an account.<br />

Note: You don&#39;t have to translate all of the messages stored in `default.po`, though it would be very much appreciated.<br />
<br />
### Updating a Translation
When newer versions of DoxBox are released, a translation (.po) file can have newer text strings merged into it using Poedit:

	* In Poedit, go to `File | Open`, and open the `default.po` file with the existing translations in it
	* Go to `Catalog | Update from POT file` and specify the updated English `default.po` file (i.e. ...\locale\en\LC_MESSAGES\default.po) 
	Note: You may have to set the filter to `All files` when opening this file
	* Poedit should give you a dialog that shows what strings have been added and removed. If you `OK` this dialog, you should see 
	all the strings merged into the translation as appropriate. Note that Poedit will attempt to default some translations where it can; 
	these are marked as `fuzzy` translations, and should be manually checked to ensure that they are correct.
	
### Additional Notes

	* If you are unsure where any given piece of text is shown in the GUI, please ask where it can be found.
	* The `&amp;` character in a piece of text marks the next letter as a shortcut key for the control with that text (i.e. in the word &#39;C&amp;ancel pressing &#39;a&#39; will click the button with the text &#39;Cancel&#39; ). This letter is shown underlined in the control.
	* Acronyms should not be translated (e.g. IV, CDB, PKCS#11)                              
	* Entries which look similar to:
		<p><code>Library files (*.dll)|*.dll|All files|*.*</code></p>
    are filters for use with open/save dialogs. The text descriptions of these filters should be translated, but not the file masks
    (e.g. `Library file` and `All files` in the above example, but not `*.dll` or `*.*`)
  * A number of the text strings include `%s`, `%1`, etc. These are placeholders which will be replaced with automatically generated text.
 
    
