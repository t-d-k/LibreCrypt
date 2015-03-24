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

<h3>To translate into a new language:</h3>

	1.  Find the two-letter ISO 639-1 Alpha-2 code for the language you want to translate to (e.g. &quot;fr&quot; for French, &quot;de&quot; for German, &quot;ru&quot; for Russian). 
	Call this &#39;XX&#39;.
	1.  Install the latest copy of DoxBox.
	1.  Find the directory DoxBox was installed in (by default <code>c:\Program Files (86)\DoxBox\</code> and copy the &quot;default.po&quot; file stored under 
	<code>locale\en\LC_MESSAGES</code> to a new directory called: <code>locale\XX\LC_MESSAGES</code> where &#39;XX&#39; is the language code.  
	You can also find this file in the DoxBoxPortable.zip archive.
	1.  Edit your copy of &quot;default.po&quot;, adding the translated versions of each &quot;msgid&quot; string as the corresponding &quot;msgstr&quot; text. 
	For example, a German translation would be:
	<br/>	     
	<code>#  Example translation<br />
			 msgid &quot;Hello world!&quot;<br />
				msgstr &quot;Hallo Welt!&quot;<br /> 
	</code>
			 You can do this with any text editor like Notepad, but it is easier if you use Poedit (a free software tool for editing &quot;.po&quot; files).<br />
			 You also must edit the string for &quot;English&quot; (RS_LANGUAGE) to show the language of the translation.<br />

<h3>To test a translation (This step is optional, you only need to do this if you want to test your translation)</h3>

	1. Compile your &quot;default.po&quot; file into a &quot;default.mo&quot; file

		* If you are using Poedit, go to &quot;File | Preferences...&quot; within Poedit, and make sure that the &quot;Automatically compile .mo file on save&quot; 
		option is checked. When you save your &quot;default.po&quot; file, Poedit should automatically generate a corresponding &quot;default.mo&quot; file for you
		*  If you are not using Poedit:

			+  Download and install the latest copy of &quot;GNU gettext for Delphi&quot; from <a href="http://dybdahl.dk/dxgettext/">http://dybdahl.dk/dxgettext/</a>
			+  In Windows explorer, right-click your &quot;default.po&quot; file
			+  Select &quot;Compile to mo file&quot; from the context menu displayed. This should then generate you a &quot;default.mo&quot; file

	1.  Run DoxBox.exe
	1. Select the &quot;View | Options...&quot; menu-item
	1. On the &quot;General&quot; tab, select the language of your translation from the drop-down shown in the upper right
	1. Click &quot;OK&quot;	
	1. DoxBox&#39;s user interface should then switch to be displayed in your selected language.	

<p>If your translation isn&#39;t listed in the &quot;Languages&quot; drop-down, please check:</p>

		* That you translated the word &quot;English&quot; to the name of your language in your &quot;default.po&quot; file
		* You compiled your &quot;default.po&quot; file to a &quot;default.mo&quot; file
		* Your &quot;default.mo&quot; file is placed in the correct &quot;locale\XX\LC_MESSAGES&quot; directory

<h3>Submit your translation for inclusion in the DoxBox project</h3>
<p>Please email your translated &quot;default.po&quot; file to the email on the <a href="https://github.com/t-d-k/doxbox/blob/master/docs/contact_details.md">github contact page</a> , or add it in github if you have an account.<br />

Note: You don&#39;t have to translate all of the messages stored in &quot;default.po&quot;, though it would be very much appreciated.<br />
<br />
<h3>Updating a Translation</h3>
When newer versions of DoxBox are released, a translation (.po) file can have newer text strings merged into it using Poedit:

	* In Poedit, go to &quot;File | Open&quot;, and open the &quot;default.po&quot; file with the existing translations in it
	* Go to &quot;Catalog | Update from POT file&quot; and specify the updated English &quot;default.po&quot; file (i.e. ...\locale\en\LC_MESSAGES\default.po) 
	Note: You may have to set the filter to &quot;All files&quot; when opening this file
	* Poedit should give you a dialog that shows what strings have been added and removed. If you &quot;OK&quot; this dialog, you should see 
	all the strings merged into the translation as appropriate. Note that Poedit will attempt to default some translations where it can; 
	these are marked as &quot;fuzzy&quot; translations, and should be manually checked to ensure that they are correct.
	
<h3>Additional Notes</h3>

	* If you are unsure where any given piece of text is shown in the GUI, please ask where it can be found.
	* The &quot;&amp;&quot; character in a piece of text marks the next letter as a shortcut key for the control with that text (i.e. in the word &#39;C&amp;ancel pressing &#39;a&#39; will click the button with the text &#39;Cancel&#39; ). This letter is shown underlined in the control.
	* Acronyms should not be translated (e.g. IV, CDB, PKCS#11)                              
	* Entries which look similar to:
		<p><code>      Library files (*.dll)|*.dll|All files|*.*</code></p>
    are filters for use with open/save dialogs. The text descriptions of these filters should be translated, but not the file masks
    (e.g. &quot;Library file&quot; and &quot;All files&quot; in the above example, but not &quot;*.dll&quot; or &quot;*.*&quot;)
  * A number of the text strings include &quot;%s&quot;, &quot;%1&quot;, etc. These are placeholders which will be replaced with automatically generated text.
 
    
