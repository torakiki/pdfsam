System properties
=============
A list of system properties that users can configure to alter PDFsam behavior.

**org.pdfsam.modules.directory**  
*values:* existing directory  
*default:* empty  
*description:* lets the user specify where the PDFsam modules are located in the filesystem. By default PDFsam searches for modules jars in its installation directory under the ```modules``` subdirectory.  


**org.pdfsam.disable.ui.restore**  
*values:* true|false  
*default:* false  
*description:* if set to true PDFsam will not restore the stage position, dimensions and mode, it will start with default values instead (maximized).  


**org.pdfsam.default.locale**  
*values:* a supported locale  
*default:* default locale  
*description:* if no locale is set in the application preferences, tells PDFsam to use this locale.   


**org.pdfsam.default.checkforupdate**  
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should check for updates at startup. 


Runtime arguments
=============
**-clean**  
*description:* clears user preferences and locally stored information, starting PDFsam with default values. 


MSI properties
=============
A list of properties that can be set during the silent installation of the PDFsam MSI package.

**CHECK_FOR_UPDATES**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should check for updates at startup. 


**LOCALE_CODE**
*values:* a supported locale  
*default:* default locale  
*description:* if no locale is set in the application preferences, tells PDFsam to use this locale.  


**SKIPTHANKSPAGE**
*values:* Yes|No  
*default:* No  
*description:* Tells the MSI installer to skip or not the thanks page that is usually opened once the installation completes successfully .  

