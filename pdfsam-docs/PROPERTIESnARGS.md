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


**org.pdfsam.default.checkfornews**  
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should check for news at startup. 


**org.pdfsam.default.play.sounds**  
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should play alter sounds. 


**org.pdfsam.disable.split.optimization**  
*values:* true|false  
*default:* false   
*description:* if set to true PDFsam will not try to optimize split tasks result files.


**org.pdfsam.default.donate.notification**  
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should show the notification to kindly ask for a donation.


**org.pdfsam.default.fetch.premium.modules**  
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should fetch and show premium modules description in the modules dashboard  

Runtime arguments
=============
**-c**    
**-clean**  
**--clean**  
*description:* clears user preferences and locally stored information, starting PDFsam with default values. 

**--workspace="/path/to/workspace"**  
*description:* loads the given workspace. This has precedence over the application settings. 

**/path/to/file1.pdf /path/to/file2.pdf**  
*description:* a list of PDF files to be opened with PDFsam. 

MSI properties
=============
A list of properties that can be set during the silent installation of the PDFsam MSI package.

**CHECK_FOR_UPDATES**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should check for updates at startup. 


**CHECK_FOR_NEWS**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should check for news at startup. 
*since:* 3.2.5  


**PLAY_SOUNDS**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should play alert sounds. 
*since:* 3.2.5  


**DONATE_NOTIFICATION**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should show the notification to kindly ask for a donation.   
*since:* 3.2.1  


**LOCALE_CODE**
*values:* a supported locale  
*default:* default locale  
*description:* if no locale is set in the application preferences, tells PDFsam to use this locale.  


**SKIPTHANKSPAGE**
*values:* Yes|No  
*default:* No  
*description:* Tells the MSI installer to skip or not the thanks page that is usually opened once the installation completes successfully.  


**PREMIUM_MODULES**   
*values:* true|false  
*default:* true   
*description:* if not already set in the application preferences, tells PDFsam if it should fetch and show premium modules description in the modules dashboard.  
*since:* 3.3.0  

