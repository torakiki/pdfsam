System properties
=============
A list of system properties that users can configure to alter PDFsam behavior.

**pdfsam.modules.directory**  
*values:* existing directory  
*default:* empty  
*description:* lets the user specify where the PDFsam modules are located in the filesystem. By default PDFsam searches for modules jars in its installation directory under the ```modules``` subdirectory.  


**pdfsam.disable.ui.restore**  
*values:* true|false 
*default:* false  
*description:* if set to true PDFsam will not restore the stage position, dimensions and mode, it will start with default values instead (maximized).  

Runtime arguments
=============
**-clean**  
*description:* clears user preferences starting PDFsam with default values for them. 
