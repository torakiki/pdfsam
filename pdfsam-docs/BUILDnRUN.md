Prerequisites
===========
* [Oracle JDK 8](https://www.java.com). PDFsam is written using the Java language version 8, you will need a JDK 8 or above.   
* [Maven](http://maven.apache.org/). Dependencies and project build is managed using Maven.
* [Gnu gettext](https://www.gnu.org/software/gettext/). Internationalization resources are built using gettext. Windows installers can be found at http://mlocati.github.io/gettext-iconv-windows/

Build
===========
* [clone the PDFsam repository](https://help.github.com/articles/cloning-a-repository/)   
* Checkout the tag you want to build. Ex. `git chckout 3.0.0.M4`   
* run one of the following commands from the project root:  
**Build and install the jars in the local repository executing all the unit test:**   
`mvn clean install -Prelease`    
**Build and install the jars in the local repository skipping tests:**   
`mvn clean install -Dmaven.test.skip=true -Prelease`    

Run
===========
Once you built the artifacts and they are locally installed you can run PDFsam following the steps  
* move to the pdfsam-community module with `cd pdfsam-community`
* run it with `mvn exec:java`   
Or you can find the zip bindle in the `target` directory of the `pdfsam-community` module.

