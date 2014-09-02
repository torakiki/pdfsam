Prerequisites
===========
To build PDFsam you need the [Maven](http://maven.apache.org/) and [Oracle JDK 8](https://www.google.com/?q=Oracle+jdk+8+download)   
Build
===========
**Build and install the jars in the local repository executing all the unit test:**   
    mvn clean install    
**Build and install the jars in the local repository executing quick unit test:**   
    mvn clean install -PfastTests    
**Build and install the jars in the local repository skipping tests:**   
    mvn clean install -Dmaven.test.skip=true    
    
Run
===========
Once you built the artifact and they are locally installed you can run PDFsam following the steps  
1. move to the pdfsam-gui module with `cd pdfsam-gui` 
2. run it with `mvn exec:java` 