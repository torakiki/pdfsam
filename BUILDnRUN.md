Prerequisites
===========
To build PDFsam you need the [Maven](http://maven.apache.org/) and [Oracle JDK 8](https://www.java.com)   
Build
===========
**Build and install the jars in the local repository executing all the unit test:**   
`mvn clean install`    
**Build and install the jars in the local repository executing quick unit test:**   
`mvn clean install -PfastTests`    
**Build and install the jars in the local repository skipping tests:**   
`mvn clean install -Dmaven.test.skip=true`    
    
Run
===========
Once you built the artifact and they are locally installed you can run PDFsam following the steps  
* move to the pdfsam-community module with `cd pdfsam-community`
* run it with `mvn exec:java` 