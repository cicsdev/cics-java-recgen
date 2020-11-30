cics-java-recgen
================
Sample CICS Java program to use a record from the IBM Record Generator for Java to build a COMMAREA for linking to the CICS COBOL EDUPGM sample.

## Supporting files
* [`com.ibm.cicsdev.jzostest`](projects/com.ibm.cicsdev.jzostest) - Eclipse OSGi plugin project 
* [`com.ibm.cicsdev.jzostest.cicsbundle`](projects/com.ibm.cicsdev.jzostest.cicsbundle) - CICS bundle project
* [`EDUPGM.cbl`](src/Cobol/EDUPGM.cbl) - Sample CICS COBOL application that demonstrates the different data types that can be used in COBOL.
* [`EDUCPY.cbl`](src/Cobol/EDUCPY.cbl) - Copybook that describes the record structure of the CICS COMMAREA for EDUPGM

## Reference
* Blog - [Building Java records from COBOL with the IBM Record Generator for Java](blog.md)
* IBM Record Generator for Java [download](https://ibm.github.io/mainframe-downloads/IBM-Record-Generator-for-Java.html)

## Java code 
Source code for the following Java class is available in the projects directory
* [`JZOSprog.java`](projects/com.ibm.cicsdev.jzostest/src/com/ibm/cicsdev/jzostest/JZOSprog.java)- JCICS CICS Java program to link to EDUPGM COBOL program using generated record

The generated record JAR file EDUPGM.jar is supplied in the [`lib`](projects/com.ibm.cicsdev.jzostest/lib) directory of the Eclipse OSGi plugin project 


## Pre-reqs

* CICS TS V4.2 or later
* Java SE 1.7 or later on the z/OS system
* Java SE 1.7 or later on the workstation
* Eclipse with CICS Explorer SDK installed
* IBM Record Generator for Java V3.0 


## Configuration

The sample project can be imported into Eclipse and deployed into a CICS OSGi JVM server as follows:


1. Import the Eclipse projects from the project folder into CICS Explorer using **File -> Import -> General -> Existing projects into workspace**. 
2. Change the name of the JVMSERVER in the .osgibundle file from DFHJVMS to the name of the JVMSERVER resource defined in CICS. 
3. Export the bundle project to zFS by selecting **Export Bundle project to z/OS Unix File System** from the context menu.
4. Create a CICS  bundle defintion, setting the bundle directory attribute to the zFS location you just exported to, and install it. 





