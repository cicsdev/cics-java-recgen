cics-java-jzosprog
================
Sample CICS Java program to use a JZOS generated record to build a COMMAREA for linking to the CICS COBOL EDUPGM sample.

The following components are supplied in this repository.

## Java package com.ibm.cicsdev.bean
Source code for the following Java class is available in the src/Java  directory 
* JZOSCommareaWrapper.java - Generated JZOS record from EDUCPY copybook

## Java package com.ibm.cicsdev.jzostest
Source code for the following Java classes is available in the src/Java directory
* JZOSprog.java- JCICS CICS Java program to link to EDUPGM COBOL program using generated JZOS record


## Supporting files
* projects/com.ibm.cicsdev.jzostest - Eclipse OSGi plugin project 
* projects/com.ibm.cicsdev.jzostest.cicsbundle - Eclipse OSGi plugin project 
* src/Cobol/EDUPGM.cbl - Sample CICS COBOL application that demonstrates the different data types that can be used in COBOL.
* src/Cobol/EDUCPY.cbl - Copybook that describes the record structure of the CICS COMMAREA for EDUPGM



## Pre-reqs

* CICS TS V4.2 or later
* Java SE 1.7 or later on the z/OS system
* Java SE 1.7 or later on the workstation
* Eclipse with WebSphere Developer Tools and CICS Explorer SDK installed


## Configuration

The sample project can be imported into Eclipse and deployed into a CICS OSGi JVM server as follows:


1. Import the Eclipse projects from the project folder into CICS Explorer using File -> Import -> General -> Existing projects into workspace. 
2. Change the name of the JVMSERVER in the .osgibundle file from DFHJVMS to the name of the JVMSERVER resource defined in CICS. 
3. Export the bundle project to zFS by selecting 'Export Bundle project to z/OS Unix File System' from the contxt menu.
4. Create a cics bundle defintion, setting the bundle directory attribute to the zFS location you just exported to, and install it. 




## Reference

* More information about using this sample can be found in the following CICS developer center [tutorial](https://developer.ibm.com/cics/2016/05/12/java-cics-using-ibmjzos/)