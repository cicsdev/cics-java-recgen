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
* /etc/META-INF/MANIFEST.MF - Sample OSGi bundle manifest that can be used when creating an OSGi plug-in project
* /lib/EDUPGM.JAR - A pre-built JAR containing the JZOS generated record JZOSCommareaWrapper
* src/Cobol/EDUPGM.cbl - Sample CICS COBOL application that demonstrates the different data types that can be used in COBOL.
* src/Cobol/EDUCPY.cbl - Copybook that describes the record structure of the CICS COMMAREA for EDUPGM



## Pre-reqs

* CICS TS V4.2 or later
* Java SE 1.7 or later on the z/OS system
* Java SE 1.7 or later on the workstation
* Eclipse with WebSphere Developer Tools and CICS Explorer SDK installed


## Configuration

The sample class JZOSprog can be added to a OSGi plug-in project and deployed into a CICS OSGi JVM server along with the CommareaWrapper class


## Reference

* More information about using this sample can be found at the following [web site] (https://developer.ibm.com/cics/2016/05/12/java-cics-using-ibmjzos/)