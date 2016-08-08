# cics-java-jzosprog
Sample CICS Java program to use a JZOS generated record to build a COMMAREA for linking to the CICS COBOL EDUPGM sample


## Java package com.ibm.cicsdev.bean
* JZOSCommareaWrapper.java - Generated JZOS record from EDUCPY copybook

## Java package com.ibm.cicsdev.jzostest
* JZOSprog.java- JCICS CICS Java program to link to EDUPGM COBOL program using generated JZOS record


## Supporting files
* MANIFEST.MF - Sample OSGi bundle manifest that can be used when creating an OSGi plug-in project
* EDUPGM.JAR - A pre-built JAR containing the JZOS generated record JZOSCommareaWrapper


#Pre-reqs

    CICS TS V4.2 or later
    Eclipse with WebSphere Developer Tools and CICS Explorer SDK installed

#Configuration

The sample class JZOSprog can be added to a OSGi plug-in project and deployed into a CICS OSGi JVM server along with the CommareaWrapper class


#Reference

More information about using this sample can be found at the following web site
https://developer.ibm.com/cics/2016/05/12/java-cics-using-ibmjzos/

To download the COBOL EDUPGM sample see this Git repository https://github.com/cicsdev/cics-cobol-edupgm