/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2016 All Rights Reserved                       */       
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */ 
package com.ibm.cicsdev.jzostest;

import com.ibm.cics.server.Program;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.bean.CommareaWrapper; // Import of JZOS wrapper record

public class JZOSprog {

	public static void main(String[] args)         
	{
		String proglink = "EDUPGM";	   // Name of COBOL program

		Task t = Task.getTask();
		t.out.println(" - Starting JOZSprog"); 

		CommareaWrapper cw = new CommareaWrapper(); // Instantiate new JZOS wrapper for commarea
		// Use the wrapper to set the input data on the commarea
		cw.setBinaryDigit(1);
		cw.setCharacterString("hello");
		cw.setNumericString(1234);
		cw.setPackedDigit(123456789);
		cw.setSignedPacked(-100);
		cw.setBool("1");

		// Create program and set the name of the COBOL program to link to
		Program p = new Program();
		p.setName(proglink);
		
		try {
			p.link(cw.getByteBuffer()); // Get a byte arrage from the wrapper and pass this in as the commarea
			
			t.out.println("Returned from "+ proglink + " with rc("+cw.getResultCode()+") " + cw.getResultText());

		} catch (Exception e) { // Generic catch for brevity, something went wrong with the link
			t.out.println("Error from "+ proglink + "with: " + e.getMessage());
			e.printStackTrace();
		} 

	}
}





