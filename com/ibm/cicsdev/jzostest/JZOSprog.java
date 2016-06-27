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


import java.text.MessageFormat;

import com.ibm.cics.server.InvalidProgramIdException;
import com.ibm.cics.server.InvalidRequestException;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.bean.JZOSCommareaWrapper;

public class JZOSprog {

	public static String proglink = "EDUPGM";  // Linked to COBOL program

	public static void main(String[] args)          
	{

		// Get details about our current CICS task
		Task task = Task.getTask();
		task.out.println(" - Starting JOZSprog"); 

		// Wrapper classes for input and output commareas
		JZOSCommareaWrapper cwIn = null;
		JZOSCommareaWrapper cwOut = null;

		// Set the input data fields

		Short binarydigit = 1;
		String charstring = "hello";
		Short numericstring = 1234;
		Integer packeddigit = 123456789;
		Integer signedpackeddigit = -100;
		String bool = "1";

		cwIn = new JZOSCommareaWrapper();
		cwIn.setBinaryDigit(binarydigit );
		cwIn.setCharacterString(charstring );
		cwIn.setNumericString(numericstring );
		cwIn.setPackedDigit(packeddigit );
		cwIn.setSignedPacked(signedpackeddigit );
		cwIn.setBool(bool );

		// Create a reference to the CICS program
		Program prog = new Program();
		prog.setName(proglink);	

		// Create byte array for input commarea from wrapper
		byte[] commarea = cwIn.getByteBuffer();

		try {
			// Link to target CICS program
			prog.link(commarea);

			//Build output record from updated commarea
			cwOut = new JZOSCommareaWrapper(commarea);

			// Catch specific JCICS exception
		} catch (InvalidProgramIdException e) { 
			task.out.println("Unknown CICS Program from "+ proglink + "with: " + e.getMessage());

			// Catch any other exception and force a  rollback of CICS UOW
		} catch (Exception e) {
			String msg = "ERROR: Exception on link to {0} with msg({1})";
			task.out.println(MessageFormat.format(msg, proglink, e.getMessage()));
			// Rollback the CICS Task 
			try 
			{
				task.rollback();
			} catch (InvalidRequestException e1) {		
				// If this fails, then throw Runtime Exception
				throw new RuntimeException(e1);
			}
		} 

		String msg = "Returned from {0} with rc({1}) {2}";
		task.out.println(MessageFormat.format(msg, proglink,cwOut.getResultCode(), cwOut.getResultText()));

	}

}
