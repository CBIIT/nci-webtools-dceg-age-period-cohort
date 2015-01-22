/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.soccer;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * This Class will invoke existing codes to validate File uploaded
 * to make sure the file can be processed by 3rd-party computing 
 * software. 
 * 
 * @author wangy21
 */
public class InputFileValidator {
    
    /*
     * Validate the file uploaded.
     * Check format etc.
     * @return String : null means no validation error. 
     * 
     */
    public static List<String> validateFile(File fileUploaded) {
        // TODO: Simulate the Validation for now. > 10KB
        if (fileUploaded.length() > 10000) {
            return null;
        } else {
            ArrayList<String> validationErrors = new ArrayList<>();
            validationErrors.add("File is undersized (<10KB).");
            validationErrors.add("Validation Error 2. <-- should return from the validatio function.");
            validationErrors.add("Validation Error 3. <-- should return from the validatio function.");
                                
            return validationErrors;
        }
    }
}
