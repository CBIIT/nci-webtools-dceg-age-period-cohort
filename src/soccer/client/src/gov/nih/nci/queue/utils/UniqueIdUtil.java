/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import java.util.UUID;

/**
 *
 * @author Yutao
 */
public class UniqueIdUtil {

    private String fileExt;
    
    // Extract the file Extenetion always.
    public UniqueIdUtil(String fileName) {
       fileExt = ""; 
        int i = fileName.lastIndexOf('.');
        if (i > 0) {
            fileExt = fileName.substring(i);
        }
    }
    
    /*
     * Generate a unique Id for the file uploaded. 
     * put prefix "i" to identify this is a input file
     * to be processed.
    */
    public String getInputUniqueID() {
        return new StringBuilder("i").append(UUID.randomUUID()).append(fileExt).toString();
    }
    
    /*
     * Generate a unique Id for the file uploaded. 
     * put prefix "o" to identify this is a output file.     
    */
    public String getOutputUniqueID() {
        return new StringBuilder("o").append(UUID.randomUUID()).append(fileExt).toString();
    }

}
