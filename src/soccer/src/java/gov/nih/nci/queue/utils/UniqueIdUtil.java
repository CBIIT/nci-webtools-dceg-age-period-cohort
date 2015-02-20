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

    /*
     * Generate a unique Id for the file uploaded. 
     * put prefix "i" to identify this is a input file
     * to be processed.
    */
    public static String getInputUniqueID() {
        return "i" + UUID.randomUUID();
    }
    
    /*
     * Generate a unique Id for the file uploaded. 
     * put prefix "o" to identify this is a output file.     
    */
    public static String getOutputUniqueID() {
        return "o" + UUID.randomUUID();
    }

}
