/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.soccer;

import gov.nih.cit.soccer.Soccer;
import gov.nih.cit.soccer.input.InputFormatException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This Class will invoke existing codes to validate File uploaded to make sure
 * the file can be processed by 3rd-party computing software.
 *
 * @author wangy21
 */
public class InputFileValidator {

    private final static Logger LOGGER = Logger.getLogger(InputFileValidator.class.getCanonicalName());
    private final Soccer soc;

    public InputFileValidator(Soccer soc) {
        this.soc = soc;
    }

    /*
     * Validate the file uploaded.
     * Check format etc.
     * @return String : null means no validation error. 
     * 
     */
    public List<String> validateFile(File _file) {
        // Invoke Soccer object to validate file.         
        ArrayList<String> validationErrors = null;

        try {
            soc.validateFile(_file);
        } catch (InputFormatException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            String[] strArray = e.getMessage().split("\n");
            validationErrors = new ArrayList<>(Arrays.asList(strArray));
            return validationErrors;
        } catch (IOException e) {
            validationErrors = new ArrayList<>();
            validationErrors.add("File is not readable - IOException:\n" + e.getMessage());
        }

        return validationErrors;
    }

}
