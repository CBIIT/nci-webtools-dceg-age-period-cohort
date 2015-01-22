/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.soccer;

import gov.nih.cit.soccer.Soccer;
import gov.nih.cit.soccer.input.InputFormatException;
import gov.nih.nci.queue.utils.FileUtil;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Yutao
 */
public class SoccerServiceHelper {

    private final static Logger LOGGER = Logger.getLogger(SoccerServiceHelper.class.getCanonicalName());

    public static double getEstimatedProcessingTime(final String absoluteFileName) {
        Soccer s = new Soccer();
        File file = new File(absoluteFileName);

        double estimatedTime;
        try {
            estimatedTime = s.getEstimatedTime(file);

            s.getEstimatedTime(file);   // currently this has to be a file (will soon accept a filename)
            s.codeFile(file);                  // this can be a filename or a file
        } catch (IOException ie) {
            LOGGER.log(Level.SEVERE, "Caught IOException! Exit -1.");
            return -1;
        } catch (InputFormatException ife) {
            LOGGER.log(Level.SEVERE, "Caught InputFormatException! Exit -1.");
            return -1;
        }

        return estimatedTime;
    }

    /*
     * Algorithom to process the file
     * OR
     * Invoke shell command(s) to process the input file.
     */
    public static void ProcessingFile(String absoluteInputFileName, String absoluteOutputFileName) {
        //
        // TODO: This is just a simulation for now.
        // Invoke algorithm or invoke command line.
        //
        new FileUtil().copyFileTo(absoluteInputFileName, absoluteOutputFileName);
    }
}
