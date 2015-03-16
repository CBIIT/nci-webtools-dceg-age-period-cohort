/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.soccer;

import gov.nih.cit.soccer.Soccer;
import gov.nih.cit.soccer.input.InputFormatException;
import gov.nih.cit.soccer.input.SOCcerException;
import gov.nih.cit.soccer.misc.RunTimer;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Yutao
 */
public class SoccerServiceHelper {

    private final static Logger LOGGER = Logger.getLogger(SoccerServiceHelper.class.getCanonicalName());

    private final Soccer soc = new Soccer();
    private final String outputDir;
    private final String outputFilePre = "SoccerResults-";

    public SoccerServiceHelper(String outputDir) {        
        this.outputDir = outputDir;
    }

    /*
     * Algorithom to process the file
     * OR
     * Invoke shell command(s) to process the input file.
     */
    public void ProcessingFile(File _fileIn, File _fileOut) throws InputFormatException, IOException, SOCcerException   {
                
        // Process the input file and generate a new output file. 
        // By soccer, the output file name would be: SoccerResults-<input_file_name> 
        RunTimer timer = new RunTimer();
        soc.codeFile(_fileIn);
        timer.stop();

        // Rename output file (SoccerResults-soccer_dataset0.csv) to _fileOutput.
        // Removing Prefix "SoccerResults-". 
        String generatedFilePath = outputDir + File.separator + outputFilePre + _fileIn.getName();
        LOGGER.log(Level.INFO, "SOCcer ouput File: {0}", generatedFilePath);
        File fileOutput = new File(generatedFilePath);
        if (fileOutput.exists() && _fileOut != null) {           
            fileOutput.renameTo(_fileOut);
        }   
        else {
            LOGGER.log(Level.SEVERE, "{0} does not exist!", generatedFilePath);
        }
    }

    // Get number of lines of the input file.
    @SuppressWarnings("empty-statement")
    public int getNumberLines(File _file)
            throws IOException {
        int numLines;
        try (BufferedReader in = new BufferedReader(new FileReader(_file))) {
            in.readLine();
            for (numLines = 0; in.readLine() != null; numLines++);
        }
        return numLines;
    }
    
    // Get Estimate Processing Time
    public double getEstimatedTime(String _absoluteInputFileName) throws IOException {
        return Math.round(soc.getEstimatedTime(_absoluteInputFileName) * 100.0) / 100.0;
    }

}
