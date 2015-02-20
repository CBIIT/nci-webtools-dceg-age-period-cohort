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

    private final Soccer soc;
    private final String outputDir;
    private final String outputFilePre = "SoccerResults-";

    public SoccerServiceHelper(Soccer _soc, String outputDir) {
        this.soc = _soc;
        this.outputDir = outputDir;
    }

    /*
     * Algorithom to process the file
     * OR
     * Invoke shell command(s) to process the input file.
     */
    public boolean ProcessingFile(File _fileIn, File _fileOut) {
        boolean bRet = false;

        try {
            int numLines = getNumberLines(_fileIn);
            // If not validation error.
            System.out.println((new StringBuilder("Number of lines = ")).append(numLines).toString());
            System.out.println((new StringBuilder("Estimated time to finish = ")).append(soc.getEstimatedTime(_fileIn)).append(" sec").toString());
            RunTimer timer = new RunTimer();
            soc.codeFile(_fileIn);
            timer.stop();
            System.out.println((new StringBuilder("Elapsed time = ")).append(timer.elapsedTime()).append(" sec").toString());
            System.out.println((new StringBuilder("Average time / line = ")).append(timer.elapsedTime() / (float) numLines).append(" sec").toString());

            // Rename output file (SoccerResults-soccer_dataset0.csv) to _fileOutput.
            System.out.println("Output File: " + outputDir + File.separator + outputFilePre + _fileIn.getName());
            File fileOutput = new File(outputDir + File.separator + outputFilePre + _fileIn.getName());
            if (fileOutput.exists() && _fileOut != null) {
                System.out.println(outputFilePre + _fileIn.getName() + " -> " + _fileOut.getName());
                boolean success = fileOutput.renameTo(_fileOut);
                if (success) {
                    bRet = true;
                }
            }
        } catch (InputFormatException e) {
            LOGGER.log(Level.SEVERE, "Exception: {0}", e.getMessage());
        } catch (IOException | SOCcerException e) {
            LOGGER.log(Level.SEVERE, "Exception: {0}", e.getMessage());
        }

        return bRet;
    }

    @SuppressWarnings("empty-statement")
    private int getNumberLines(File file)
            throws IOException {
        int numLines;
        try (BufferedReader in = new BufferedReader(new FileReader(file))) {
            in.readLine();
            for (numLines = 0; in.readLine() != null; numLines++);
        }
        return numLines;
    }

}
