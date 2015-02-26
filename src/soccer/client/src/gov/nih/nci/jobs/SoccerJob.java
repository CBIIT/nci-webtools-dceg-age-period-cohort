/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.jobs;

import gov.nih.nci.queue.model.QueueModel;
import gov.nih.nci.queue.utils.MailUtil;
import gov.nih.nci.queue.utils.QueueConsumerUtil;
import gov.nih.nci.queue.utils.UniqueIdUtil;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import org.apache.log4j.Logger;
import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

/**
 *
 * @author Yutao
 */
public class SoccerJob implements Job {

    private static final Logger LOGGER = Logger.getLogger(SoccerJob.class.getName());
    private static final QueueConsumerUtil qcu = new QueueConsumerUtil();
    private final String outputFilePre = "SoccerResults-"; // Soccer App will add this prefix. 
    private final String PORT = "8080";

    @Override
    public void execute(JobExecutionContext context)
            throws JobExecutionException {

        // Get data passed in.
        JobDataMap data = context.getJobDetail().getJobDataMap();
        String cmd = (String) data.getString("cmd");

        /* Processing the message */
        QueueModel qm = qcu.cousume();

        if (qm != null) {
            String path = qm.getPath();
            String fileName = qm.getFileName();
            String email = qm.getEmail();
            String timeStamp = qm.getTimeStamp();
            String outputDir = qm.getOutputDir();
            LOGGER.info(new StringBuilder("Parsed message: ")
                    .append("\r\nPath: ").append(path)
                    .append("\r\nFileName: ").append(fileName)
                    .append("\r\nEmail: ").append(email)
                    .append("\r\nOutputDir: ").append(outputDir)
                    .append("\r\nTimeStamp: ").append(timeStamp)
                    .toString());

            String outputFileId = UniqueIdUtil.getOutputUniqueID();

            // Run command.  
            String fullCmd = new StringBuilder(cmd)
                    .append(" ")
                    .append(path).append(File.separator).append(fileName)
                    .toString();
            LOGGER.info("Full Command: " + fullCmd);
            if (ExeCommand(fullCmd)) {
                // rename to the outputFileId.
                String outputFile = outputDir + File.separator + outputFilePre + fileName;
                String outputFile2 = outputDir + File.separator + outputFileId;
                renameFileName(outputFile, outputFile2);

                // Send email. 
                String from = "SOCcer <do.not.reply@mail.nih.gov>";
                boolean isMailSent = new MailUtil().mailTo(from, email, composeMailTitle(), composeMailBody(timeStamp, outputFileId));
                if (isMailSent) {
                    LOGGER.info("Message has been sent to " + email + " successfully.");
                } else {
                    LOGGER.error("Failed to send email. Please check email settings or security settings.");
                }
            }
        } else {
            LOGGER.info("."); // indicating the applicaiton is running.
        }
    }

    /*
     * Execute shell command.
     */
    private boolean ExeCommand(String cmd) {
        boolean bRet = false;
        try {
            Process p = Runtime.getRuntime().exec(cmd);

            BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }

            // check result.
            if (p.waitFor() == 0) {
                bRet = true;
            }
        } catch (IOException | InterruptedException ie) {
            LOGGER.error("Caught IOException and InterruptedException. {0}\r\n" + ie.getMessage());
        }

        return bRet;
    }

    /*
     * renamte filename.
     */
    public boolean renameFileName(String oldName, String newName) {
        System.out.println(new StringBuilder("Rename ")
                .append(oldName)
                .append(" to ")
                .append(newName).toString());

        File fileOld = new File(oldName);
        File fileNew = new File(newName);
        if (fileOld.exists()) {
            fileOld.renameTo(fileNew);
            return true;
        } else {
            return false;
        }
    }

    // Compose Mail Title.
    private String composeMailTitle() {
        return "Your request has been processed";
    }

    // Compose Mail Body.
    private String composeMailBody(String timeStamp, String outputFileId) {
        // get hostname automatically.
        String hostname = System.getenv("HOSTNAME"); // Linux
        if (hostname == null || hostname.length() < 1) {
            hostname = System.getenv("COMPUTERNAME"); // Windows. 
        }

        return new StringBuilder("( Please do not reply to this email. If you need assistance, please contact ncicbiit@mail.nih.gov )\r\n\r\n")
                .append("The file you uploaded on ")
                .append(timeStamp)
                .append(" has been processed. ")
                .append("\r\nYou can view the result page at: http://")
                .append(hostname)
                .append(":")
                .append(PORT)
                .append("/soccer/soccerouput.html?fileid=")
                .append(outputFileId)
                .append("\r\n\r\n - SOCcer Team")
                .toString();
    }

}
