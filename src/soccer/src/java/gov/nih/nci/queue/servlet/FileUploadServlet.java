/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

import gov.nih.cit.soccer.Soccer;
import gov.nih.nci.queue.model.ResponseModel;
import gov.nih.nci.queue.utils.PropertiesUtil;
import gov.nih.nci.soccer.SoccerServiceHelper;
import gov.nih.nci.queue.utils.UniqueIdUtil;
import gov.nih.nci.soccer.InputFileValidator;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.annotation.WebServlet;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.codehaus.jackson.map.ObjectMapper;

@WebServlet(name = "fileUploadServlet", urlPatterns = {"/upload"})
@MultipartConfig
public class FileUploadServlet extends HttpServlet {

    private final static Logger LOGGER = Logger.getLogger(FileUploadServlet.class.getCanonicalName());

    /**
     * *************************************************
     * URL: /upload doPost(): upload the files and other parameters
     *
     * @param request
     * @param response
     * @throws javax.servlet.ServletException
     * @throws java.io.IOException
     * **************************************************
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // Create an object for JSON response.   
        ResponseModel rm = new ResponseModel();
        // Set response type to json
        response.setContentType("application/json");
        PrintWriter writer = response.getWriter();

        // Get property values.        
        // SOCcer related.
        final Double estimatedThreshhold = Double.valueOf(PropertiesUtil.getProperty("gov.nih.nci.soccer.computing.time.threshhold").trim());
        // FileUpload Settings.
        final String repositoryPath = PropertiesUtil.getProperty("gov.nih.nci.queue.repository.dir");
//        final long fileSizeMax = Long.valueOf(PropertiesUtil.getProperty("gov.nih.nci.queue.filesize.max"));
        final long fileSizeMax = 10000000000L; // 10G
        final String strOutputDir = PropertiesUtil.getProperty("gov.nih.cit.soccer.output.dir").trim();
        System.setProperty("gov.nih.cit.soccer.output.dir", strOutputDir);
        System.setProperty("gov.nih.cit.soccer.wordnet.dir", PropertiesUtil.getProperty("gov.nih.cit.soccer.wordnet.dir").trim());
        LOGGER.log(Level.INFO, "repository.dir: {0}, filesize.max: {1}, time.threshhold: {2}, output.dir: {3}",
                new Object[]{repositoryPath, fileSizeMax, estimatedThreshhold, strOutputDir});
        if (System.getProperty("gov.nih.cit.soccer.output.dir", "na").equals("na")
                || System.getProperty("gov.nih.cit.soccer.wordnet.dir", "na").equals("na")) {
            LOGGER.log(Level.SEVERE, "Internal Error: Cannot find system variables.");
            writer.print("Internal Error: Cannot find system variables. Please contact Technical Support.");
        }

        // Check that we have a file upload request
        // Check that we have a file upload request
        boolean isMultipart = ServletFileUpload.isMultipartContent(request);

        // Ensuring that the request is actually a file upload request.
        if (isMultipart) {
            // Create a factory for disk-based file items
            DiskFileItemFactory factory = new DiskFileItemFactory();

            // Set factory constraints
//                factory.setSizeThreshold(yourMaxMemorySize);
            // Configure a repository 
            factory.setRepository(new File(repositoryPath));

            // Create a new file upload handler
            ServletFileUpload upload = new ServletFileUpload(factory);
            upload.setFileSizeMax(fileSizeMax);

            try {
                // Parse the request
                List<FileItem> items = upload.parseRequest(request);

                // Process the uploaded items
                Iterator<FileItem> iter = items.iterator();
                while (iter.hasNext()) {
                    FileItem item = iter.next();

                    if (!item.isFormField()) { // Handle file field.                              
                        String fileName = item.getName();
                        rm.setFileName(fileName);
                        String contentType = item.getContentType();
                        rm.setFileType(contentType);
                        long sizeInBytes = item.getSize();
                        rm.setFileSize(String.valueOf(sizeInBytes));

                        String inputFileId = UniqueIdUtil.getInputUniqueID();
                        rm.setInputFileId(inputFileId);
                        String absoluteInputFileName = repositoryPath + File.separator + inputFileId;
                        rm.setRepositoryPath(repositoryPath);

                        // Write file to the destination folder.
                        File inputFile = new File(absoluteInputFileName);
                        item.write(inputFile);

                        // Validation.
                        Soccer soccer = new Soccer();
                        InputFileValidator validator = new InputFileValidator(soccer);
                        List<String> validationErrors = validator.validateFile(inputFile);

                        if (validationErrors == null) { // Pass validation
                            // check estimatedProcessingTime.
                            Double estimatedTime = Math.round(soccer.getEstimatedTime(absoluteInputFileName) * 100.0) / 100.0;
                            if (estimatedTime > estimatedThreshhold) { // STATUS: QUEUE (Ask client for email)
                                // Construct Response String in JSON format.
//                                    obj.put("status", "queue");
                                rm.setStatus("queue");
                                rm.setMessage("Your file has been uploaded successfully and is ready for processing. While the estimated processing time is "
                                        + estimatedTime
                                        + "seconds, we would like you to provide us with your email address, and once we finish processing your file, we will let you know instantly via email.");
                            } else { // STATUS: PASS
                                // all good. Process the output and Go to result page directly.
                                rm.setStatus("pass");
                                rm.setMessage("Your file has been uploaded and processed successfully. (Processing time: " + estimatedTime + " seconds)");

                                // Process the input file and generate output result file.
                                LOGGER.log(Level.INFO, "Start processing input file <{0}>.", new Object[]{absoluteInputFileName});
                                String outputFileId = UniqueIdUtil.getOutputUniqueID();
                                String absoluteOutputFileName = repositoryPath + File.separator + outputFileId;
                                SoccerServiceHelper ssh = new SoccerServiceHelper(soccer, strOutputDir);
                                if (ssh.ProcessingFile(new File(absoluteInputFileName), new File(absoluteOutputFileName))) {
                                    LOGGER.log(Level.INFO, "The output file <{0}> has been generated successfully.", absoluteOutputFileName);
                                    rm.setOutputFileUrl(outputFileId); 
                                } else {
                                    LOGGER.log(Level.SEVERE, "Failed to generate output file <{0}>.", absoluteOutputFileName);
                                }
                            }
                        } else {  // STATUS: FAIL // Did not pass validation. 
                            // Construct Response String in JSON format.
                            rm.setStatus("invalid");
                            rm.setMessage("Your file did not pass validation.");
                            rm.setDetails(validationErrors);
                        }
                    } else { // TODO: Handel Form Fields such as SOC_SYSTEM.

                        /* // Place Holder
                         String name = item.getFieldName();
                         String value = item.getString();
                         */
                    } // End of isFormField
                }
            } catch (FileUploadException | FileNotFoundException  e) {                
                LOGGER.log(Level.SEVERE, "FileUploadException or FileNotFoundException. Error Message: {0}", new Object[]{e.getMessage()});
                rm.setStatus("fail");
                rm.setMessage("Oops! We met with problems when uploading your file. Error Message: " + e.getMessage());
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Exception. Error: {0}", new Object[]{e.getMessage()});
                rm.setStatus("fail");
                rm.setMessage("Oops! File has been uploaded successfully, however, we met with problems when processing your file. Error Message: " + e.getMessage());
            }

            // Send the response.
            ObjectMapper jsonMapper = new ObjectMapper();
            LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(rm)});
            writer.print(jsonMapper.writeValueAsString(rm));

        } else { // The request is NOT actually a file upload request
            writer.print("You hit the wrong file upload page. The request is NOT actually a file upload request.");
        }
    }
}

