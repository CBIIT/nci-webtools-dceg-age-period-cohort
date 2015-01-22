/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

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
import java.util.ArrayList;
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

        // Get property values.        
        // SOCcer related.
        final Double estimatedThreshhold = Double.valueOf(PropertiesUtil.getProperty("gov.nih.nci.soccer.computing.time.threshhold"));
        // FileUpload Settings.
        final String repositoryPath = PropertiesUtil.getProperty("gov.nih.nci.queue.repository.dir");
        final long fileSizeMax = Long.valueOf(PropertiesUtil.getProperty("gov.nih.nci.queue.filesize.max"));
        LOGGER.log(Level.INFO, "repository.dir: {0}, filesize.max: {1}, time.threshhold: {2}",
                new Object[]{repositoryPath, fileSizeMax, estimatedThreshhold});

        // Check that we have a file upload request
        try (PrintWriter writer = response.getWriter()) {
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

                // Create an object for JSON response.   
                ResponseModel rm = new ResponseModel();

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
                            List<String> validationErrors = InputFileValidator.validateFile(inputFile);

                            if (validationErrors == null) { // Pass validation
                                // check estimatedProcessingTime.
                                Double estimatedTime = Math.round(SoccerServiceHelper.getEstimatedProcessingTime(absoluteInputFileName) * 100.0) / 100.0;
                                if (estimatedTime > estimatedThreshhold) { // STATUS: QUEUE (Ask client for email)
                                    // Construct Response String in JSON format.
//                                    obj.put("status", "queue");
                                    rm.setStatus("queue");
                                    rm.setMessage("Your file has been uploaded successfully and is ready for processing. While the estimated processing time is "
                                            + estimatedTime
                                            + "minutes, we would like you to provide us with your email address, and once we finish processing your file, we will let you know instantly via email.");
                                } else { // STATUS: PASS
                                    // all good. Process the output and Go to result page directly.
                                    rm.setStatus("pass");
                                    rm.setMessage("Your file has been uploaded and processed successfully. (Processing time: " + estimatedTime + ")");

                                    // Process the input file and generate output result file.
                                    LOGGER.log(Level.INFO, "Start processing input file <{0}>.", new Object[]{absoluteInputFileName});
                                    String outputFileId = UniqueIdUtil.getOutputUniqueID();
                                    String absoluteOutputFileName = repositoryPath + File.separator + outputFileId;
                                    SoccerServiceHelper.ProcessingFile(absoluteInputFileName, absoluteOutputFileName);
                                    LOGGER.log(Level.INFO, "Finished. Output file <{0}> has been generated.", new Object[]{absoluteInputFileName});
                                    rm.setOutputFileUrl("resultpage?fileid=" + outputFileId);
                                }
                            } else {  // STATUS: FAIL // Did not pass validation. 
                                // Construct Response String in JSON format.
                                rm.setStatus("fail");
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
                } catch (FileUploadException fue) {
                    LOGGER.log(Level.SEVERE, "FileUploadException. Error: {0}", new Object[]{fue.getMessage()});
                    rm.setStatus("fail");
                    rm.setMessage("FileUploadException: " + fue.getMessage());
                } catch (FileNotFoundException fne) {
                    LOGGER.log(Level.SEVERE, "Problems during file upload. Error: {0}", new Object[]{fne.getMessage()});
                    rm.setStatus("fail");
                    rm.setMessage("FileNotFoundException: " + fne.getMessage());
                } catch (Exception e) {
                    LOGGER.log(Level.SEVERE, "Problems during file write. Error: {0}", new Object[]{e.getMessage()});
                    rm.setStatus("fail");
                    rm.setMessage("Exception: " + e.getMessage());
                }
                
                // Set response type to json
                response.setContentType("application/json");
                // Send the response.
                ObjectMapper jsonMapper = new ObjectMapper();
                LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(rm)});
                writer.print(jsonMapper.writeValueAsString(rm));

            } else { // The request is NOT actually a file upload request
                writer.print("You hit the wrong file upload page. The request is NOT actually a file upload request.");
            }
        }
    }
}
