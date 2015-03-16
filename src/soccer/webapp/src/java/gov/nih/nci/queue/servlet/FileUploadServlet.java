/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

import gov.nih.nci.queue.model.ResponseModel;
import gov.nih.nci.queue.utils.MetadataFileUtil;
import gov.nih.nci.queue.utils.PropertiesUtil;
import gov.nih.nci.queue.utils.UniqueIdUtil;
import gov.nih.nci.soccer.InputFileValidator;
import gov.nih.nci.soccer.SoccerServiceHelper;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.annotation.WebServlet;
import org.apache.commons.fileupload.FileItem;
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
        final String strOutputDir = PropertiesUtil.getProperty("gov.nih.cit.soccer.output.dir").trim();
        final long fileSizeMax = 10000000000L; // 10G
        LOGGER.log(Level.INFO, "repository.dir: {0}, filesize.max: {1}, time.threshhold: {2}",
                new Object[]{repositoryPath, fileSizeMax, estimatedThreshhold});

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

                        String inputFileId = new UniqueIdUtil(fileName).getInputUniqueID();
                        rm.setInputFileId(inputFileId);
                        String absoluteInputFileName = repositoryPath + File.separator + inputFileId;
                        rm.setRepositoryPath(repositoryPath);

                        // Write file to the destination folder.
                        File inputFile = new File(absoluteInputFileName);
                        item.write(inputFile);

                        // Validation.                        
                        InputFileValidator validator = new InputFileValidator();
                        List<String> validationErrors = validator.validateFile(inputFile);

                        if (validationErrors == null) { // Pass validation
                            // check estimatedProcessingTime.
                            SoccerServiceHelper soccerHelper = new SoccerServiceHelper(strOutputDir);
                            Double estimatedTime = soccerHelper.getEstimatedTime(absoluteInputFileName);
                            rm.setEstimatedTime(String.valueOf(estimatedTime));
                            if (estimatedTime > estimatedThreshhold) { // STATUS: QUEUE (Ask client for email)
                                // Construct Response String in JSON format.
                                rm.setStatus("queue");  
                            } else { // STATUS: PASS (Ask client to confirm calculate)
                                // all good. Process the output and Go to result page directly.
                                rm.setStatus("pass");                                  
                            }
                        } else {  // STATUS: FAIL // Did not pass validation. 
                            // Construct Response String in JSON format.
                            rm.setStatus("invalid");
                            rm.setDetails(validationErrors);
                        }
                    } else { // TODO: Handel Form Fields such as SOC_SYSTEM.

                        /* // Place Holder
                         String name = item.getFieldName();
                         String value = item.getString();
                         */
                    } // End of isFormField
                }
            } catch (Exception e) {  
                LOGGER.log(Level.SEVERE, "FileUploadException or FileNotFoundException. Error Message: {0}", new Object[]{e.getMessage()});
                rm.setStatus("fail");
                rm.setErrorMessage("Oops! We met with problems when uploading your file. Error Message: " + e.getMessage());
            }

            // Send the response.
            ObjectMapper jsonMapper = new ObjectMapper();
            LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(rm)});
            // Generate metadata file
            new MetadataFileUtil(rm.getInputFileId(), repositoryPath).generateMetadataFile(jsonMapper.writeValueAsString(rm));
            // Responde to the client. 
            writer.print(jsonMapper.writeValueAsString(rm));

        } else { // The request is NOT actually a file upload request
            writer.print("You hit the wrong file upload page. The request is NOT actually a file upload request.");
        }
    }
}

