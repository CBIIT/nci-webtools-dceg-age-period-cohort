/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

import gov.nih.cit.soccer.input.SOCcerException;
import gov.nih.nci.queue.model.ResponseModel;
import gov.nih.nci.queue.utils.PropertiesUtil;
import gov.nih.nci.soccer.SoccerServiceHelper;
import gov.nih.nci.queue.utils.UniqueIdUtil;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import org.codehaus.jackson.map.ObjectMapper;

@WebServlet(name = "fileCalculateServlet", urlPatterns = {"/calc"})
@MultipartConfig
public class FileCalculateServlet extends HttpServlet {

    private final static Logger LOGGER = Logger.getLogger(FileCalculateServlet.class.getCanonicalName());

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // Set response type to json        
        PrintWriter writer = response.getWriter();

        // Get parameters.
        final String repositoryPath = PropertiesUtil.getProperty("gov.nih.nci.queue.repository.dir").trim();
        final String strOutputDir = PropertiesUtil.getProperty("gov.nih.cit.soccer.output.dir").trim();
        System.setProperty("gov.nih.cit.soccer.output.dir", strOutputDir);
        System.setProperty("gov.nih.cit.soccer.wordnet.dir", PropertiesUtil.getProperty("gov.nih.cit.soccer.wordnet.dir").trim());
        if (System.getProperty("gov.nih.cit.soccer.output.dir", "na").equals("na")
                || System.getProperty("gov.nih.cit.soccer.wordnet.dir", "na").equals("na")) {
            LOGGER.log(Level.SEVERE, "Internal Error: Cannot find system variables.");
            writer.print("Internal Error: Cannot find system variables. Please contact Technical Support.");
        }
        // get input fileId
        final String inputFileId = request.getParameter("inputFileId");
        String absoluteInputFileName = repositoryPath + File.separator + inputFileId;
        LOGGER.log(Level.INFO, "AbsoluteInputFileName: {0}, output.dir: {1}", new Object[]{absoluteInputFileName, strOutputDir});

        // Process the file. 
        LOGGER.log(Level.INFO, "Start processing input file <{0}>.", new Object[]{absoluteInputFileName});
        // all good. Prepare the json output.
        ResponseModel rm = new ResponseModel();
        String outputFileId = new UniqueIdUtil(inputFileId).getOutputUniqueID();
        String absoluteOutputFileName = repositoryPath + File.separator + outputFileId;
        try {
            SoccerServiceHelper ssh = new SoccerServiceHelper(strOutputDir);
            ssh.ProcessingFile(new File(absoluteInputFileName), new File(absoluteOutputFileName));
            // all good. Prepare the json output.
            LOGGER.log(Level.INFO, "The output file <{0}> has been generated successfully.", absoluteOutputFileName);
            rm.setStatus("pass");
            rm.setOutputFileId(outputFileId);

        } catch (IOException | SOCcerException e) {
            rm.setStatus("fail");
            rm.setErrorMessage(e.getMessage());
            LOGGER.log(Level.SEVERE, "Failed to generate output file <{0}>. Error Message: {1}", new Object[]{absoluteOutputFileName, e.getMessage()});
        }

        // Set response type to json
        response.setContentType("application/json");
        // Send the response.
        ObjectMapper jsonMapper = new ObjectMapper();
        LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(rm)});
        writer.print(jsonMapper.writeValueAsString(rm));
    }
}
