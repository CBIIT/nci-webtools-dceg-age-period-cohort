/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

import gov.nih.nci.queue.model.QueueModel;
import gov.nih.nci.queue.model.ResponseModel;
import gov.nih.nci.queue.utils.PropertiesUtil;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import gov.nih.nci.queue.utils.QueueProducerUtil;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Yutao
 */
@WebServlet(name = "FileQueueServlet", urlPatterns = {"/queue"})
@MultipartConfig
public class FileQueueServlet extends HttpServlet {

    private final static Logger LOGGER = Logger.getLogger(FileQueueServlet.class.getCanonicalName());

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        final String repositoryPath = PropertiesUtil.getProperty("gov.nih.nci.queue.repository.dir");
        // Create path components to save the file
        final String emailAddress = request.getParameter("emailAddress");
        final String inputFileId = request.getParameter("inputFileId");

        // Send to Queue
        QueueModel qm = new QueueModel();
        qm.setFileName(inputFileId);
        qm.setPath(repositoryPath);
        qm.setEmail(emailAddress);
        new QueueProducerUtil().sendToQueue(qm);
        LOGGER.log(Level.INFO, "File <{0}> has been queued. ", new Object[]{inputFileId});

        // Create objects for JSON response.                
        ResponseModel rm = new ResponseModel();

        // all good. Prepare the json output.
        rm.setStatus("pass");
        rm.setEmailAddress(emailAddress);
        rm.setMessage("Congratulations! Your file has been added into queue successfully! We will send you an email notification to " + emailAddress + " once the file is processed!");

        // Set response type to json
        response.setContentType("application/json");
        // Get writer object.
        PrintWriter writer = response.getWriter();
        // Send the response.
        ObjectMapper jsonMapper = new ObjectMapper();
        LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(rm)});
        writer.print(jsonMapper.writeValueAsString(rm));
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        resp.getWriter().println("This is a Service Provider.");
    }
}
