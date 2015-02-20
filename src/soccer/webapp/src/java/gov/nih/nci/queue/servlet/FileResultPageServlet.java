/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.servlet;

import gov.nih.nci.queue.utils.PropertiesUtil;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author Yutao
 *
 * This page is just a placeholder, will be replaced by another dynamic page.
 */
@WebServlet(name = "FileResultPageServlet", urlPatterns = {"/resultpage"})
public class FileResultPageServlet extends HttpServlet {

    private final static Logger LOGGER = Logger.getLogger(FileResultPageServlet.class.getCanonicalName());

    // <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on the + sign on the left to edit the code.">
    /**
     * Handles the HTTP <code>GET</code> method.
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // Get Property value.
        String path = PropertiesUtil.getProperty("gov.nih.nci.queue.repository.dir");
        LOGGER.log(Level.INFO, "Get information from properties file: gov.nih.cit.soccer.input.dir={0}", path);

        // Query request to ger fileid        
        final String fileId = request.getParameter("fileid");
        LOGGER.log(Level.INFO, "File ID = {0}", new Object[]{fileId});
        String absoluteFileName = path + File.separator + fileId;
        
        // Set response content type.
        response.setContentType("text/html;charset=UTF-8");
        // Get response writer
        PrintWriter writer = response.getWriter();
        writer.println("<html><head><title>Message Queue System Prototype - Output</title><link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css\"></head><body>");
        writer.println("<div class=\"container\"><h1>Message Queue System Prototype - Result</h1><div class='alert alert-success' role=\"alert\">This is an automatically generated page. - Under Construction.</div><blockquote>Your file has been processed!</blockquote><pre> ");
        try (InputStream in = new BufferedInputStream(new FileInputStream(absoluteFileName))) {
            int ch; 
            while ((ch = in.read()) != -1) {
                writer.print((char) ch);
            }
            writer.println("</pre><hr/><a href=\"files/" + fileId + "\">Click here to download output file</a>");
            writer.println("<br><a href=\"index.html\">Home</a></div></body></html>");
        } finally {
            writer.close();
        } 
    }

}
