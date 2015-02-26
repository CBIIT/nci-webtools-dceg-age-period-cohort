/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import java.util.Properties;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

/**
 *
 * @author wangy21
 */
public class MailUtil {

    public boolean mailTo(String from, String destEmail, String _title, String _message) {
        // Recipient's email ID needs to be mentioned.
        String to = destEmail; 
        String host = "mailfwd.nih.gov";           

        // Get system properties
        Properties properties = System.getProperties();

        // Setup mail server
        properties.setProperty("mail.smtp.host", host);

        // Get the default Session object.
        Session session = Session.getDefaultInstance(properties);

        try {
            // Create a default MimeMessage object.
            MimeMessage message = new MimeMessage(session);

            // Set From: header field of the header.
            message.setFrom(new InternetAddress(from));

            // Set To: header field of the header.
            message.addRecipient(Message.RecipientType.TO,
                    new InternetAddress(to));

            // Set Subject: header field
            message.setSubject(_title);

            // Send the actual HTML message, as big as you like
            // message.setContent(_message, "text/html");
            message.setText(_message);

            // Send message
            Transport.send(message);
        } catch (MessagingException mex) {            
            return false;
        }
        return true;
    }
}
