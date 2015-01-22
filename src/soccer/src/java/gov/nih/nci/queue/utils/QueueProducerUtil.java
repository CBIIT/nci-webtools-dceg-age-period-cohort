/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import gov.nih.nci.queue.model.QueueModel;
import java.io.IOException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.DeliveryMode;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Yutao
 */
public class QueueProducerUtil {

    private final static Logger LOGGER = Logger.getLogger(QueueProducerUtil.class.getCanonicalName());

    private Context context;
    private Queue fileQueue;
    private ConnectionFactory connectionFactory;

    /*
     * Add message into Queue.
    */
    public void sendToQueue(QueueModel qm) {
        try {
            context = new InitialContext();            
            connectionFactory = (ConnectionFactory) context.lookup("openejb:Resource/MyJmsConnectionFactory");
            fileQueue = (Queue) context.lookup("openejb:Resource/FooQueue");

            // Connect to Queue.
            Connection connection = connectionFactory.createConnection();
            connection.start();

            // Create a Session and set auto acknowledge.
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Create a MessageProducer from the Session to the Topic or Queue
            MessageProducer producer = session.createProducer(fileQueue);
            producer.setDeliveryMode(DeliveryMode.PERSISTENT);

            // Add timestamp and create a json message
            ObjectMapper jsonMapper = new ObjectMapper();
            qm.setTimeStamp(new Date() + "");
            LOGGER.log(Level.INFO, "Response: {0}", new Object[]{jsonMapper.writeValueAsString(qm)});
            TextMessage message = session.createTextMessage(jsonMapper.writeValueAsString(qm));

            // Tell the producer to send the message
            producer.send(message);

            // close all.
            producer.close();
            session.close();
            connection.close();

        } catch (NamingException ne) {
            LOGGER.log(Level.SEVERE, "Problems during Lookup JNDI. Error: {0}", ne.getMessage());
        } catch (JMSException jmse) {
            LOGGER.log(Level.SEVERE, "Problems during Queue. Error: {0}", jmse.getMessage());
        } catch (JsonGenerationException | JsonMappingException e) {
            LOGGER.log(Level.SEVERE, "Problems during Parsing JSON. Error: {0}", e.getMessage());
        } catch (IOException ie) {
            LOGGER.log(Level.SEVERE, "Problems during Parsing JSON. Error: {0}", ie.getMessage());
        } 
    }
}
