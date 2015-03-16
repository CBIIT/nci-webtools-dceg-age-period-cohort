/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import gov.nih.nci.queue.model.QueueModel;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import javax.jms.TextMessage;
import org.apache.activemq.ActiveMQConnectionFactory;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Yutao
 */
public class QueueConsumerUtil implements ExceptionListener {

    private final static Logger LOGGER = Logger.getLogger(QueueConsumerUtil.class.getCanonicalName());

    private String QUEUE_LINK;
    private String QUEUE_NAME;

    public QueueModel cousume() {

        QueueModel qm = null;

        QUEUE_LINK = PropertiesUtil.getProperty("queue.remote.url");
        QUEUE_NAME = PropertiesUtil.getProperty("queue.remote.name");
        try {
            // Create a ConnectionFactory
            ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory(QUEUE_LINK);

            // Create a Connection
            Connection connection = connectionFactory.createConnection();
            connection.start();

            connection.setExceptionListener(new QueueConsumerUtil());

            // Create a Session
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Create the destination (Topic or Queue)
            Destination destination = session.createQueue(QUEUE_NAME);

            // Create a MessageConsumer from the Session to the Topic or Queue
            MessageConsumer consumer = session.createConsumer(destination);

            // Wait for a message
            Message message = consumer.receive(1000);

            if (message instanceof TextMessage) {
                TextMessage textMessage = (TextMessage) message;
                String text = textMessage.getText();
                LOGGER.log(Level.INFO, "Received: {0}", text);
                
                // convert it to Object.
                qm = convertJsonToObject(text);
            }

            // close all.
            consumer.close();
            session.close();
            connection.close();

        } catch (JMSException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
        }

        return qm;
    }

    @Override
    public synchronized void onException(JMSException ex) {
        System.out.println("JMS Exception occured.  Shutting down client.");
    }

    /*
     * convert json string to QueueModel Object.
     */
    public QueueModel convertJsonToObject(String jsonString) {
        ObjectMapper mapper = new ObjectMapper();
        QueueModel qm = null;

        try {
            qm = mapper.readValue(jsonString, QueueModel.class);
        } catch (JsonGenerationException | JsonMappingException e) {
            LOGGER.log(Level.SEVERE, "Caught JSON Exception. Error: {0}", e.getMessage());
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Caught JsonGenerationException. Error: {0}", e.getMessage());
        }
        return qm;
    }

}
