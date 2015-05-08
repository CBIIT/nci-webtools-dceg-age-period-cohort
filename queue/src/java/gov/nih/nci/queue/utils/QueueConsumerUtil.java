/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

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

/**
 *
 * @author Yutao
 */
public class QueueConsumerUtil implements ExceptionListener {

    private final static Logger LOGGER = Logger.getLogger(QueueConsumerUtil.class.getCanonicalName());

    private final String QUEUE_LINK;
    private final String QUEUE_NAME;

    public QueueConsumerUtil(String queueId) {
        QUEUE_NAME = queueId.trim();
        QUEUE_LINK = PropertiesUtil.getProperty("queue.remote.url").trim();
    }

    public String cousume() {

        String text = null;
        

        try {
            // Create a ConnectionFactory
            ActiveMQConnectionFactory connectionFactory = new ActiveMQConnectionFactory(QUEUE_LINK);

            // Create a Connection
            Connection connection = connectionFactory.createConnection();
            connection.start();

            connection.setExceptionListener(new QueueConsumerUtil(QUEUE_NAME));

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
                text = textMessage.getText();
                LOGGER.log(Level.INFO, "Received: {0}", text);
            }

            // close all.
            consumer.close();
            session.close();
            connection.close();

        } catch (JMSException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
        }

        return text;
    }

    @Override
    public synchronized void onException(JMSException e) {
        LOGGER.log(Level.SEVERE, e.getMessage());
    }

}
