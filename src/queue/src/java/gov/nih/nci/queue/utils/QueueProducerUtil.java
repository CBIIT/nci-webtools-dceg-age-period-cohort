/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

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

/**
 *
 * @author Yutao
 */
public class QueueProducerUtil {

    private final static Logger LOGGER = Logger.getLogger(QueueProducerUtil.class.getCanonicalName());

    private Context context;
    private Queue msgQueue;
    private ConnectionFactory connectionFactory;

    private final String CONNECTION_FACTORY_JNDI;
    private final String QUEUE_JNDI;

    public QueueProducerUtil(String queueId) {
        CONNECTION_FACTORY_JNDI = new StringBuilder("openejb:Resource/")
                .append(PropertiesUtil.getProperty("queue.connectionfactory").trim())
                .toString();
        QUEUE_JNDI = new StringBuilder("openejb:Resource/")
                .append(queueId.trim())
                .toString();
    }

    /*
     * Add message into Queue.
     */
    public void produce(String jsonString) {
        LOGGER.log(Level.INFO, "CONNECTION_FACTORY: {0} QUEUE: {1}", new Object[]{CONNECTION_FACTORY_JNDI, QUEUE_JNDI});
        try {
            context = new InitialContext();
            connectionFactory = (ConnectionFactory) context.lookup(CONNECTION_FACTORY_JNDI);
            msgQueue = (Queue) context.lookup(QUEUE_JNDI);

            // Connect to Queue.
            Connection connection = connectionFactory.createConnection();
            connection.start();

            // Create a Session and set auto acknowledge.
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Create a MessageProducer from the Session to the Topic or Queue
            MessageProducer producer = session.createProducer(msgQueue);
            producer.setDeliveryMode(DeliveryMode.PERSISTENT);

            // create a message            
            TextMessage message = session.createTextMessage(jsonString);

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
        }
    }

}
