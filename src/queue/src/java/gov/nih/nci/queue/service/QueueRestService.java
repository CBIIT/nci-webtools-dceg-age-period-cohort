/*
 * Curl commands (Windows): 

 Consume: 
 curl -XGET -H "Accept:application/json" http://localhost:8080/queue/restapi/soccerqueue

 Produce: 
 curl -X POST -H "Content-Type:application/json" -d "{\"QueueRoot\":{\"email\":\"yutao.wang@essexmanagement.com\",\"fileName\":\"filename2.csv\"}}" http://localhost:8080/queue/restapi/soccerqueue

 */
package gov.nih.nci.queue.service;

import gov.nih.nci.queue.utils.QueueConsumerUtil;
import gov.nih.nci.queue.utils.QueueProducerUtil;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

/**
 * @author Yutao
 */
@Path("/restapi")
public class QueueRestService {

    private final static Logger LOGGER = Logger.getLogger(QueueRestService.class.getCanonicalName());

    @POST
    @Path("/{queueid}")
    @Consumes({"application/json"})
    public Response produce(@PathParam("queueid") String _queueId, String _jsonString) {
        LOGGER.log(Level.INFO, "produce has been invoked - {0}", _queueId);
        if (_queueId != null && _jsonString != null) {
            if(_jsonString.length() < 100000) // temprarily. 
                new QueueProducerUtil(_queueId).produce(_jsonString);
            else 
                new QueueProducerUtil(_queueId).produce("{\"Error\": \"length is over 100000\"}");
        }
        return Response.ok("OK - Message added into " + _queueId).build();
    }

    @GET
    @Path("/{queueid}")
    @Produces({"application/json"})
    public String consume(@PathParam("queueid") String _queueId) {
        LOGGER.log(Level.INFO, "consume has been invoked. {0}", _queueId);
        return new QueueConsumerUtil(_queueId).cousume();
    }
}
