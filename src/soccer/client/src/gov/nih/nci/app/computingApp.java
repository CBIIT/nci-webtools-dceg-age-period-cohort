/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.app;

import gov.nih.nci.jobs.*;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SimpleTrigger;
import org.quartz.impl.StdSchedulerFactory;

/**
 *
 * @author wangy21
 */
public class computingApp {

    private static final Logger LOGGER = Logger.getLogger(computingApp.class.getCanonicalName());
    
    /**
     * @param args the command line arguments
     */
    // Set interval in Sec, default value is 5 seconds.
    private static final String propertiesFileName = "config.properties";

    public static void main(String args[]) {
        Properties prop = new Properties();
        try {
            // Read properties
            prop.load(new FileInputStream(propertiesFileName));
            String fullCmd = prop.getProperty("full_cmd");
            String intervalInSec = prop.getProperty("interval_in_sec");
            
            LOGGER.info(new StringBuilder("Request to run <")
                    .append(fullCmd).append("> every <").append(intervalInSec)
                    .append("> seconds.").toString());

            /*
             *Schedule Soccer Job
             */
            // Define job and pass data into job.
            JobDetail job = new JobDetail();
            job.setName("SoccerJob");
            JobDataMap dataMap = new JobDataMap();
            dataMap.put("cmd", fullCmd);
            job.setJobDataMap(dataMap);
            job.setJobClass(SoccerJob.class);

            //configure the scheduler time
            SimpleTrigger trigger = new SimpleTrigger();
            trigger.setName("SimpleTrigger");
            trigger.setStartTime(new Date(System.currentTimeMillis() + 1000));
            trigger.setRepeatCount(SimpleTrigger.REPEAT_INDEFINITELY);
            trigger.setRepeatInterval(1000 * Integer.parseInt(intervalInSec.trim()));

            //schedule it
            Scheduler scheduler = new StdSchedulerFactory().getScheduler();
            scheduler.start();
            scheduler.scheduleJob(job, trigger);
        } catch (IOException | SchedulerException e) {
            System.err.println("Caught Exception. Exit -1.\r\n" + e.getMessage());
            System.exit(-1);
        }
    }

}
