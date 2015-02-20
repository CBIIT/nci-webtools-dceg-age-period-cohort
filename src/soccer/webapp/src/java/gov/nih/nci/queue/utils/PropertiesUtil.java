/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.Set;

/**
 * Singleton class to access properties files anywhere in project’s classpath.
 */
public class PropertiesUtil {

    private final static String propertiesFileName = "applicationQueue.properties";
    private static Properties props;

    static {
        try {
            PropertiesUtil util = new PropertiesUtil();
            props = util.getPropertiesFromClasspath(propertiesFileName);
        } catch (FileNotFoundException fnfe) {
            System.out.println(fnfe.getMessage());
        } catch (IOException ioe) {
            System.out.println(ioe.getMessage());
        }
    }

    // private constructor
    private PropertiesUtil() {
    }

    public static String getProperty(String key) {
        return props.getProperty(key);
    }

    public static Set<Object> getkeys() {
        return props.keySet();
    }

    public static Properties getProperties() {
        return props;
    }
    
    /**
     * loads properties file from classpath
     *
     * @param propFileName
     * @return Properties
     * @throws IOException
     */
    private Properties getPropertiesFromClasspath(String propFileName)
            throws IOException {
        Properties localProps = new Properties();
        try (InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream(propFileName)) {
            if (inputStream == null) {
                throw new FileNotFoundException("property file '" + propFileName
                        + "' not found in the classpath");
            }
            localProps.load(inputStream);
        }
        return localProps;
    }
}
