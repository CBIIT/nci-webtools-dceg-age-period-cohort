/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author wangy21
 */
public class MetadataFileUtil {

    private final static Logger LOGGER = Logger.getLogger(MetadataFileUtil.class.getCanonicalName());
    private final String METADATA_FILE_EXT = ".json";
    private final String metadataFileId;
    private final String metadataFileDir;

    // we will use fileOutputId as the Id to locate the metadata file. 
    // 
    public MetadataFileUtil(String fileOutputId, String outputDir) {
        metadataFileId = fileOutputId;
        metadataFileDir = outputDir;
    }

    // create the metadata file which stores queue message.
    public void generateMetadataFile(String jsonString) {
        try {
            String metadataFilePath = metadataFileDir + File.separator + metadataFileId + METADATA_FILE_EXT;
            File file = new File(metadataFilePath);
            // if file doesnt exists, then create it
            if (!file.exists()) {
                file.createNewFile();
            }

            FileWriter fw = new FileWriter(file.getAbsoluteFile());
            BufferedWriter bw = new BufferedWriter(fw);
            bw.write(jsonString);
            bw.close();

            LOGGER.log(Level.INFO, "Generated {0} successfully.", metadataFilePath);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Failed to create metadata file. {0}", e.getMessage());
        }
    }
}
