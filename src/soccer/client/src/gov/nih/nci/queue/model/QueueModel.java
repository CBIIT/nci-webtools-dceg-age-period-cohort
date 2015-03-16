/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.model;

import java.io.Serializable;

/**
 * The unit added in queue. 
 * 
 * @author wangy21
 */
public class QueueModel implements Serializable {

    private String fileName;
    private String path;
    private String email;
    private String timeStamp;
    private String outputDir;

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getTimeStamp() {
        return timeStamp;
    }

    public void setTimeStamp(String timeStamp) {
        this.timeStamp = timeStamp;
    }

    public String getOutputDir() {
        return outputDir;
    }

    public void setOutputDir(String outputDir) {
        this.outputDir = outputDir;
    } 

    @Override
    public String toString() {
        return new StringBuilder("")
                    .append("\r\nPath: ").append(path)
                    .append("\r\nFileName: ").append(fileName)
                    .append("\r\nEmail: ").append(email)
                    .append("\r\nOutputDir: ").append(outputDir)
                    .append("\r\nTimeStamp: ").append(timeStamp)
                    .toString();
    }
    
   
}
