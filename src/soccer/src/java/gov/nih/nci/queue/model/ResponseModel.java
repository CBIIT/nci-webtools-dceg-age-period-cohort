/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package gov.nih.nci.queue.model;

import java.io.Serializable;
import java.util.List;

public class ResponseModel implements Serializable {

    private String status;
    private String message;
    private String fileName;
    private String fileType;
    private String fileSize;
    private String inputFileId;
    private String inputFileUrl;
    private String outputFileUrl;
    private String repositoryPath;    
    private String emailAddress;     
    private List<String> details;

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }

    public String getFileSize() {
        return fileSize;
    }

    public void setFileSize(String fileSize) {
        this.fileSize = fileSize;
    }

    public String getInputFileId() {
        return inputFileId;
    }

    public void setInputFileId(String inputFileId) {
        this.inputFileId = inputFileId;
    }

    public String getInputFileUrl() {
        return inputFileUrl;
    }

    public void setInputFileUrl(String inputFileUrl) {
        this.inputFileUrl = inputFileUrl;
    }

    public String getOutputFileUrl() {
        return outputFileUrl;
    }

    public void setOutputFileUrl(String outputFileUrl) {
        this.outputFileUrl = outputFileUrl;
    }



    public String getRepositoryPath() {
        return repositoryPath;
    }

    public void setRepositoryPath(String repositoryPath) {
        this.repositoryPath = repositoryPath;
    }

    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(String emailAddress) {
        this.emailAddress = emailAddress;
    }

    public List<String> getDetails() {
        return details;
    }

    public void setDetails(List<String> details) {
        this.details = details;
    }

    
    
}
