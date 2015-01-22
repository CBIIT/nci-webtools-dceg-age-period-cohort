package gov.nih.nci.queue.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;

public class FileUtil {

    public FileUtil() {
    }

    /*
     *  example, return : 1407947293000
     */
    public long getLongLastModified(String _filePath) {
        File file = new File(_filePath);
        return file.lastModified();
    }

    /*
     * example, return : 08/13/2014 12:28:13
     */
    public String getFormattedLastModified(String _filePath) {
        File file = new File(_filePath);
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        return sdf.format(file.lastModified());
    }

    /*
     * rename/move file to another name and dest.
     */
    public boolean moveFileTo(String absoluteFileName, String absoluteDestFile) {
        try {

            File afile = new File(absoluteFileName);
            return afile.renameTo(new File(absoluteDestFile));
        } catch (Exception e) {
        }

        return false;
    }

    /*
     * Copy a file to another dest file.
     */
    public boolean copyFileTo(String absoluteFileName, String absoluteDestFile) {
        InputStream inStream;
        OutputStream outStream;

        try {
            inStream = new FileInputStream(new File(absoluteFileName));
            outStream = new FileOutputStream(new File(absoluteDestFile));

            byte[] buffer = new byte[1024];
            int length;
            //copy the file content in bytes 
            while ((length = inStream.read(buffer)) > 0) {
                outStream.write(buffer, 0, length);
            }

            inStream.close();
            outStream.close();
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    /*
     * Write InputStream to a file.
     */
    public void writeInputStreamToFile(InputStream ins, String absoluteFileName) throws IOException, FileNotFoundException {

        try (OutputStream out = new FileOutputStream(new File(absoluteFileName))) {

            int read;
            final byte[] bytes = new byte[1024];

            while ((read = ins.read(bytes)) != -1) {
                out.write(bytes, 0, read);
            }
            out.close();
        }
    }

}
