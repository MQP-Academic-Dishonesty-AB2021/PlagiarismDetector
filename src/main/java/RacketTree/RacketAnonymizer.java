package RacketTree;


import javax.swing.*;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;

// TODO: Make this actually thread safe
public class RacketAnonymizer {
    public static String racketPath = "racket";
    private static String anonymizerPath = "./Anonymizer.rkt";

    public static File anonymizeFile(File fin) throws IOException, InterruptedException {
        Path tempDir = Files.createTempDirectory(fin.getName());
        ProcessBuilder racketProcessBuilder = new ProcessBuilder(racketPath, anonymizerPath, "-o",
                tempDir.toFile().getAbsolutePath(), "-i", fin.getAbsolutePath());
        Process racketProcess = racketProcessBuilder.start();
        racketProcess.waitFor();
        return tempDir.toFile();
    };
}
