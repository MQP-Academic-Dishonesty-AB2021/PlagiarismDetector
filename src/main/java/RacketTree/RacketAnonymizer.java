package RacketTree;


import javax.swing.*;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

// TODO: Make this actually thread safe
public class RacketAnonymizer {
    public static String racketPath = "racket";
    private static String anonymizerPath = "./Anonymizer.rkt";
    private ArrayList<RacketSubmission> submissions;

    Path final_path;

    public static File anonymizeFile(File fin) throws IOException, InterruptedException {
        Path tempDir = Files.createTempDirectory(fin.getName());
        ProcessBuilder racketProcessBuilder = new ProcessBuilder(racketPath, anonymizerPath, "-o",
                tempDir.toFile().getAbsolutePath(), "-i", fin.getAbsolutePath());
        Process racketProcess = racketProcessBuilder.start();
        racketProcess.waitFor();
        return tempDir.toFile();
    };

    public RacketAnonymizer(File fin) throws IOException, InterruptedException {
        Path tempDir = Files.createTempDirectory(fin.getName());
        ProcessBuilder racketProcessBuilder = new ProcessBuilder(racketPath, anonymizerPath, "-o",
                tempDir.toFile().getAbsolutePath(), "-i", fin.getAbsolutePath());
        Process racketProcess = racketProcessBuilder.start();
        racketProcess.waitFor();
        this.final_path = tempDir;
        this.submissions = new ArrayList<RacketSubmission>();
        for (File submission : this.final_path.resolve("regular").toFile().listFiles()) {
            submissions.add(new RacketSubmission(this, submission.getName()));
        }
    };

    public File getRegular() {
        return this.final_path.resolve("regular").toFile();
    }

    public File getAnonymized() {
        return this.final_path.resolve("anonymized").toFile();
    }

    public ArrayList<RacketSubmission> getSubmissions() {
        return this.submissions;
    }
}
