package Comparison;

import RacketTree.RacketAnonymizer;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.DoubleBinding;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.atomic.AtomicInteger;

public class ComparisonPromise extends Thread {
    private static Logger logger = LoggerFactory.getLogger(ComparisonPromise.class);
    private final File originalDirectory;
    private File anonymizedDirectory;
    private boolean anonymized;
    private DoubleProperty numFinished;
    private DoubleProperty numExpected;
    private final Comparison comparison;

    @Override
    public void run() {
        try {
            this.anonymizedDirectory = RacketAnonymizer.anonymizeFile(this.originalDirectory);
        } catch (IOException | InterruptedException e) {
            logger.error(e.getMessage());
            logger.error(e.getStackTrace().toString());
            return;
        }
        this.anonymized = true;
        File[] submissions = anonymizedDirectory.listFiles();
        if (submissions == null) {
            logger.error("Invalid File Path: " + this.originalDirectory.getAbsolutePath());
            return;
        }
        this.numExpected.setValue(submissions.length);
        ForkJoinPool pool = new ForkJoinPool(Comparison.numThreads);
        for (File submission : submissions) {
            if (this.isInterrupted()) {
                return;
            }
            if (submission.isFile() &&
                !submission.getName().substring(submission.getName().length() - 4).equals(".rkt")) {
                synchronized (this.comparison) {
                    this.numFinished.setValue(this.numFinished.get() + 1);
                }
                continue;
            }
            this.comparison.addSubmission(submission);
            synchronized (this.comparison) {
                this.numFinished.setValue(this.numFinished.get() + 1);
            }
        }
    }

    public ComparisonPromise(String directory, Comparison.Method method) {
        this.originalDirectory = new File(directory);
        this.comparison = Comparison.generateComparison(method);
        this.numFinished = new SimpleDoubleProperty(0);
        this.numExpected = new SimpleDoubleProperty(1);
    }

    public Comparison getComparison() throws InterruptedException {
        super.join();
        return this.comparison;
    }

    public boolean isAnonymized() {
        return this.anonymized;
    }

    public NumberBinding getPercentCompletion() {
        return Bindings.divide(this.numFinished, this.numExpected);
    }
}
