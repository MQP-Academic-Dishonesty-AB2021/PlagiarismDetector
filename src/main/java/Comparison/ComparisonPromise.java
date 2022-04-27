package Comparison;

import RacketTree.RacketAnonymizer;
import RacketTree.RacketSubmission;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class ComparisonPromise extends Thread {
    private static Logger logger = LoggerFactory.getLogger(ComparisonPromise.class);
    private final File originalDirectory;
    private RacketAnonymizer anonymizer;
    private boolean anonymized;
    private DoubleProperty numFinished;
    private DoubleProperty numExpected;
    private final Comparison comparison;

    @Override
    public void run() {
        try {
            this.anonymizer = new RacketAnonymizer(this.originalDirectory);
        } catch (IOException | InterruptedException e) {
            logger.error(e.getMessage());
            logger.error(e.getStackTrace().toString());
            return;
        }
        this.anonymized = true;
        ArrayList<RacketSubmission> submissions = anonymizer.getSubmissions();
        if (submissions == null) {
            logger.error("Invalid File Path: " + this.originalDirectory.getAbsolutePath());
            return;
        }
        this.numExpected.setValue(submissions.size());
        this.numFinished.setValue(0);
        ForkJoinPool pool = new ForkJoinPool(Comparison.numThreads);
        pool.submit(() -> submissions.stream().parallel().forEach(
                (submission) -> {
                    if (this.isInterrupted()) {
                        return;
                    }
                    try {
                        this.comparison.addSubmission(submission);
                    }
                    catch (Error e) {
                        logger.error(e.getMessage());
                    }
                    synchronized (this.comparison) {
                        this.numFinished.setValue(this.numFinished.get() + 1);
                    }
                }
        ));
        try {
            pool.shutdown();
            pool.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        this.numFinished.setValue(this.numExpected.getValue());
    }

    public ComparisonPromise(String directory, Comparison.Method method) {
        this.originalDirectory = new File(directory);
        this.comparison = Comparison.generateComparison(method);
        this.numFinished = new SimpleDoubleProperty(0);
//        this.numFinished.addListener((obs, oldVal, newVal) -> {
//            System.out.print(oldVal);
//            System.out.print(" ");
//            System.out.println(newVal);
//        });
        this.numExpected = new SimpleDoubleProperty(1);
    }

    public Comparison getComparison() throws InterruptedException {
        super.join();
        return this.comparison;
    }

    public boolean isAnonymized() {
        return this.anonymized;
    }

    public DoubleProperty getNumFinished() { return this.numFinished; }
    public DoubleProperty getNumExpected() { return this.numExpected; }

    public NumberBinding getPercentCompletion() {
        return Bindings.divide(this.numFinished, this.numExpected);
    }
}
