package ComparisonRunner;

import Checksims.ChecksimsCommandLine;
import Checksims.ChecksimsConfig;
import Checksims.algorithm.AlgorithmResults;
import Checksims.algorithm.AlgorithmRunner;
import Checksims.algorithm.preprocessor.PreprocessSubmissions;
import Checksims.algorithm.preprocessor.SubmissionPreprocessor;
import Checksims.algorithm.similaritymatrix.MatrixEntry;
import Checksims.algorithm.similaritymatrix.SimilarityMatrix;
import Checksims.submission.Submission;
import Checksims.util.PairGenerator;
import Checksims.util.threading.ParallelAlgorithm;
import RacketTree.RacketTree;
import RacketTree.InvalidFormatException;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.Pair;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

public class Comparison {
    private HashMap<ComparisonPair, Double> values;
    private ArrayList<String> fileList;

    public Comparison(String directory) {
        this.generateOurComparison(directory);
    }

    public Comparison(String directory, String method) {
        switch(method) {
            case "TreeSimilarity":
                this.generateOurComparison(directory);
                break;
            case "Checksims":
            default:
                this.generateChecksimsComparison(directory);
        }
    }

    private void generateOurComparison(String assignment) {
        File dir = new File(assignment);
        HashMap<String, RacketTree> assignmentMap = new HashMap<String, RacketTree>();
        this.values = new HashMap<>();
        try {
            this.fileList = new ArrayList<>();
            for (File submission : dir.listFiles()) {
                // Skip any extraneous files
                if (submission.isFile() &&
                        !submission.getName().substring(submission.getName().length() - 4).equals(".rkt")) {
                    continue;
                }
                RacketTree assignmentTree = new RacketTree(submission.getPath());
                // skip non rkt projects in the directory
                if (assignmentTree.numLeaves != 0) {
                    assignmentMap.put(submission.getName(), assignmentTree);
                    this.fileList.add(submission.getName());
                }
            }
            for (int i = 0; i < fileList.size(); i++) {
                String filenameI = fileList.get(i);
                RacketTree treeI = assignmentMap.get(filenameI);
                for (int j = 0; j < fileList.size(); j++) {
                    if (i == j) { continue; }
                    String filenameJ = fileList.get(j);
                    RacketTree treeJ = assignmentMap.get(filenameJ);
                    double simValI = treeI.similarityValue(treeJ);
                    this.values.put(new ComparisonPair(filenameI, filenameJ), simValI);
                }
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        catch (NullPointerException e) {
            e.printStackTrace();
        }
        catch (InvalidFormatException e) {

        }
    }

    private void generateChecksimsComparison(String assignment) {
        String[] args = {"-s", assignment, "-o", "csv"};
        this.values = new HashMap<>();
        this.fileList = new ArrayList<>();
        try {
            // All in this scope are copy and pasted from checksims with unused items removed (logging)
            ChecksimsConfig config = ChecksimsCommandLine.generateFinalConfig(args);
            int threads = config.getNumThreads();
            ParallelAlgorithm.setThreadCount(threads);

            System.setProperty("java.util.concurrent.ForkJoinPool.common.parallelism", "" + threads);

            ImmutableSet<Submission> submissions = config.getSubmissions();

            ImmutableSet<Submission> archiveSubmissions = config.getArchiveSubmissions();

            // Apply all preprocessors
            for(SubmissionPreprocessor p : config.getPreprocessors()) {
                submissions = ImmutableSet.copyOf(PreprocessSubmissions.process(p, submissions));

                if(!archiveSubmissions.isEmpty()) {
                    archiveSubmissions = ImmutableSet.copyOf(PreprocessSubmissions.process(p, archiveSubmissions));
                }
            }

            // Apply algorithm to submissions
            Set<Pair<Submission, Submission>> allPairs = PairGenerator.generatePairsWithArchive(submissions,
                    archiveSubmissions);
            Set<AlgorithmResults> results = AlgorithmRunner.runAlgorithm(allPairs, config.getAlgorithm());
            SimilarityMatrix resultsMatrix = SimilarityMatrix.generateMatrix(submissions, archiveSubmissions, results);

            for (int i = 0; i < resultsMatrix.getXSubmissions().size(); i++) {
                Submission xsub = resultsMatrix.getXSubmission(i);
                this.fileList.add(xsub.getName());
                for (int j = 0; j < resultsMatrix.getYSubmissions().size(); j++) {
                    if (i == j) { continue; }
                    MatrixEntry result = resultsMatrix.getEntryFor(i, j);
                    this.values.put(new ComparisonPair(result.getBase().getName(), result.getComparedTo().getName()),
                            result.getSimilarityPercent());
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void toCSV(String filename) throws IOException {
        File csvFile = new File(filename + ".csv");
        if (!csvFile.exists()) { csvFile.createNewFile(); }
        FileWriter csv = new FileWriter(filename + ".csv");
        for (int i = 0; i < fileList.size(); i++) {
            csv.write("," + this.fileList.get(i));
        }
        csv.write("\n");
        for (int i = 0; i < fileList.size(); i++) {
            String filenameI = this.fileList.get(i);
            csv.write(filenameI + ",");
            for (int j = 0; j < fileList.size(); j++) {
                String filenameJ = this.fileList.get(j);
                if (i == j) {
                    if (j != this.fileList.size() - 1) {
                        csv.write(",");
                    }
                    continue;
                }
                csv.write(Double.toString(this.values.get(new ComparisonPair(filenameI, filenameJ))));
                if (j != this.fileList.size() - 1) {
                    csv.write(",");
                }
            }
            csv.write("\n");
        }
        csv.close();
    }
}
