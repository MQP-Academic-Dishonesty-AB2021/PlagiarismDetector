package Comparison;

import RacketTree.RacketTree;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import RacketTree.InvalidFormatException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import RacketTree.RacketSubmission;

public class TreeSimilarityComparison extends Comparison {
    private static Logger logger = LoggerFactory.getLogger(TreeSimilarityComparison.class);

    private ConcurrentHashMap<String, RacketTree> assignmentMap;

    TreeSimilarityComparison() {
        this.assignmentMap = new ConcurrentHashMap<>();
    }

    @Override
    public int addSubmission(RacketSubmission submission) {
        RacketTree assignmentTree = null;
        try {
            assignmentTree = new RacketTree(submission.anonymized().getPath());
        }
        catch (IOException | InterruptedException e) {
            logger.error(e.getMessage());
        }
        catch (InvalidFormatException e) {
            logger.error(e.getMessage());
            logger.debug("Errored File:");
            try {
                logger.debug(new String(Files.readAllBytes(submission.anonymized().toPath())));
            }
            catch (Exception e2) {}
            return -1;
        }
        if (assignmentTree == null) { return -1; }
        // skip non rkt projects in the directory
        if (assignmentTree.numLeaves == 0) {
            return -1;
        }
        assignmentMap.put(submission.getName(), assignmentTree);
        this.fileList.add(submission.getName());
        int currentIndex = super.addSubmission(submission);
        for (int i = 0; i < currentIndex; i++) {
            String otherName = this.fileList.get(i);
            RacketTree otherTree = assignmentMap.get(otherName);
            this.values.put(new ComparisonPair(submission.getName(), otherName),
                    assignmentTree.similarityValue(otherTree));
            this.values.put(new ComparisonPair(otherName, submission.getName()),
                    otherTree.similarityValue(assignmentTree));
        }
        return currentIndex;
    }
}
