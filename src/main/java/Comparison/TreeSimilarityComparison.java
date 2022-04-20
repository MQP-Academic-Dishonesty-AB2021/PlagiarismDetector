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

    private ConcurrentHashMap<RacketSubmission, RacketTree> assignmentMap;

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
            try {
                logger.info("Errored File:");
                logger.info(new String(Files.readString(submission.anonymized().toPath())));
            }
            catch (Exception e2) {}
            return -1;
        }
        if (assignmentTree == null) { return -1; }
        // skip non rkt projects in the directory
        if (assignmentTree.numLeaves == 0) {
            return -1;
        }
        int currentIndex = super.addSubmission(submission);
        assignmentMap.put(submission, assignmentTree);
        for (int i = 0; i < currentIndex; i++) {
            RacketSubmission otherSubmission = this.fileList.get(i);
            RacketTree otherTree = assignmentMap.get(otherSubmission);
            this.values.put(new ComparisonPair(submission, otherSubmission),
                    assignmentTree.similarityValue(otherTree));
            this.values.put(new ComparisonPair(otherSubmission, submission),
                    otherTree.similarityValue(assignmentTree));
        }
        return currentIndex;
    }
}
