package Comparison;

import Checksims.algorithm.AlgorithmRegistry;
import Checksims.algorithm.AlgorithmResults;
import Checksims.algorithm.SimilarityDetector;
import Checksims.submission.NoMatchingFilesException;
import Checksims.submission.Submission;
import Checksims.token.TokenType;
import Checksims.token.tokenizer.Tokenizer;
import Checksims.util.threading.SimilarityDetectionWorker;
import RacketTree.RacketSubmission;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class ChecksimsComparison extends Comparison {
    public static Logger logger = LoggerFactory.getLogger(ChecksimsComparison.class);
    private ConcurrentHashMap<RacketSubmission, Submission> assignmentMap;
    private SimilarityDetector algorithm;
    private TokenType tokenization;

    ChecksimsComparison() {
        this.assignmentMap = new ConcurrentHashMap<>();
        this.algorithm = AlgorithmRegistry.getInstance().getDefaultImplementation();
//        this.algorithm = Checksims.algorithm.smithwaterman.SmithWaterman.getInstance();
        this.tokenization = this.algorithm.getDefaultTokenType();
    }

    @Override
    public int addSubmission(RacketSubmission submission) {
        Submission csSubmission = null;
        try {
            csSubmission = Submission.submissionFromDir(submission.regular(),
                    "*.rkt",
                    Tokenizer.getTokenizer(tokenization),
                    true);
        } catch (NoMatchingFilesException | IOException e) {
            logger.error(e.getMessage());
            return -1;
        }
//        if (csSubmission.getNumTokens() == 0) { return -1; }
        int currentIndex = super.addSubmission(submission);
        this.assignmentMap.put(submission, csSubmission);
        for (int i = 0; i < currentIndex; i++) {
            RacketSubmission otherSubmission = this.fileList.get(i);
            Submission otherCsSubmission = this.assignmentMap.get(otherSubmission);
            try {
                AlgorithmResults results = new SimilarityDetectionWorker(this.algorithm,
                        new ImmutablePair<>(csSubmission, otherCsSubmission)).call();
                this.values.put(new ComparisonPair(submission,
                        otherSubmission), results.percentMatchedA());
                this.values.put(new ComparisonPair(otherSubmission,
                        submission), results.percentMatchedB());
            }
            catch (Exception e) {
                logger.error(e.getMessage());
            }
        }

        return currentIndex;
    }
}
