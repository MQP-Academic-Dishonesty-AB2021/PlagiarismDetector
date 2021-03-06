package Comparison;

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
import RacketTree.RacketAnonymizer;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import RacketTree.RacketExpression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import RacketTree.RacketSubmission;

import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;

public abstract class Comparison {
	private static Logger logger = LoggerFactory.getLogger(Comparison.class);

	public enum Method {
		TreeSimilarity,
		Checksims
	};

	protected ConcurrentHashMap<ComparisonPair, Double> values;
	protected ArrayList<RacketSubmission> fileList;
	private final Object listLock = new Object();
	public static int numThreads = 4;

	private RacketAnonymizer anonymizer;

	Comparison() {
		this.values = new ConcurrentHashMap<ComparisonPair, Double>();
		this.fileList = new ArrayList<>();
	}

	public int addSubmission(RacketSubmission submission) {
		int index = -1;
		synchronized (this.listLock) {
			index = this.fileList.size();
			this.fileList.add(submission);
		}
		return index;
	}

	static Comparison generateComparison(Method method) {
		switch (method) {
			case TreeSimilarity:
				return new TreeSimilarityComparison();
			case Checksims:
			default:
				return new ChecksimsComparison();
		}
	}

	/**
	 * Get the value of a comparison between two projects
	 * 
	 * @param pair The pair containing the projects
	 * @return the value of the comparison
	 */
	public double getValue(ComparisonPair pair) {
		return this.values.get(pair);
	}

	/**
	 *
	 * @return Get an ordered list of pairs of projects ordered by most likely to
	 *         least likely to have cheated
	 */
	public ArrayList<ImmutablePair<ComparisonPair, Double>> getOrderedList() {
		ArrayList<ImmutablePair<ComparisonPair, Double>> list = new ArrayList<>();
		for (Map.Entry<ComparisonPair, Double> value : values.entrySet()) {
			list.add(new ImmutablePair<>(value.getKey(), value.getValue()));
		}
		list.sort((val1, val2) -> Double.compare(val2.getValue(), val1.getValue()));
		return list;
	}

	/**
	 * Get the value of a comparison between two projects
	 * 
	 * @param base     The base project
	 * @param compared The project the base was compared too
	 * @return the value of the comparison
	 */
	public double getValue(String base, String compared) {
		return this.values.get(
				new ComparisonPair(
						new RacketSubmission(this.anonymizer, base),
						new RacketSubmission(this.anonymizer, compared)
				));
	}


	/**
	 * Write the results of the comparison to a csv file
	 * 
	 * @param filename The file name of the csv
	 * @throws IOException Error writing to file
	 */
	public void toCSV(String filename) throws IOException {
		File csvFile = new File(filename + ".csv");
		if (!csvFile.exists()) {
			csvFile.createNewFile();
		}
		FileWriter csv = new FileWriter(filename + ".csv");
		csv.write("BaseFilename,ComparedFilename,Value\n");
		this.values.forEach((pair, val) -> {
			try {
				csv.write(pair.getBaseFilename());
				csv.write(",");
				csv.write(pair.getComparedFilename());
				csv.write(",");
				csv.write(Double.toString(val));
				csv.write("\n");
			}
			catch (IOException e) {
				System.out.println("bruh wtf");
			}
		});
		csv.close();
	}

	public ArrayList<RacketSubmission> getSubmissions() {
		return this.fileList;
	}
}
