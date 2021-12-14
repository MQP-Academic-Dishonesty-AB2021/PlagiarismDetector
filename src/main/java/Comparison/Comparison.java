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
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import RacketTree.RacketExpression;

import java.io.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;

public class Comparison {
	private ConcurrentHashMap<ComparisonPair, Double> values;
	private ArrayList<String> fileList;
	public static int numThreads = 4;

	/**
	 * Calculate a Racket Tree comparison of all projects in a directory
	 * 
	 * @param directory The directory
	 */
	public Comparison(String directory) {
		this.generateRacketTreeComparison(directory);
	}

	/**
	 * Calculate a comparison of all projects in a directory with a given method
	 * Defaults to Checksims
	 * 
	 * @param directory The directory
	 * @param method    The method
	 */
	public Comparison(String directory, String method) {
		switch (method) {
			case "TreeSimilarity":
				this.generateRacketTreeComparison(directory);
				break;
			case "Checksims":
			default:
				this.generateChecksimsComparison(directory);
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
	 * Get the value of a comparison between two projects
	 * 
	 * @param base     The base project
	 * @param compared The project the base was compared too
	 * @return the value of the comparison
	 */
	public double getValue(String base, String compared) {
		return this.values.get(new ComparisonPair(base, compared));
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
	 * Generate a RacketTree comparison between all projects in a directory
	 * 
	 * @param assignment The directory
	 */
	private void generateRacketTreeComparison(String assignment) {
		File dir = new File(assignment);
		HashMap<String, RacketTree> assignmentMap = new HashMap<String, RacketTree>();
		this.values = new ConcurrentHashMap<>();
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

			HashMap<String, Integer> numTimes = new HashMap<>();
			for (Map.Entry<String, RacketTree> sub : assignmentMap.entrySet()) {
				for (Map.Entry<RacketExpression, ArrayList<RacketExpression>> leaf : sub.getValue().leavesHash
						.entrySet()) {
					numTimes.merge(leaf.getKey().toString(), leaf.getValue().size(), Integer::sum);
				}
			}
			ArrayList<ImmutablePair<String, Integer>> sortedNum = new ArrayList<>();
			for (Map.Entry<String, Integer> x : numTimes.entrySet()) {
				sortedNum.add(new ImmutablePair<>(x.getKey(), x.getValue()));
			}
			sortedNum.sort((x, y) -> Integer.compare(y.getValue(), x.getValue()));

			ForkJoinPool myPool = new ForkJoinPool(numThreads);
			myPool.submit(() -> fileList.parallelStream().forEach(
					(filenameI) -> {
						RacketTree treeI = assignmentMap.get(filenameI);
						for (String filenameJ : fileList) {
							if (filenameI.equals(filenameJ)) {
								continue;
							}
							RacketTree treeJ = assignmentMap.get(filenameJ);
							double simValI = treeI.similarityValue(treeJ);
							this.values.put(new ComparisonPair(filenameI, filenameJ), simValI);
						}
					})).get();
		} catch (IOException | NullPointerException | InvalidFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Perform a checksims comparison between all projects in a directory
	 * 
	 * @param assignment the directory
	 */
	private void generateChecksimsComparison(String assignment) {
		String[] args = { "-s", assignment, "-o", "csv", "-j", Integer.toString(numThreads) };
		this.values = new ConcurrentHashMap<>();
		this.fileList = new ArrayList<>();
		try {
			// All in this scope are copy and pasted from checksims with unused items
			// removed (logging)
			ChecksimsConfig config = ChecksimsCommandLine.generateFinalConfig(args);
			int threads = config.getNumThreads();
			ParallelAlgorithm.setThreadCount(threads);

			System.setProperty("java.util.concurrent.ForkJoinPool.common.parallelism", "" + threads);

			ImmutableSet<Submission> submissions = config.getSubmissions();

			ImmutableSet<Submission> archiveSubmissions = config.getArchiveSubmissions();

			// Apply all preprocessors
			for (SubmissionPreprocessor p : config.getPreprocessors()) {
				submissions = ImmutableSet.copyOf(PreprocessSubmissions.process(p, submissions));

				if (!archiveSubmissions.isEmpty()) {
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
					if (i == j) {
						continue;
					}
					MatrixEntry result = resultsMatrix.getEntryFor(i, j);
					this.values.put(new ComparisonPair(result.getBase().getName(), result.getComparedTo().getName()),
							result.getSimilarityPercent());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
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
