/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * See LICENSE.txt included in this distribution for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at LICENSE.txt.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *
 * Copyright (c) 2014-2015 Nicholas DeMarinis, Matthew Heon, and Dolan Murvihill
 */

package Checksims.util.threading;

import com.google.common.collect.ImmutableSet;
import Checksims.ChecksimsException;
import Checksims.algorithm.AlgorithmResults;
import Checksims.algorithm.SimilarityDetector;
import Checksims.algorithm.preprocessor.SubmissionPreprocessor;
import Checksims.submission.Submission;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Apply a given algorithm to a given set of data in parallel.
 */
public final class ParallelAlgorithm {
	private ParallelAlgorithm() {
	}

	private static Logger logs = LoggerFactory.getLogger(ParallelAlgorithm.class);

	private static int threadCount = Runtime.getRuntime().availableProcessors();
	private static ThreadPoolExecutor executor = new ThreadPoolExecutor(threadCount, threadCount, 1, TimeUnit.SECONDS,
			new LinkedBlockingQueue<>(), new ThreadPoolExecutor.AbortPolicy());

	/**
	 * @param threads Number of threads to be used for execution
	 */
	public static void setThreadCount(int threads) {
		checkArgument(threads > 0, "Attempted to set number of threads to " + threads
				+ ", but must be positive integer!");

		threadCount = threads;
		executor.shutdown();
		// Set up the executor again with the new thread count
		executor = new ThreadPoolExecutor(threadCount, threadCount, 1, TimeUnit.SECONDS, new LinkedBlockingQueue<>(),
				new ThreadPoolExecutor.AbortPolicy());
	}

	/**
	 * Shut down the executor, preventing any more jobs from being processed.
	 */
	public static void shutdownExecutor() {
		executor.shutdown();
	}

	/**
	 * @return Number of threads to be used for execution. Defaults to number of
	 *         CPUs available on system.
	 */
	public static int getThreadCount() {
		return threadCount;
	}

	/**
	 * Detect similarities in parallel.
	 *
	 * @param algorithm Algorithm to use for similarity detection
	 * @param pairs     Pairs of submissions to perform detection on
	 * @return Collection of results, one for each pair
	 */
	public static Set<AlgorithmResults> parallelSimilarityDetection(SimilarityDetector algorithm,
			Set<Pair<Submission, Submission>> pairs)
			throws ChecksimsException {
		checkNotNull(algorithm);
		checkNotNull(pairs);

		// Map the pairs to ChecksimsWorker instances
		Collection<SimilarityDetectionWorker> workers = pairs.stream()
				.map((pair) -> new SimilarityDetectionWorker(algorithm, pair))
				.collect(Collectors.toList());

		return ImmutableSet.copyOf(executeTasks(workers));
	}

	public static Set<Submission> parallelSubmissionPreprocessing(SubmissionPreprocessor preprocessor,
			Set<Submission> submissions)
			throws ChecksimsException {
		checkNotNull(preprocessor);
		checkNotNull(submissions);

		// Map the submissions to PreprocessorWorker instances
		Collection<PreprocessorWorker> workers = submissions.stream()
				.map((submission) -> new PreprocessorWorker(submission, preprocessor))
				.collect(Collectors.toList());

		return ImmutableSet.copyOf(executeTasks(workers));
	}

	/**
	 * Internal backend: Execute given tasks on a new thread pool.
	 *
	 * Expects Callable tasks, with non-void returns. If the need for void returning
	 * functions emerges, might need
	 * another version of this?
	 *
	 * @param tasks Tasks to execute
	 * @param <T>   Type returned by the tasks
	 * @return Collection of Ts
	 */
	private static <T, T2 extends Callable<T>> Collection<T> executeTasks(Collection<T2> tasks)
			throws ChecksimsException {
		checkNotNull(tasks);

		if (tasks.size() == 0) {
			logs.warn("Parallel execution called with no tasks - no work done!");
			return new ArrayList<>();
		}

		if (executor.isShutdown()) {
			throw new ChecksimsException("Attempted to call executeTasks while executor was shut down!");
		}

		logs.info("Starting work using " + threadCount + " threads.");

		// Invoke the executor on all the worker instances
		try {
			// Create a monitoring thread to show progress
			MonitorThread monitor = new MonitorThread(executor);
			Thread monitorThread = new Thread(monitor);
			monitorThread.start();

			List<Future<T>> results = executor.invokeAll(tasks);

			// Stop the monitor
			monitor.shutDown();

			// Unpack the futures
			ArrayList<T> unpackInto = new ArrayList<>();

			for (Future<T> future : results) {
				try {
					unpackInto.add(future.get());
				} catch (ExecutionException e) {
					executor.shutdownNow();
					logs.error("Fatal error in executed job!");
					throw new ChecksimsException("Error while executing worker for future", e.getCause());
				}
			}

			return unpackInto;
		} catch (InterruptedException e) {
			executor.shutdownNow();
			throw new ChecksimsException("Execution of Checksims was interrupted!", e);
		} catch (RejectedExecutionException e) {
			executor.shutdownNow();
			throw new ChecksimsException("Could not schedule execution of all tasks!", e);
		}
	}
}
