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

package Checksims.algorithm.preprocessor;

import Checksims.util.reflection.Registry;

import java.util.Collections;

/**
 * Registry to obtain valid preprocessors.
 */
public final class PreprocessorRegistry extends Registry<SubmissionPreprocessor> {
	private static PreprocessorRegistry instance;

	private PreprocessorRegistry() {
		super("Checksims.algorithm.preprocessor", SubmissionPreprocessor.class,
				Collections.singleton("commoncodeline"));
	}

	/**
	 * @return Singleton instance of PreprocessorRegistry
	 */
	public static PreprocessorRegistry getInstance() {
		if (instance == null) {
			instance = new PreprocessorRegistry();
		}

		return instance;
	}

	@Override
	public String toString() {
		return "Singleton instance of PreprocessorRegistry";
	}
}
