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

package Checksims.algorithm.similaritymatrix.output;

import Checksims.algorithm.InternalAlgorithmError;
import Checksims.algorithm.similaritymatrix.SimilarityMatrix;
import Checksims.util.reflection.NamedInstantiable;

/**
 * Output a Similarity Matrix in human-readable or machine-readable format.
 */
public interface MatrixPrinter extends NamedInstantiable {
	/**
	 * Print a Similarity Matrix to string.
	 *
	 * @param matrix Matrix to print
	 * @return String representation of matrix
	 * @throws InternalAlgorithmError Thrown on internal error processing matrix
	 */
	String printMatrix(SimilarityMatrix matrix) throws InternalAlgorithmError;
}
