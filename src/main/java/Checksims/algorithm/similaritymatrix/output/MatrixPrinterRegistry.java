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

import Checksims.util.reflection.NoSuchImplementationException;
import Checksims.util.reflection.RegistryWithDefault;

/**
 * Registry for Matrix Printers.
 */
public final class MatrixPrinterRegistry extends RegistryWithDefault<MatrixPrinter> {
	private static MatrixPrinterRegistry instance;

	private MatrixPrinterRegistry() throws NoSuchImplementationException {
		super("Checksims.algorithm.similaritymatrix.output", MatrixPrinter.class,
				MatrixThresholdPrinter.getInstance().getName());
	}

	/**
	 * @return Singleton instance of MatrixPrinterRegistry
	 */
	public static MatrixPrinterRegistry getInstance() {
		if (instance == null) {
			try {
				instance = new MatrixPrinterRegistry();
			} catch (NoSuchImplementationException e) {
				throw new RuntimeException(e);
			}
		}

		return instance;
	}

	@Override
	public String toString() {
		return "Singleton instance of MatrixPrinterRegistry";
	}
}
