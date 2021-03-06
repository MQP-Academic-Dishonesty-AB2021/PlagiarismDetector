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

package Checksims.token;

import Checksims.ChecksimsException;

/**
 * Exception thrown when attempting to compare two submissions with different
 * tokenizations.
 */
public class TokenTypeMismatchException extends ChecksimsException {
	private static final long serialVersionUID = 1L;

	public TokenTypeMismatchException(String message) {
		super(message);
	}

	public TokenTypeMismatchException(String message, Throwable cause) {
		super(message, cause);
	}
}
