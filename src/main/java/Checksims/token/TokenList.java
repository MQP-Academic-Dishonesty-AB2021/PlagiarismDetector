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

import com.google.common.collect.ImmutableList;
import org.apache.commons.collections4.list.PredicatedList;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * A list of tokens of a specific type.
 */
public class TokenList extends PredicatedList<Token> {
	public final TokenType type;

	private static final long serialVersionUID = 1L;

	/**
	 * Create a TokenList accepting a specific type of token.
	 *
	 * @param type Type of token which will be allowed in the list
	 */
	public TokenList(TokenType type) {
		super(new ArrayList<>(), (token) -> token.getType().equals(type));

		checkNotNull(type);

		this.type = type;
	}

	/**
	 * Internal copy constructor.
	 *
	 * DOES NOT enforce the invariant that all tokens already in baseList must be of
	 * the given type (performance
	 * reasons). All uses of this function are in scenarios that guarantee this
	 * invariant is held true, however.
	 *
	 * @param type     Type of token accepted
	 * @param baseList List of tokens to use when building list
	 */
	private TokenList(TokenType type, List<Token> baseList) {
		super(baseList, (token) -> token.getType().equals(type));
		this.type = type;
	}

	/**
	 * Join each token in the list in order, using a tokenization-appropriate
	 * separating character.
	 *
	 * @param onlyValid If true, ignore invalid tokens when joining
	 * @return String composed of each element in the token list, in order,
	 *         separated by appropriate character
	 */
	public String join(boolean onlyValid) {
		if (this.size() == 0) {
			return "";
		}

		StringBuilder b = new StringBuilder();
		String sepChar;

		switch (type) {
			case CHARACTER:
				sepChar = "";
				break;
			case WHITESPACE:
				sepChar = " ";
				break;
			case LINE:
				sepChar = "\n";
				break;
			default:
				sepChar = "";
				break;
		}

		this.stream().forEachOrdered((token) -> {
			if (!onlyValid || token.isValid()) {
				b.append(token.getTokenAsString());
				b.append(sepChar);
			}
		});

		// Trim the last trailing whitespace in whitespace tokenization
		if (type.equals(TokenType.WHITESPACE)) {
			return b.toString().trim();
		}

		return b.toString();
	}

	/**
	 * Peforms a deep copy of a TokenList, returning an immutable version of the
	 * initial list with immutable tokens.
	 *
	 * @param cloneFrom List to copy
	 * @return Immutable copy of cloneFrom
	 */
	public static TokenList immutableCopy(final TokenList cloneFrom) {
		checkNotNull(cloneFrom);

		final List<ImmutableToken> tmp = new LinkedList<>();
		for (final Token t : cloneFrom) {
			tmp.add(new ImmutableToken(Token.cloneToken(t)));
		}

		return new TokenList(cloneFrom.type, ImmutableList.copyOf(tmp));
	}

	/**
	 * Perform a deep copy of a TokenList.
	 *
	 * TODO add a copy constructor as well
	 *
	 * @param cloneFrom List to deep copy
	 * @return Cloned copy of the tokenization list
	 */
	public static TokenList cloneTokenList(TokenList cloneFrom) {
		checkNotNull(cloneFrom);

		Supplier<TokenList> tokenListSupplier = () -> new TokenList(cloneFrom.type);

		return cloneFrom.stream().map(Token::cloneToken).collect(Collectors.toCollection(tokenListSupplier));
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof TokenList)) {
			return false;
		}

		TokenList otherList = (TokenList) other;

		// The super.equals() call here is bad practice because we can't *guarantee*
		// it's a PredicatedList<Token>
		// However, the instanceof TokenList should ensure that invariant is met
		return otherList.type.equals(this.type) && super.equals(otherList);
	}

	@Override
	public String toString() {
		return "Token list of type " + type.toString() + " containing " + super.toString();
	}

	@Override
	public int hashCode() {
		return type.hashCode() ^ super.hashCode();
	}

	public int numValid() {
		return (int) this.stream().filter(Token::isValid).count();
	}
}
