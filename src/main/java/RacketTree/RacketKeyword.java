package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;

public class RacketKeyword extends RacketExpression {
	private final String keyword;

	private StringBuilder getKeywordBuilder(PushbackReader fin) throws InvalidFormatException, IOException {
		StringBuilder word = new StringBuilder();
		for (int lastChar = fin.read(); !Character.isWhitespace((char) lastChar); lastChar = fin.read()) {
			switch (lastChar) {
				// EOF
				case -1:
					throw new EndOfFileException();
				// Special case where a backslash escapes any character
				case '\\': {
					int nextChar = fin.read();
					if (nextChar == -1) {
						throw new EndOfFileException();
					}
					word.append((char) lastChar);
					word.append((char) nextChar);
					break;
				}
				// Expression blocks automatically escape any character
				case '|':
					word.append('|');
					for (int nextChar = fin.read(); nextChar != '|'; nextChar = fin.read()) {
						if (nextChar == -1) {
							throw new EndOfFileException();
						}
						word.append((char) nextChar);
					}
					word.append('|');
					break;
				// An apostrophe can be part of a keyword iff its at the start
				case '\'':
					if (word.length() == 0) {
						word.append('\'');
						break;
					}
					// all the delimeters
				case '{':
				case '(':
				case '[':
				case '}':
				case ']':
				case ')':
				case '"':
				case '`':
				case ',':
					fin.unread(lastChar);
					return word;

				default:
					word.append((char) lastChar);
			}
		}
		return word;
	}

	/**
	 * Constructs a RacketKeyword from the current place in the given file
	 * 
	 * @param fin    The file to read from
	 * @param parent The parent of the keyword
	 * @throws InvalidFormatException Was given an improperly formatted file and was
	 *                                not able to read the keyword
	 * @throws IOException            There was an error reading the file
	 */
	RacketKeyword(PushbackReader fin, RacketList parent)
			throws InvalidFormatException,
			IOException {
		super.parent = parent;
		StringBuilder word = this.getKeywordBuilder(fin);
		if (word.length() == 0) {
			throw new InvalidFormatException("Error: found an invalid keyword");
		}
		this.keyword = word.toString();
		super.height = 1;
	}

	/**
	 * Returns true iff the other object is a RacketKeyword with the same keyword
	 * value
	 * 
	 * @param other The object to be compared against
	 * @return True if they are equal, false if not
	 */
	@Override
	public boolean equals(Object other) {
		if (!(other.getClass() == this.getClass())) {
			return false;
		}
		RacketKeyword otherKeyword = (RacketKeyword) other;
		return otherKeyword.keyword.equals(this.keyword);
	}

	/**
	 * The hashcode is just equivalent to the hashCode of the keyword
	 * 
	 * @return The hashCode of the keyword
	 */
	@Override
	public int hashCode() {
		return this.keyword.hashCode();
	}

	/**
	 * The string representation would just be the keyword
	 * 
	 * @return the keyword
	 */
	@Override
	public String toString() {
		return this.keyword;
	}
}
