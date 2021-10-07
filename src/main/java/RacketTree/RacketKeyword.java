package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;

public class RacketKeyword implements RacketAtom {
    private String keyword;
    private boolean commutative;
    private boolean associative;

    @Override
    public boolean equals(RacketAtom other) {
        return false;
    }

    public RacketKeyword() {
        this.keyword = "test";
        this.commutative = false;
        this.associative = false;
    }

    public RacketKeyword(PushbackReader fin, int openingChar)
            throws InvalidFormatException,
            IOException {
        StringBuilder keyword = new StringBuilder();
        switch (openingChar) {
            case ')':
            case '}':
            case ']':
                throw new InvalidFormatException("Error: Unmatched closing paranthesis");
            // TODO: Handle different primitive types (exact/inexact number, list, vector, string, etc...)
            default:
                keyword.append((char)openingChar);
        }

        for (int lastChar = fin.read(); !Character.isWhitespace(lastChar); lastChar = fin.read()) {
            if (lastChar == '\\')  {
                if (fin.read() == -1) {
                    throw new InvalidFormatException("Error: Randomly Terminated File");
                }
                continue;
            }
            if (lastChar == -1) {
                throw new InvalidFormatException("Error: Randomly Terminated File");
            }
            if (RacketTree.function_open.indexOf(lastChar) != -1 ||
                    RacketTree.function_close.indexOf(lastChar) != -1) {
                fin.unread(lastChar);
                break;
            }
            keyword.append((char)lastChar);
        }
        this.keyword = keyword.toString();
        this.associative = false;
        this.commutative = false;
    }
}
