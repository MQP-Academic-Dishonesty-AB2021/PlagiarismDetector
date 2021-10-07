package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.util.ArrayList;

import static RacketTree.RacketTree.function_open;

public class RacketFunction implements RacketAtom {
    private ArrayList<RacketAtom> parameters;

    static private boolean MatchingBraces(int openingBrace, int closingBrace)
    throws InvalidFormatException {
        switch (openingBrace) {
            case '{':
                return closingBrace == '}';
            case '(':
                return closingBrace == ')';
            case '[':
                return closingBrace == ']';
            default:
                throw new InvalidFormatException("Error: not a proper opening brace: " + (char)openingBrace);
        }
    }

    public RacketFunction(PushbackReader fin, int openingBrace)
    throws InvalidFormatException, IOException {
        this.parameters = new ArrayList<RacketAtom>();
        for (int lastChar = fin.read(); !MatchingBraces(openingBrace, lastChar); lastChar = fin.read()) {
            if (lastChar == -1) {
                throw new InvalidFormatException("Error: Randomly Terminated File");
            }
            if (Character.isWhitespace(lastChar)) {
                continue;
            }
            if (function_open.indexOf(lastChar) != -1) {
                this.parameters.add(new RacketFunction(fin, lastChar));
            }
            else {
                this.parameters.add(new RacketKeyword(fin, lastChar));
            }
        }
    }

    @Override
    public boolean equals(RacketAtom other) {
        return false;
    }
}
