package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;

public class RacketKeyword extends RacketAtom {
    private String keyword;

    @Override
    public boolean equals(Object other) {
        if (!(other.getClass() == this.getClass())) {
            return false;
        }
        RacketKeyword otherKeyword = (RacketKeyword) other;
        return otherKeyword.keyword.equals(this.keyword);
    }

    private StringBuilder getKeywordBuilder(PushbackReader fin) throws InvalidFormatException, IOException {
        StringBuilder word = new StringBuilder();
        for (int lastChar = fin.read(); !Character.isWhitespace((char)lastChar); lastChar = fin.read()) {
            switch (lastChar) {
                case -1:
                    throw new EndOfFileException();
                case '\\': {
                    int nextChar = fin.read();
                    if (nextChar == -1) { throw new EndOfFileException(); }
                    word.append((char)lastChar);
                    word.append((char)nextChar);
                    return word;
                }
                case '|':
                    word.append('|');
                    for (int nextChar = fin.read(); nextChar != '|'; nextChar = fin.read()) {
                        if (nextChar == -1) { throw new EndOfFileException(); }
                        word.append((char)nextChar);
                    }
                    word.append('|');
                    break;
                // all the delimeters
                case '\'':
                    if (word.length() == 0) {
                        word.append('\'');
                        break;
                    }
                case '{': case '(': case '[': case '}': case ']': case ')': case '"': case '`': case ',':
                    fin.unread(lastChar);
                    return word;

                default:
                    word.append((char)lastChar);
            }
        }
        return word;
    }

    public RacketKeyword(PushbackReader fin, RacketList parent)
            throws InvalidFormatException,
            IOException {
        super.parent = parent;
        StringBuilder word = this.getKeywordBuilder(fin);
        if (word.length() == 0) {
            throw new InvalidFormatException("Error: found an invalid keyword");
        }
        this.keyword = word.toString();
    }

    @Override
    public int insertIntoTreeMap(HashMap<String, ArrayList<RacketAtom>> map) {
        ArrayList<RacketAtom> list = map.get(this.keyword);
        if (list == null) {
            ArrayList<RacketAtom> newList = new ArrayList<RacketAtom>();
            newList.add(this);
            map.put(this.keyword, newList);
        }
        else {
            list.add(this);
            // TODO: check if I really need to put it back in
            map.put(this.keyword, list);
        }
        return 1;
    }

    @Override
    public int similarityValue(HashMap<String, ArrayList<RacketAtom>> map) {
        ArrayList<RacketAtom> list = map.get(this.keyword);

        if (list == null) {
            return 0;
        }
        int sum = 0;
        for (RacketAtom leaf : list) {
            sum += this.similarityValue(leaf);
        }
        return sum;
    }

    private int similarityValue(RacketAtom other) {
        int value = 1;
        RacketList otherParent = other.parent;
        RacketList parent = this.parent;
        while (parent != null && otherParent != null) {
            // requiring consecutive matches decreased time by a lot
            // while almost not change to output
            int numMatches = otherParent.numberOfMatchingChildren(parent, this);
            if (numMatches == 0) {
                break;
            }
            value += numMatches;
            parent = parent.parent;
            otherParent = otherParent.parent;
        }
        return value;
    }

    @Override
    public int hashCode() {
        return this.keyword.hashCode();
    }
}
