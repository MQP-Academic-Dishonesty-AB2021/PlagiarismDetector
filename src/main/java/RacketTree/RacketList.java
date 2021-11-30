package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

import static RacketTree.RacketTree.function_open;
import static RacketTree.RacketTree.string_chars;

public class RacketList extends RacketAtom {
    private ArrayList<RacketAtom> items;

    static private int MatchingBraces(int openingBrace)
    throws InvalidFormatException {
        switch (openingBrace) {
            case '{':
                return '}';
            case '(':
                return ')';
            case '[':
                return ']';
            default:
                throw new InvalidFormatException("Error: not a proper opening brace: " + (char)openingBrace);
        }
    }

    public RacketList(PushbackReader fin, RacketList parent)
    throws InvalidFormatException, IOException {
        super.parent = parent;
        int openingBrace = fin.read();
        int closingBrace = MatchingBraces(openingBrace);
        this.items = new ArrayList<RacketAtom>();
        for (int lastChar = fin.read(); lastChar != closingBrace; lastChar = fin.read()) {
            if (lastChar == -1) {
                throw new InvalidFormatException("Error: Randomly Terminated File");
            }
            if (Character.isWhitespace(lastChar)) {
                continue;
            }
            else {
                fin.unread(lastChar);
                try {
                    this.items.add(RacketAtom.generateAtom(fin, this));
                }
                catch (InvalidFormatException e) {
                    if (e.toString().equals("Error:found an invalid keyword")) {
                        continue;
                    }
                    throw e;
                }
            }
        }
    }

    @Override
    public void insertIntoTreeMap(HashMap<String, ArrayList<RacketAtom>> map) {
        for (RacketAtom child : this.items) {
            child.insertIntoTreeMap(map);
        }
    }

    public int numberOfMatchingChildren(RacketList other) {
        // Since hopefully the number of children should really never go above 5ish I will do a use O(n^2)
        // algorithm to find the intersection rather than deal with making a hashmap to make it O(n)
        // TODO: see if its worth it to use a hashmap to make it O(n)
        int num = 0;
        for (RacketAtom child : this.items) {
            for (RacketAtom otherChild : other.items) {
                if (child.equals(otherChild)) {
                    num++;
                }
            }
        }
        return num;
    }

    @Override
    public boolean equals(RacketAtom other) {
        if (!(other instanceof RacketList)) {
            return false;
        }
        RacketList otherList = (RacketList) other;
        if (otherList.items.size() != this.items.size()) {
            return false;
        }
        for (int i = 0; i < this.items.size(); i++) {
            if (!otherList.items.get(i).equals(this.items.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int similarityValue(HashMap<String, ArrayList<RacketAtom>> leavesMap) {
        int sum = 0;
        for (RacketAtom child : this.items) {
            sum += child.similarityValue(leavesMap);
        }
        return sum;
    }
}
