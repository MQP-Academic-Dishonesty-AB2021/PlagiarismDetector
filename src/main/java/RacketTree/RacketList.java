package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Optional;

import static RacketTree.RacketTree.function_open;
import static RacketTree.RacketTree.string_chars;

public class RacketList extends RacketAtom {
    public ArrayList<RacketAtom> items;

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
        super.height = this.items.stream().reduce(0,
                (max, next) -> Integer.max(max, next.height),
                (Integer::max)) + 1;
    }

    @Override
    public int insertIntoTreeMap(HashMap<RacketAtom, ArrayList<RacketAtom>> map, int leafDepth) {
        if (this.height <= leafDepth) {
            return super.insertIntoTreeMap(map, leafDepth);
        }
        int sum = 0;
        for (RacketAtom child : this.items) {
            sum += child.insertIntoTreeMap(map, leafDepth);
        }
        return sum;
    }

    public int numberOfMatchingChildren(RacketList other, RacketAtom child) {
        int num = 0;
        for (RacketAtom thisChild : this.items) {
            if (thisChild == child) {
                continue;
            }
            for (RacketAtom otherChild : other.items) {
                if (child.equals(otherChild)) {
                    num++;
                }
            }
        }
        return num;
    }

    @Override
    public boolean equals(Object other) {
        if (other.getClass() != this.getClass()) {
            return false;
        }
        RacketList otherList = (RacketList) other;
        if (otherList.items.size() != this.items.size()) {
            return false;
        }
        return this.items.equals(otherList.items);
    }

    @Override
    public int similarityValue(HashMap<RacketAtom, ArrayList<RacketAtom>> leavesMap) {
        int sum = super.similarityValue(leavesMap);
        for (RacketAtom child : this.items) {
            sum += child.similarityValue(leavesMap);
        }
        return sum;
    }

    public int hashCode() {
        if (this.items.size() == 0) { return 0; }
        return this.items.get(0).hashCode() * this.items.size();
    }

    public String toString() {
        StringBuilder strbuild = new StringBuilder("(");
        for (RacketAtom child : this.items) {
            strbuild.append(child.toString());
            strbuild.append(" ");
        }
        strbuild.append(')');
        return strbuild.toString();
    }
}
