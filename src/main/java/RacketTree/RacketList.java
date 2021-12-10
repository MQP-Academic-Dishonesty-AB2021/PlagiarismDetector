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
//    public static long matchingChildrenCount = 0;

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
            ArrayList<RacketAtom> currentList = map.get(this);
            if (currentList == null) {
                ArrayList<RacketAtom> newList = new ArrayList<RacketAtom>();
                map.put(this, newList);
            }
            else {
                currentList.add(this);
            }
            return 1;
        }
        int sum = 0;
        for (RacketAtom child : this.items) {
            sum += child.insertIntoTreeMap(map, leafDepth);
        }
        return sum;
    }

    public int numberOfMatchingChildren(RacketList other, RacketAtom child) {
//        this.matchingChildrenCount++;
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
//        RacketAtom.equalsCount++;
        if (other.getClass() != this.getClass()) {
            return false;
        }
        RacketList otherList = (RacketList) other;
        if (otherList.items.size() != this.items.size()) {
            return false;
        }
//        for (int i = 0; i < this.items.size(); i++) {
//            if (!otherList.items.get(i).equals(this.items.get(i))) {
//                return false;
//            }
//        }
        return this.items.equals(otherList.items);
    }

    @Override
    public int similarityValue(HashMap<RacketAtom, ArrayList<RacketAtom>> leavesMap) {
        int sum = 0;
        ArrayList<RacketAtom> sameList = leavesMap.get(this);
        if (sameList != null) {
            for (RacketAtom leaf : sameList) {
                sum += this.similarityValue(leaf);
            }
        }
        for (RacketAtom child : this.items) {
            sum += child.similarityValue(leavesMap);
        }
        return sum;
    }

    public int hashCode() {
        if (this.items.size() == 0) { return 0; }
        return this.items.get(0).hashCode() * this.items.size();
    }
}
