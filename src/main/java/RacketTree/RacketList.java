package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

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
    public int insertIntoTreeMap(HashMap<String, ArrayList<RacketAtom>> map) {
        int sum = 0;
        for (RacketAtom child : this.items) {
            sum += child.insertIntoTreeMap(map);
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
//         using a hash set slows it down considerably -- even though it should be O(n) (small dataset)
//         TODO: Delete this commented out code once it's tracked by git (I dont want to have to retype it later)
//        HashSet<RacketAtom> set = new HashSet<RacketAtom>();
//        for (RacketAtom thisChild : this.items) {
//            if (thisChild != child) {
//                set.add(thisChild);
//            }
//        }
//        int num = 0;
//        for (RacketAtom otherChild : other.items) {
//            num += set.contains(otherChild) ? 1 : 0;
//        }
//        return num;
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

    public int hashCode() {
        if (this.items.size() == 0) { return 0; }
        return this.items.get(0).hashCode() * this.items.size();
    }
}
