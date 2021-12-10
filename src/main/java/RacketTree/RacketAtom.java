package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

public abstract class RacketAtom {
    public RacketList parent;
    public int height = 1;


    public static RacketAtom generateAtom(PushbackReader fin, RacketList parent) throws InvalidFormatException, IOException {
        int firstChar = fin.read();
        while (Character.isWhitespace((char)firstChar)) { firstChar = fin.read(); }
        switch (firstChar) {
            case -1:
                throw new EndOfFileException();
            case '"':
                return new RacketString(fin, parent);
            case '(': case '{': case '[':
                fin.unread((char)firstChar);
                return new RacketList(fin, parent);
            default:
                fin.unread((char)firstChar);
                return new RacketKeyword(fin, parent);
        }
    }

    public int similarityValue(HashMap<RacketAtom, ArrayList<RacketAtom>> map) {
        return 0;
    }

    /**
     *
     * @param map The map to add the leaves into
     * @return The number of leaves added
     */
    public int insertIntoTreeMap(HashMap<RacketAtom, ArrayList<RacketAtom>> map, int leafDepth) {
        return 0;
    }

    int similarityValue(RacketAtom other) {
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
}
