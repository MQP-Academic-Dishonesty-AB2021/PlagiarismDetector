package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

public abstract class RacketAtom {
    public RacketList parent;

    boolean equals(RacketAtom other) {
        return false;
    };

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

    public int similarityValue(HashMap<String, ArrayList<RacketAtom>> map) {
        return 0;
    }

    public void insertIntoTreeMap(HashMap<String, ArrayList<RacketAtom>> map) {}
}
