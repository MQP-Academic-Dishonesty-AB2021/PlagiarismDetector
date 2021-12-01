package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

public class RacketString extends RacketAtom {
    private String str;
    public RacketString(PushbackReader fin, RacketList parent)
    throws InvalidFormatException, IOException {
        super.parent = parent;
        StringBuilder val = new StringBuilder();
        for (int lastChar = fin.read(); lastChar != '"'; lastChar = fin.read()) {
            if (lastChar == -1) {
                throw new EndOfFileException();
            }
            if (lastChar == '\\') {
                int nextChar = fin.read();
                if (nextChar == -1) { throw new EndOfFileException(); }
                val.append('\\');
                val.append((char)nextChar);
                continue;
            }
            val.append((char)lastChar);
        }
        this.str = val.toString();
    }

    public int insertIntoTreeMap(HashMap<String, ArrayList<RacketAtom>> map) {
        ArrayList<RacketAtom> list = map.get("\"" + this.str + "\"");
        if (list == null) {
            ArrayList<RacketAtom> newList = new ArrayList<RacketAtom>();
            newList.add(this);
            map.put("\"" + this.str + "\"", newList);
        }
        else {
            list.add(this);
            // TODO: check if I really need to put it back in
            map.put("\"" + this.str + "\"", list);
        }
        return 1;
    }

    public boolean equals(RacketAtom other) {
        if (!(other instanceof RacketString)) {
            return false;
        }
        RacketString otherString = (RacketString) other;
        return otherString.str.equals(this.str);
    }
}
