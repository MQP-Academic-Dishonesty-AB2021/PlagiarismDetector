package RacketTree;
import java.io.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class RacketTree {
    static public String function_open = "([{";
    static public String function_close = ")]}";
    static public String string_chars = "\"";
    static public String comment_chars = ";";
    static public String hash_delimeter = "#";
    static public String symbol_delimeter = "'";
    static public String lambda_chars = "λ";

    ArrayList<RacketAtom> children;
    HashMap<String, ArrayList<RacketAtom>> leavesHash;
    int numLeaves;


    public RacketTree(String filename) throws
            java.io.IOException,
            InvalidFormatException{
        // TODO: Check if buffered reader is actually better for efficiency
        this(new PushbackReader(new FileReader(filename)));
    }

    public RacketTree(PushbackReader fin) throws java.io.IOException, InvalidFormatException {
        this.children = new ArrayList<RacketAtom>();
        // iterate through file character by character
        // Theres just too many token exceptions to do anything but that
        for (int lastChar = fin.read(); lastChar != -1; lastChar = fin.read()) {
            if (!Character.isWhitespace((char)lastChar)) {
                fin.unread(lastChar);
                this.children.add(RacketAtom.generateAtom(fin, null));
            }
        }
        this.leavesHash = this.getLeavesHash();
    }

    /**
     *  First try is going to just count the size of subtrees between the two
     * @param other The other racket tree to compare with
     * @return a value representing how similar they are
     */
    public int TreeSimilarity(RacketTree other) {

        // First make a hashtable of all the leaf nodes of this
        // Then for each leaf see if its in the map and then if so go up and see if it has matching leaf nodes in their parents
        // add up the sum of length for each one
        return 0;
    }

    private HashMap<String, ArrayList<RacketAtom>> getLeavesHash() {
        HashMap<String, ArrayList<RacketAtom>> map = new HashMap<String, ArrayList<RacketAtom>>();
        int sum = 0;
        for (RacketAtom tree : this.children) {
            sum += tree.insertIntoTreeMap(map);
        }
        this.numLeaves = sum;
        return map;
    }

    private int similarityValue(HashMap<String, ArrayList<RacketAtom>> leavesMap) {
        int sum = 0;
        for (RacketAtom child : this.children) {
            sum += child.similarityValue(leavesMap);
        }
        return sum;
    }

    public int similarityValue(RacketTree other) {
        HashMap<String, ArrayList<RacketAtom>> map = this.getLeavesHash();
        return other.similarityValue(map) / this.numLeaves;
    }

    public int size() {
        return children.size();
    }
}
