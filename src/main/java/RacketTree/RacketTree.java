package RacketTree;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class RacketTree {
    static public String function_open = "([{";
    static public String function_close = ")]}";
    static public String string_chars = "\"";
    static public String comment_chars = ";";
    static public String hash_delimeter = "#";
    static public String symbol_delimeter = "'";
    static public String lambda_chars = "λ";

    ArrayList<RacketAtom> children;
    public HashMap<RacketAtom, ArrayList<RacketAtom>> leavesHash;
    private int leafDepth;
    public int numLeaves;
    public static int defaultLeafDepth = 1;


    public RacketTree(String filename) throws
            java.io.IOException,
            InvalidFormatException{
        // TODO: Check if buffered reader is actually better for efficiency
        File fin = new File(filename);
        if (fin.isFile()) {
            this.children = this.generateChildren(new PushbackReader(new FileReader(fin)));
            this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
            return;
        }
        else if (fin.isDirectory()) {
            Stream<Path> files = Files.find(Paths.get(fin.getAbsolutePath()), 999,
                    (p, bfa) -> p.getFileName().toString().matches(".*\\.rkt"));
            this.children = new ArrayList<RacketAtom>();
            files.forEach((file) -> {
                try {
                    this.children.addAll(this.generateChildren(new PushbackReader(new FileReader(file.toFile()))));
                } catch (IOException | InvalidFormatException e) {
                    e.printStackTrace();
                }
            });
        }
        this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
    }

    public RacketTree(PushbackReader fin) throws java.io.IOException, InvalidFormatException {
        this.children = generateChildren(fin);
        this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
    }

    private ArrayList<RacketAtom> generateChildren(PushbackReader fin)
            throws java.io.IOException, InvalidFormatException {
        ArrayList<RacketAtom> children = new ArrayList<RacketAtom>();
        // iterate through file character by character
        // Theres just too many token exceptions to do anything but that
        for (int lastChar = fin.read(); lastChar != -1; lastChar = fin.read()) {
            if (!Character.isWhitespace((char)lastChar)) {
                fin.unread(lastChar);
                children.add(RacketAtom.generateAtom(fin, null));
            }
        }
        return children;
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

    private HashMap<RacketAtom, ArrayList<RacketAtom>> generateLeavesHash() {
        return this.generateLeavesHash(this.leafDepth);
    }

    private HashMap<RacketAtom, ArrayList<RacketAtom>> generateLeavesHash(int height) {
        HashMap<RacketAtom, ArrayList<RacketAtom>> map = new HashMap<>();
        int sum = 0;
        for (RacketAtom tree : this.children) {
            sum += tree.insertIntoTreeMap(map, height);
        }
        this.numLeaves = sum;
        return map;
    }

    private double similarityValue(HashMap<RacketAtom, ArrayList<RacketAtom>> leavesMap, int minHeight) {
        int sum = 0;
        for (RacketAtom child : this.children) {
            sum += child.similarityValue(leavesMap);
        }
//        for (ArrayList<RacketAtom> list: this.leavesHash.values()) {
//        for (Map.Entry<String, ArrayList<RacketAtom>> leaf : this.leavesHash.entrySet()) {
//            if (leaf.getKey().equals("define")) { continue; }
//            for (RacketAtom child : leaf.getValue()) {
//                sum += child.similarityValue(leavesMap);
//            }
//        }
        return (double)sum / this.numLeaves;
    }

    public void setLeafDepth(int leafDepth) {
        if (this.leafDepth == leafDepth) { return; }
        this.leafDepth = leafDepth;
        this.leavesHash = this.generateLeavesHash();
    }

    public double similarityValue(RacketTree other) {
        return other.similarityValue(this.leavesHash, this.leafDepth);
    }

    public int size() {
        return children.size();
    }
}
