package RacketTree;
import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

public class RacketTree {
    static public String function_open = "([{";
    static public String function_close = ")]}";
    static public String comment_chars = ";";
    static public String hash_delimeter = "#";
    static public String symbol_delimeter = "'";
    static public String lambda_chars = "λ";

    ArrayList<RacketAtom> children;

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
            // backslash literally escapes everything
            if (Character.isWhitespace(lastChar)) {
                continue;
            }
            if (function_open.indexOf(lastChar) != -1) {
                this.children.add(new RacketFunction(fin, lastChar));
            }
            else {
                this.children.add(new RacketKeyword(fin, lastChar));
            }
        }
    }

    public int size() {
        return children.size();
    }
}
