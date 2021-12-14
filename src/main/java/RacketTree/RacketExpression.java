package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

public abstract class RacketExpression {
    protected RacketList parent;
    protected int height = 1;

    /**
     * Generates an expression from the current position in a file
     * @param fin The file to read from
     * @param parent The parent of the generated expression
     * @return The RacketAtom corresponding to the next expression in file
     * @throws InvalidFormatException The file does not follow proper racket syntax and a next racket expression could not be parsed
     * @throws IOException There was an error reading the file
     */
    public static RacketExpression generateAtom(PushbackReader fin, RacketList parent) throws InvalidFormatException, IOException {
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

    /**
     * Insert the racket expression into the tree map
     * @param map The map to add the leaves into
     * @param leafDepth the maximum height of the tree to be added to the map
     * @return The number of leaves added
     */
    int insertIntoTreeMap(HashMap<RacketExpression, ArrayList<RacketExpression>> map, int leafDepth) {
        ArrayList<RacketExpression> list = map.get(this);
        if (list == null) {
            ArrayList<RacketExpression> newList = new ArrayList<RacketExpression>();
            newList.add(this);
            map.put(this, newList);
        }
        else {
            list.add(this);
            // TODO: check if I really need to put it back in
            map.put(this, list);
        }
        return 1;
    }

    /**
     * Generate a similarity value between this expression and another
     * This number is about equivilent to the height of the tree that both are a part of
     * @param other The other expression to compare to
     * @return A value representing the similarity; Based off the height of the tree containing both this expression and the other
     */
    protected int similarityValue(RacketExpression other) {
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

    /**
     * Generate the sum of similarity values for an expression and a map of trees
     * @param map The map containing the expressions to generate similarity values from
     * @return The sum of similarity values between this expression and the expressions in the map
     */
    int similarityValue(HashMap<RacketExpression, ArrayList<RacketExpression>> map) {
        ArrayList<RacketExpression> list = map.get(this);

        if (list == null) {
            return 0;
        }
        int sum = 0;
        for (RacketExpression leaf : list) {
            sum += this.similarityValue(leaf);
        }
        return sum;
    }
}
