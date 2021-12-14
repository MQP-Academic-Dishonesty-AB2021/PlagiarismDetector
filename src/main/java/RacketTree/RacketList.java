package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.ArrayList;
import java.util.HashMap;

public class RacketList extends RacketExpression {
    public ArrayList<RacketExpression> items;

    /**
     * Helper function to return the corresponding closing brace given opening brace
     * @param openingBrace the opening brace
     * @return The corresponding closingBrace
     * @throws InvalidFormatException If not given a proper opening brace
     */
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

    /**
     * Constructs a racket list from the current location in the file
     * @param fin The file to read from
     * @param parent The parent of the list
     * @throws InvalidFormatException Was given an invalidly formatted file and could not parse a proper list
     * @throws IOException There was an error reading the file
     */
    RacketList(PushbackReader fin, RacketList parent)
    throws InvalidFormatException, IOException {
        super.parent = parent;
        int openingBrace = fin.read();
        int closingBrace = MatchingBraces(openingBrace);
        this.items = new ArrayList<RacketExpression>();
        for (int lastChar = fin.read(); lastChar != closingBrace; lastChar = fin.read()) {
            if (lastChar == -1) {
                throw new InvalidFormatException("Error: Randomly Terminated File");
            }
            if (Character.isWhitespace(lastChar)) {
                // Leaving this continue just in case someone deletes the else later on
                continue;
            }
            else {
                fin.unread(lastChar);
                this.items.add(RacketExpression.generateAtom(fin, this));
            }
        }
        // Calculate the height from the maximum height of its children
        super.height = this.items.stream().reduce(0,
                (max, next) -> Integer.max(max, next.height),
                (Integer::max)) + 1;
    }

    /**
     * Insert the list into the tree map if it has a height less than or equal to the leaf depth
     * @param map The map to add the leaves into
     * @param leafDepth the maximum height of the tree to be added to the map
     * @return The number of leaves added
     */
    @Override
    int insertIntoTreeMap(HashMap<RacketExpression, ArrayList<RacketExpression>> map, int leafDepth) {
        if (this.height <= leafDepth) {
            return super.insertIntoTreeMap(map, leafDepth);
        }
        int sum = 0;
        for (RacketExpression child : this.items) {
            sum += child.insertIntoTreeMap(map, leafDepth);
        }
        return sum;
    }

    /**
     * Returns the number of matching children between this list and another list, making sure to skip the child it is coming from
     * Using a hashmap would make this O(n) algorithm instead, but I found that since we're dealing with low values of n
     * The hashing and equals functions end up overriding the benefit
     * @param other The other list to compare to
     * @param child The child that this comparison is coming from
     * @return The number of matching children between the two lists
     */
    int numberOfMatchingChildren(RacketList other, RacketExpression child) {
        int num = 0;
        for (RacketExpression thisChild : this.items) {
            if (thisChild == child) {
                continue;
            }
            for (RacketExpression otherChild : other.items) {
                if (child.equals(otherChild)) {
                    num++;
                }
            }
        }
        return num;
    }

    /**
     * Returns a similarity value given a leavesMap
     * For a list this is defined and the sum of the leaves value of the children and itself
     * @param leavesMap
     * @return The sum of the similarity value of its children and itself
     */
    @Override
    int similarityValue(HashMap<RacketExpression, ArrayList<RacketExpression>> leavesMap) {
        int sum = super.similarityValue(leavesMap);
        for (RacketExpression child : this.items) {
            sum += child.similarityValue(leavesMap);
        }
        return sum;
    }

    /**
     *
     * @param other The other object to compare too
     * @return true iff the other item is a RacketList with an equal list of items
     */
    @Override
    public boolean equals(Object other) {
        if (other.getClass() != this.getClass()) {
            return false;
        }
        RacketList otherList = (RacketList) other;
        return this.items.equals(otherList.items);
    }


    /**
     * hashCode for a Racket List, equivalent to the hashCode of its first element times the size of the list
     * @return the hashCode
     */
    public int hashCode() {
        if (this.items.size() == 0) { return 0; }
        return this.items.get(0).hashCode() * this.items.size();
    }

    /**
     * Returns a string representation of the list what is the children wrapped in paranthesis
     * @return a string representation of the list
     */
    public String toString() {
        StringBuilder strbuild = new StringBuilder("(");
        for (RacketExpression child : this.items) {
            strbuild.append(child.toString());
            strbuild.append(" ");
        }
        strbuild.append(')');
        return strbuild.toString();
    }
}
