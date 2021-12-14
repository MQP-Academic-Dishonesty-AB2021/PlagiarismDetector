package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;

public class RacketString extends RacketExpression {
    private final String str;

    /**
     * Generate a Racket String from the current place in the file,
     * Assumes that you have already the first opening "
     * @param fin The file to read from
     * @param parent The parent of the string
     * @throws InvalidFormatException Given an improperly formatted file and could not finish the string
     * @throws IOException There was an error reading the file
     */
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
        this.height = 1;
    }

    /**
     * Returns true iff the other object is a RacketString with the same string value
     * @param other The object to compare too
     * @return
     */
    public boolean equals(RacketExpression other) {
        if (other.getClass() != this.getClass()) {
            return false;
        }
        RacketString otherString = (RacketString) other;
        return otherString.str.equals(this.str);
    }

    /**
     * The hashCode is the same as the underlying string but added one to avoid collisions with RacketAtoms
     * @return the hashCode
     */
    @Override
    public int hashCode() { return this.str.hashCode() + 1; }

    /**
     *
     * @return The string value of the String surrounded by quotes
     */
    public String toString() { return "\"" + this.str + "\""; }
}
