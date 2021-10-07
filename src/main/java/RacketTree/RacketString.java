package RacketTree;

import java.io.IOException;
import java.io.PushbackReader;

public class RacketString implements RacketAtom {
    private String str;
    public RacketString(PushbackReader fin, int opener)
    throws InvalidFormatException, IOException {
        StringBuilder val = new StringBuilder();
        for (int lastChar = fin.read(); lastChar != opener; lastChar = fin.read()) {
            if (lastChar == -1) {
                throw new InvalidFormatException("Error: Randomly Terminated File");
            }
            val.append(lastChar);
        }
        this.str = val.toString();
    }

    public boolean equals(RacketAtom other) {
        return false;
    }
}
