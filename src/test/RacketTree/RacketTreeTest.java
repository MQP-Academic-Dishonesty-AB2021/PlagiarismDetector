package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import java.io.*;

import static org.junit.jupiter.api.Assertions.*;

public class RacketTreeTest {

    /**
     * Check that the code completes
     */
    @Test
    void RacketTreeCompletes() {
        for (File fin : new File("test_files").listFiles()) {
            try {
                RacketTree test = new RacketTree(new PushbackReader(new FileReader(fin)));
            }
            catch (Exception e) {
                fail();
            }
        }
        assertTrue(true);
    }

    /**
     * Check to see if it finds the proper amount of elements in the tree
     */
    @Test
    void RacketTreeProperNum1() {
        try {
            RacketTree test = new RacketTree("test_files/1153273.txt");
            assertEquals(test.size(), 55);
        }
        catch (Exception e) {
            fail();
        }
    }
}