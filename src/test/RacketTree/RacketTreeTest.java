package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class RacketTreeTest {

    /**
     * Check that the code completes
     */
    @Test
    void RacketTreeCompletes() {
        try {
            RacketTree test = new RacketTree("test_files/1153273.txt");
            assertTrue(true);
        }
        catch (Exception e) {
            fail();
        }
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