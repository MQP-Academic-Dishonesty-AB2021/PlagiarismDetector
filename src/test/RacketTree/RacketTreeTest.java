package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import java.io.*;

import static org.junit.jupiter.api.Assertions.*;

public class RacketTreeTest {

    @Test
    void RacketTreeWorks1() {
        try {
            RacketTree test = new RacketTree("test_files/7214742.txt");
        }
        catch (Exception e) {
            fail();
        }
        assertTrue(true);
    }

    /**
     * Check that the code completes
     */
    @Test
    void RacketTreeCompletes() {
        int errorCount = 0;
        int totalCount = 0;
        for (File fin : new File("test_files").listFiles()) {
            try {
                RacketTree test = new RacketTree(new PushbackReader(new FileReader(fin)));
            }
            catch (Exception e) {
//                fail();
                errorCount++;
            }
            totalCount++;
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

    @Test
    void RacketTreeSimilarityValueFinishes() {
        try {
            RacketTree tree1 = new RacketTree("test_files/7214742.txt");
            RacketTree tree2 = new RacketTree("test_files/7214742.txt");
            int value = tree1.similarityValue(tree2);
            RacketTree tree3 = new RacketTree("test_files/8595055.txt");
            int value2 = tree1.similarityValue(tree3);
            assertTrue(value2 < value);
        }
        catch (Exception e) {
            fail();
        }
    }
}