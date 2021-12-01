package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

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

    @Test
    void RunSimilarityForAll() {
        int maxValue = 0;
        HashMap<String, RacketTree> treeMap = new HashMap<String, RacketTree>();
        try {
            for (File fin : new File("test_files").listFiles()) {
                treeMap.put(fin.getName(), new RacketTree(new PushbackReader(new FileReader(fin))));
            }
        }
        catch (Exception e) {
            fail();
        }
        int outerCount = 0;
        for (Map.Entry<String, RacketTree> entry1 : treeMap.entrySet()) {
            for (Map.Entry<String, RacketTree> entry2 : treeMap.entrySet()) {
                if (entry1.getKey().equals(entry2.getKey())) { continue; }
                int val = entry1.getValue().similarityValue(entry2.getValue());
                if (val > maxValue) {
                    maxValue = val;
                    System.out.print("New Max: ");
                    System.out.print(Integer.toString(maxValue));
                    System.out.print(" " + entry1.getKey());
                    System.out.println(" " + entry2.getKey());
                }
            }
            outerCount++;
            System.out.println(Integer.toString(outerCount));
        }
    }

}