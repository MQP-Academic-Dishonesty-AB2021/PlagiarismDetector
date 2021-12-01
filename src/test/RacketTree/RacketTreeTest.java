package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.ArrayList;

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
        File bestFile1;
        File bestFile2;
        try {
            int outerFileCount = 0;
            for (File fin : new File("test_files").listFiles()) {
                RacketTree tree1 = new RacketTree(new PushbackReader(new FileReader(fin)));
//                int innerFileCount = 0;
                for (File fin2 : new File("test_files").listFiles()) {
                    if (fin.equals(fin2)) { continue; }
                    RacketTree tree2 = new RacketTree(new PushbackReader(new FileReader(fin2)));
                    int val = tree1.similarityValue(tree2);
                    if (val > maxValue) {
                        maxValue = val;
                        bestFile1 = fin;
                        bestFile2 = fin2;
                        System.out.println("New Max: " + Integer.toString(maxValue) + " " + fin.getName() + " " + fin2.getName());
                    }
//                    innerFileCount++;
//                    System.out.println("innerFileCount" + Integer.toString(innerFileCount));
                }
                outerFileCount++;
                System.out.println("outerFileCount" + Integer.toString(outerFileCount));
            }
        }
        catch (Exception e) {
            fail();
        }
    }

}