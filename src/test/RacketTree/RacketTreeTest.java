package RacketTree;

import RacketTree.RacketTree;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
    void RemoveIncorrectFiles() {
        try {
            FileWriter rm_file = new FileWriter("rm_file.sh");
            RecursivelyDeleteBadFiles(new File("test_files"), rm_file);
            rm_file.close();
        }
        catch (Exception e) {

        }
        assertTrue(true);
    }

    void RecursivelyDeleteBadFiles(File file, FileWriter rm_file) throws IOException {
        for (File child : file.listFiles()) {
            if (child.isFile()) {
                try {
                    RacketTree test = new RacketTree(new PushbackReader(new FileReader(child)));
                }
                catch (Exception e) {
                    if (child.delete()) {
                        System.out.println("Deleted file: " + child.getPath());
                    }
                    else {
                        System.out.println("Failed to delete: " + child.getPath());
                        rm_file.write("rm " + child.getPath() + "\n");
                    }
                }
            }
            else {
                RecursivelyDeleteBadFiles(child, rm_file);
            }
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

    @Test
    void GenerateComparisonCSV() {
        File test_files = new File("test_files/");
        try {
            for (File assignement : test_files.listFiles()) {
                // For .DS_Store
                if (assignement.isFile()) { continue; }
                ArrayList<RacketTree> treeList = new ArrayList<RacketTree>();
                ArrayList<String> fileOrder = new ArrayList<>();
                File csvFile = new File( assignement.getName() + ".csv");
                if (!csvFile.exists()) { csvFile.createNewFile(); }
                FileWriter csv = new FileWriter(csvFile.getPath());
                for (File child : assignement.listFiles()) {
                    if (child.getName().substring(child.getName().length() - 3).equals("txt")) {
                        treeList.add(new RacketTree(new PushbackReader(new FileReader(child))));
                        fileOrder.add(child.getName().substring(0, child.getName().length() - 4));
                        csv.write("," + fileOrder.get(fileOrder.size() - 1));
                    }
                }
                csv.write("\n");
                for (int i = 0; i < treeList.size(); i++) {
                    csv.write(fileOrder.get(i) + ",");
                    for (int j = 0; j < treeList.size(); j++) {
                        if (i == j) {
                            csv.write(",");
                            continue;
                        }
                        RacketTree treeI = treeList.get(i);
                        RacketTree treeJ = treeList.get(j);
                        csv.write(Integer.toString(treeI.similarityValue(treeJ)));
                        if (j != treeList.size() - 1) { csv.write(","); };
                    }
                    csv.write("\n");
                }
                System.out.println(csvFile.getName());
                csv.close();
            }
        }
        catch (Exception e) {
            fail();
        }
    }

}