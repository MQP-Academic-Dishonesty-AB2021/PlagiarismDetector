package RacketTree;

import ComparisonRunner.Comparison;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

public class RacketTreeTest {

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

    @Test
    void GenerateComparisonCSV() {
        File test_files = new File("test_files/");
        try {
            for (File assignment : test_files.listFiles()) {
                if (assignment.isFile()) { continue; }
                Comparison submissions = new Comparison(assignment.getPath());
                System.out.println(assignment.getName());
                submissions.toCSV(assignment.getName());
            }
        }
        catch (Exception e) {
            fail();
        }
    }

    @Test
    void ChecksimsTest() {
        try {
            File test_files = new File("test_files/");
            for (File assignment : test_files.listFiles()) {
                if (assignment.isFile()) { continue; }
                Comparison submissions = new Comparison(assignment.getPath(), "Checksims");
                System.out.println(assignment.getName());
                submissions.toCSV(assignment.getName() + "Checksims");
//                submissions.toCSV(assignment.getName());
//                String[] args = {"-s", assignment.getPath(), "-o", "csv", "-f", assignment.getName() + "Checksims"};
//                Checksims.ChecksimsCommandLine.runCLI(args);
            }
            assertTrue(true);
        }
        catch (Exception e) {
            fail();
        }
    }

}