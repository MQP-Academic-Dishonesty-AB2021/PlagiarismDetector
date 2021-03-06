package RacketTree;

import Comparison.Comparison;
import Comparison.ComparisonPromise;
import Comparison.ComparisonPair;
import javafx.beans.binding.Bindings;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import org.apache.commons.lang3.tuple.ImmutablePair;
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
				} catch (Exception e) {
					if (child.delete()) {
						System.out.println("Deleted file: " + child.getPath());
					} else {
						System.out.println("Failed to delete: " + child.getPath());
						rm_file.write("rm " + child.getPath() + "\n");
					}
				}
			} else {
				RecursivelyDeleteBadFiles(child, rm_file);
			}
		}
	}

	@Test
	void GenerateComparisonCSV() {
		File test_files = new File("test_files/");
		try {
			for (File assignment : test_files.listFiles()) {
				if (assignment.isFile()) {
					continue;
				}
				ComparisonPromise promise = new ComparisonPromise(assignment.getAbsolutePath(), Comparison.Method.TreeSimilarity);
				promise.start();
				Comparison submissions = promise.getComparison();
				System.out.println(assignment.getName());
				submissions.toCSV(assignment.getName());
			}
			assertTrue(true);
		} catch (Exception e) {
			fail();
		}
	}

	@Test
	void ChecksimsTest() {
		try {
			File test_files = new File("test_files/");
			for (File assignment : test_files.listFiles()) {
				if (assignment.isFile()) {
					continue;
				}
				ComparisonPromise promise = new ComparisonPromise(assignment.getAbsolutePath(), Comparison.Method.Checksims);
				promise.start();
				promise.join();
				Comparison submissions = promise.getComparison();
				System.out.println(assignment.getName());
				submissions.toCSV(assignment.getName() + "Checksims");
			}
		} catch (Exception e) {
			fail();
		}
	}

	@Test
	void TestOrderedListInRightOrder() throws InterruptedException {
		ComparisonPromise promise = new ComparisonPromise("test_files/assign2", Comparison.Method.TreeSimilarity);
		promise.start();
		Comparison test = promise.getComparison();
		ArrayList<ImmutablePair<ComparisonPair, Double>> list = test.getOrderedList();
		for (int i = 1; i < list.size(); i++) {
			assertTrue(list.get(i - 1).getValue() >= list.get(i).getValue());
		}
		assertTrue(true);
	}

	@Test
	void RacketAnonymizerTest() {
		File badFile = new File("./test_files/checksims_test");
		try {
			File goodFile = RacketAnonymizer.anonymizeFile(badFile);
		}
		catch (Exception e) {
			fail();
		}
		assertTrue(true);
	}

}
