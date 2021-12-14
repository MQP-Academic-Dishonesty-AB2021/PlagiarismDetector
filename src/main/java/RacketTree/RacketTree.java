package RacketTree;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.stream.Stream;

public class RacketTree {
	static public String function_open = "([{";
	static public String function_close = ")]}";
	static public String string_chars = "\"";
	static public String comment_chars = ";";
	static public String hash_delimeter = "#";
	static public String symbol_delimeter = "'";
	static public String lambda_chars = "λ";

	ArrayList<RacketExpression> children;
	public HashMap<RacketExpression, ArrayList<RacketExpression>> leavesHash;
	private int leafDepth;
	public int numLeaves;
	public static int defaultLeafDepth = 3;

	/**
	 * Construct a racket tree from a filename
	 * If it is a directory it recursively combines all rkt files inside
	 * 
	 * @param filename the filename to use
	 * @throws java.io.IOException    There was an error opening the file
	 * @throws InvalidFormatException There was an error parsing the file into a
	 *                                tree
	 */
	public RacketTree(String filename) throws java.io.IOException,
			InvalidFormatException {
		// TODO: Check if buffered reader is actually better for efficiency
		File fin = new File(filename);
		if (fin.isFile()) {
			this.children = this.generateChildren(new PushbackReader(new FileReader(fin)));
			this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
			return;
		} else if (fin.isDirectory()) {
			Stream<Path> files = Files.find(Paths.get(fin.getAbsolutePath()), 999,
					(p, bfa) -> p.getFileName().toString().matches(".*\\.rkt"));
			this.children = new ArrayList<RacketExpression>();
			files.forEach((file) -> {
				try {
					this.children.addAll(this.generateChildren(new PushbackReader(new FileReader(file.toFile()))));
				} catch (IOException | InvalidFormatException e) {
					e.printStackTrace();
				}
			});
		}
		this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
	}

	/**
	 * Construct a Racket Tree from a file
	 * 
	 * @param fin the input file
	 * @throws java.io.IOException    There was an error reading the file
	 * @throws InvalidFormatException The file was not able to be parsed into a
	 *                                Racket Tree
	 */
	public RacketTree(PushbackReader fin) throws java.io.IOException, InvalidFormatException {
		this.children = generateChildren(fin);
		this.leavesHash = this.generateLeavesHash(defaultLeafDepth);
	}

	/**
	 * Generates the children (root nodes) of the tree
	 * 
	 * @param fin input file
	 * @return the list containing the children
	 * @throws java.io.IOException    Error reading file
	 * @throws InvalidFormatException The file could not be parsed into valid racket
	 *                                code
	 */
	private ArrayList<RacketExpression> generateChildren(PushbackReader fin)
			throws java.io.IOException, InvalidFormatException {
		ArrayList<RacketExpression> children = new ArrayList<RacketExpression>();
		// iterate through file character by character
		// Theres just too many token exceptions to do anything but that
		for (int lastChar = fin.read(); lastChar != -1; lastChar = fin.read()) {
			if (!Character.isWhitespace((char) lastChar)) {
				fin.unread(lastChar);
				children.add(RacketExpression.generateAtom(fin, null));
			}
		}
		return children;
	}

	/**
	 * Generate leaves hash with current leaf depth
	 * 
	 * @return the leaves hash
	 */
	private HashMap<RacketExpression, ArrayList<RacketExpression>> generateLeavesHash() {
		return this.generateLeavesHash(this.leafDepth);
	}

	/**
	 * Generate leaves hash with a given depth for each leaf
	 * 
	 * @param height The maximum height of a leaf
	 * @return The leaves hash
	 */
	private HashMap<RacketExpression, ArrayList<RacketExpression>> generateLeavesHash(int height) {
		HashMap<RacketExpression, ArrayList<RacketExpression>> map = new HashMap<>();
		int sum = 0;
		for (RacketExpression tree : this.children) {
			sum += tree.insertIntoTreeMap(map, height);
		}
		this.numLeaves = sum;
		return map;
	}

	/**
	 * Return the similarity value from a leaves map
	 * Equivalent to the sum of the similarity value of its children, normalized for
	 * the number of leaves in the tree
	 * 
	 * @param leavesMap The leaves hash
	 * @return The sum of similarity values of its children normalized for the
	 *         number of leaves it has
	 */
	private double similarityValue(HashMap<RacketExpression, ArrayList<RacketExpression>> leavesMap) {
		int sum = 0;
		for (RacketExpression child : this.children) {
			sum += child.similarityValue(leavesMap);
		}
		return (double) sum / this.numLeaves;
	}

	/**
	 * Return the similarity value to another tree
	 * Equivalent to the sum of the similarity value of its children, normalized for
	 * the number of leaves in the tree
	 * 
	 * @param other The other RacketTree
	 * @return The sum of similarity values of its children normalized for the
	 *         number of leaves it has
	 */
	public double similarityValue(RacketTree other) {
		return other.similarityValue(this.leavesHash);
	}

	/**
	 * Setter for leaf depth attribute, using a setter because if it changes we have
	 * to recalculate the hash, and if it
	 * doesn't we dont want to
	 * 
	 * @param leafDepth The new leaf depth
	 */
	public void setLeafDepth(int leafDepth) {
		if (this.leafDepth == leafDepth) {
			return;
		}
		this.leafDepth = leafDepth;
		this.leavesHash = this.generateLeavesHash();
	}

	/**
	 * The size of children it has
	 * 
	 * @return the number of children
	 */
	public int size() {
		return children.size();
	}

}
