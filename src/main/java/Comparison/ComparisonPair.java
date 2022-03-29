package Comparison;

public class ComparisonPair {
	private String baseFile;
	private String comparedFile;

	public String getBaseFile() {
		return this.baseFile;
	}

	public String getComparedFile() {
		return this.comparedFile;
	}

	public ComparisonPair(String baseFile, String comparedFile) {
		this.baseFile = baseFile;
		this.comparedFile = comparedFile;
	}

	@Override
	public int hashCode() {
		// Multiplying by two to avoid collisions
		return baseFile.hashCode() + 2 * comparedFile.hashCode();
	}

	public boolean equals(Object o) {
		if (o.getClass() != this.getClass()) {
			return false;
		}
		ComparisonPair other = (ComparisonPair) o;
		return (this.baseFile.equals(other.baseFile) && this.comparedFile.equals(other.comparedFile));
	}
}
