package Comparison;

import RacketTree.RacketSubmission;

public class ComparisonPair {
	private RacketSubmission baseFile;
	private RacketSubmission comparedFile;

	public RacketSubmission getBaseFile() {
		return this.baseFile;
	}

	public RacketSubmission getComparedFile() {
		return this.comparedFile;
	}
	public String getBaseFilename() {
		return this.baseFile.getName();
	}

	public String getComparedFilename() {
		return this.comparedFile.getName();
	}

	public ComparisonPair(RacketSubmission baseFile, RacketSubmission comparedFile) {
		this.baseFile = baseFile;
		this.comparedFile = comparedFile;
	}

	@Override
	public int hashCode() {
		// Multiplying by two to avoid collisions
		return baseFile.getName().hashCode() + 2 * comparedFile.getName().hashCode();
	}

	public boolean equals(Object o) {
		if (o.getClass() != this.getClass()) {
			return false;
		}
		ComparisonPair other = (ComparisonPair) o;
		return (this.baseFile.equals(other.baseFile) && this.comparedFile.equals(other.comparedFile));
	}
}
