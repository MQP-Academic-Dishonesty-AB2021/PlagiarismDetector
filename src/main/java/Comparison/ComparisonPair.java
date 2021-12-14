package Comparison;

public class ComparisonPair {
    private String file1;
    private String file2;

    public String getBaseFile() { return this.file1; }
    public String getComparedFile() { return this.file2; }

    public ComparisonPair(String file1, String file2) {
        this.file1 = file1;
        this.file2 = file2;
    }

    @Override
    public int hashCode() {
        // Multiplying by two to avoid collisions
        return file1.hashCode() + 2 * file2.hashCode();
    }

    public boolean equals(Object o) {
        if (o.getClass() != this.getClass()) { return false; }
        ComparisonPair other = (ComparisonPair) o;
        return (this.file1.equals(other.file1) && this.file2.equals(other.file2));
    }
}
