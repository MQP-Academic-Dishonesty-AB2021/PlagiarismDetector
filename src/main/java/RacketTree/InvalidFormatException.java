package RacketTree;

public class InvalidFormatException extends Exception {
	private String str;

	InvalidFormatException(String str) {
		this.str = str;
	}

	public String toString() {
		return this.str;
	}
}
