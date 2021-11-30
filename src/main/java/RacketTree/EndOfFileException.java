package RacketTree;

public class EndOfFileException extends InvalidFormatException {
    EndOfFileException() {
        super("Error: Random termination of file");
    }
}
