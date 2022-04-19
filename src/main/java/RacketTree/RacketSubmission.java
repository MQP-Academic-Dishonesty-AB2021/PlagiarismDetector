package RacketTree;

import java.io.File;

public class RacketSubmission {
    private RacketAnonymizer parent;
    private String projectName;
    
    public RacketSubmission(RacketAnonymizer parent, String projectName) {
        this.parent = parent;
        this.projectName = projectName;
    }

    public String getName() {
        return this.projectName;
    }

    public File anonymized() {
        return parent.final_path.resolve("anonymized").resolve(projectName).toFile();
    }
    
    public File regular() {
        return parent.final_path.resolve("regular").resolve(projectName).toFile();
    }

    public boolean equals(Object o) {
        if (o.getClass() != this.getClass()) {
            return false;
        }
        RacketSubmission other = (RacketSubmission) o;
        return this.parent == other.parent &&
                this.projectName.equals(other.projectName);
    }

    public String toString() {
        return this.projectName;
    }
}
