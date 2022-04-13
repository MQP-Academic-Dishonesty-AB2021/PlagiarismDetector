package RacketTree;

import java.io.File;

public class RacketSubmission {
    private RacketAnonymizer parent;
    private String projectName;
    
    RacketSubmission(RacketAnonymizer parent, String projectName) {
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
}
