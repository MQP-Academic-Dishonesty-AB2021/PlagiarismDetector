package com.JavaFX;

import com.sun.javafx.stage.EmbeddedWindow;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class SetupController implements Initializable {



    @FXML
    private Pane dashPane;

    @FXML
    private Pane mainPane;



    FileChooser fileChooser1 = new FileChooser();
    FileChooser fileChooser2 = new FileChooser();

    private Stage stage;
    private Scene scene;
    private Parent root;



    //Opens dialog to select database of files to be cross referenced
    public void openFileDialogDatabase(ActionEvent event) throws IOException {
        File selectedFile = fileChooser1.showOpenDialog(((Node)event.getTarget()).getScene().getWindow());
    }

    //Opens dialog to select database of files to be tested
    public void openFileDialogTested(ActionEvent event) throws IOException {
        File selectedFile = fileChooser2.showOpenDialog(((Node)event.getTarget()).getScene().getWindow());
    }

    //returns to main menu
    public void returnToMainMenu(ActionEvent event) throws IOException {
        Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
        stage = (Stage)((Node)event.getSource()).getScene().getWindow();
        scene = new Scene(root);
        stage.setScene(scene);
    }

    //returns to main menu
    public void goToResults(ActionEvent event) throws IOException {
        Parent root = FXMLLoader.load(getClass().getResource("/partials/resultsPartial.fxml"));
        stage = (Stage)((Node)event.getSource()).getScene().getWindow();
        scene = new Scene(root);
        stage.setScene(scene);
    }

    @Override
    @FXML
    public void initialize(URL location, ResourceBundle resources) {



    }
}

