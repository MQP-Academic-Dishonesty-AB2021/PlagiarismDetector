package com.JavaFX;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class SettingsController implements Initializable {

	double x, y = 0;

	@FXML
	private Pane dashPane;

	@FXML
	private Pane mainPane;

	@FXML
	private ImageView exit;

	DirectoryChooser directoryChooser1 = new DirectoryChooser();
	DirectoryChooser directoryChooser2 = new DirectoryChooser();

	private Stage stage;
	private Scene scene;
	private Parent root;

	public String parameter1;
	public String parameter2;



	// Opens dialog to select database of files to be cross referenced
	public void openFileDialogDatabase(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
	}

	// Opens dialog to select database of files to be tested
	public void openFileDialogTested(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
	}

	// returns to main menu
	public void returnToMainMenu(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	// returns to main menu
	public void goToResults(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/partials/resultsPartial.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {

	}
}
