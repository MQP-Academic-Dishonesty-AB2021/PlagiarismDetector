package com.JavaFX;

import Comparison.Comparison;
import com.jfoenix.controls.JFXCheckBox;
import com.jfoenix.controls.JFXSlider;
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
import java.util.ArrayList;
import java.util.ResourceBundle;



public class SetupController implements Initializable {

	double x, y = 0;

	@FXML
	private Pane dashPane;

	@FXML
	private Pane mainPane;

	@FXML
	private ImageView exit;

	@FXML
	private JFXSlider leafSlider;

	@FXML
	private JFXSlider threadSlider;

	@FXML
	private JFXCheckBox useChecksims;


	DirectoryChooser directoryChooser1 = new DirectoryChooser();
	DirectoryChooser directoryChooser2 = new DirectoryChooser();

	private Stage stage;
	private Scene scene;
	private Parent root;

	public File databaseDirectory;
	public File testingDirectory;

	int cores;

	// Opens dialog to select database of files to be cross referenced
	public void openFileDialogDatabase(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		databaseDirectory = selectedFile;
	}

	// Opens dialog to select database of files to be tested
	public void openFileDialogTested(ActionEvent event) throws IOException {
		File selectedFile = directoryChooser1.showDialog(((Node) event.getTarget()).getScene().getWindow());
		testingDirectory = selectedFile;
	}

	// returns to main menu

	public void returnToMainMenu(ActionEvent event) throws IOException {
		Parent root = FXMLLoader.load(getClass().getResource("/menuBar.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);
	}

	@FXML
	private void sendData(ActionEvent event) {
		useChecksims.selectedProperty().set(false);
		Comparison results = new Comparison(testingDirectory.getAbsolutePath(),
				useChecksims.selectedProperty().getValue() ? Comparison.Method.Checksims : Comparison.Method.TreeSimilarity);
		// Step 2
		Node node = (Node) event.getSource();
		// Step 3
		Stage stage = (Stage) node.getScene().getWindow();
		stage.close();
		try {
			// Step 4
			Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
			// Step 5
			stage.setUserData(results);
			// Step 6
			Scene scene = new Scene(root);
			stage.setScene(scene);
			// Step 7
			stage.show();
		} catch (IOException e) {
			System.err.println(String.format("Error: %s", e.getMessage()));
		}
	}




	// returns to main menu
	public void goToResults(ActionEvent event) throws IOException {


		Parent root = FXMLLoader.load(getClass().getResource("/resultsPartial.fxml"));
		stage = (Stage) ((Node) event.getSource()).getScene().getWindow();
		scene = new Scene(root);
		stage.setScene(scene);





	}


	public void detectAvailableCores() throws IOException {
		int cores = Runtime.getRuntime().availableProcessors();
		threadSlider.setValue(cores);
	}

	//TODO make the slider change the cores from defaulting to max
	public void changeAvailableCores() throws IOException {
		threadSlider.setValue(cores);
	}

	@Override
	@FXML
	public void initialize(URL location, ResourceBundle resources) {
		try {
			detectAvailableCores();
		}
		catch(IOException e) {
			e.printStackTrace();
		}

	}
}
